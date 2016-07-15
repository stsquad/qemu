/*
 *  emulator main execution loop
 *
 *  Copyright (c) 2003-2005 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "cpu.h"
#include "sysemu/cpus.h"
#include "exec/exec-all.h"
#include "exec/memory-internal.h"

bool exit_request;
CPUState *tcg_current_cpu;
int tcg_pending_threads;

/* exit the current TB, but without causing any exception to be raised */
void cpu_loop_exit_noexc(CPUState *cpu)
{
    /* XXX: restore cpu registers saved in host registers */

    cpu->exception_index = -1;
    siglongjmp(cpu->jmp_env, 1);
}

#if defined(CONFIG_SOFTMMU)
void cpu_reloading_memory_map(void)
{
    if (qemu_in_vcpu_thread()) {
        /* The guest can in theory prolong the RCU critical section as long
         * as it feels like. The major problem with this is that because it
         * can do multiple reconfigurations of the memory map within the
         * critical section, we could potentially accumulate an unbounded
         * collection of memory data structures awaiting reclamation.
         *
         * Because the only thing we're currently protecting with RCU is the
         * memory data structures, it's sufficient to break the critical section
         * in this callback, which we know will get called every time the
         * memory map is rearranged.
         *
         * (If we add anything else in the system that uses RCU to protect
         * its data structures, we will need to implement some other mechanism
         * to force TCG CPUs to exit the critical section, at which point this
         * part of this callback might become unnecessary.)
         *
         * This pair matches cpu_exec's rcu_read_lock()/rcu_read_unlock(), which
         * only protects cpu->as->dispatch. Since we know our caller is about
         * to reload it, it's safe to split the critical section.
         */
        rcu_read_unlock();
        rcu_read_lock();
    }
}
#endif

void cpu_loop_exit(CPUState *cpu)
{
    siglongjmp(cpu->jmp_env, 1);
}

void cpu_loop_exit_restore(CPUState *cpu, uintptr_t pc)
{
    if (pc) {
        cpu_restore_state(cpu, pc);
    }
    siglongjmp(cpu->jmp_env, 1);
}

QemuCond qemu_work_cond;
QemuCond qemu_safe_work_cond;
QemuCond qemu_exclusive_cond;

static int safe_work_pending;

#ifdef CONFIG_USER_ONLY
#define can_wait_for_safe() (1)
#else
/*
 * We never sleep in SoftMMU emulation because we would deadlock as
 * all vCPUs are in the same thread. This will change for MTTCG
 * however.
 */
#define can_wait_for_safe() (0)
#endif

void wait_safe_cpu_work(void)
{
    while (can_wait_for_safe() && atomic_mb_read(&safe_work_pending) > 0) {
        /*
         * If there is pending safe work and no pending threads we
         * need to signal another thread to start its work.
         */
        if (tcg_pending_threads == 0) {
            qemu_cond_signal(&qemu_exclusive_cond);
        }
        qemu_cond_wait(&qemu_safe_work_cond, qemu_get_cpu_work_mutex());
    }
}

static void queue_work_on_cpu(CPUState *cpu, struct qemu_work_item *wi)
{
    qemu_mutex_lock(&cpu->work_mutex);
    if (cpu->queued_work_first == NULL) {
        cpu->queued_work_first = wi;
    } else {
        cpu->queued_work_last->next = wi;
    }
    cpu->queued_work_last = wi;
    wi->next = NULL;
    wi->done = false;
    if (wi->safe) {
        atomic_inc(&safe_work_pending);
    }
    qemu_mutex_unlock(&cpu->work_mutex);

    if (!wi->safe) {
        qemu_cpu_kick(cpu);
    } else {
        CPU_FOREACH(cpu) {
            qemu_cpu_kick(cpu);
        }
    }
}

void run_on_cpu(CPUState *cpu, run_on_cpu_func func, void *data)
{
    struct qemu_work_item wi;

    if (qemu_cpu_is_self(cpu)) {
        func(cpu, data);
        return;
    }

    wi.func = func;
    wi.data = data;
    wi.free = false;
    wi.safe = false;

    queue_work_on_cpu(cpu, &wi);
    while (!atomic_mb_read(&wi.done)) {
        CPUState *self_cpu = current_cpu;

        qemu_cond_wait(&qemu_work_cond, qemu_get_cpu_work_mutex());
        current_cpu = self_cpu;
    }
}

void async_run_on_cpu(CPUState *cpu, run_on_cpu_func func, void *data)
{
    struct qemu_work_item *wi;

    if (qemu_cpu_is_self(cpu)) {
        func(cpu, data);
        return;
    }

    wi = g_malloc0(sizeof(struct qemu_work_item));
    wi->func = func;
    wi->data = data;
    wi->free = true;
    wi->safe = false;

    queue_work_on_cpu(cpu, wi);
}

void async_safe_run_on_cpu(CPUState *cpu, run_on_cpu_func func, void *data)
{
    struct qemu_work_item *wi;

    wi = g_malloc0(sizeof(struct qemu_work_item));
    wi->func = func;
    wi->data = data;
    wi->free = true;
    wi->safe = true;

    queue_work_on_cpu(cpu, wi);
}

void process_queued_cpu_work(CPUState *cpu)
{
    struct qemu_work_item *wi;

    if (cpu->queued_work_first == NULL) {
        return;
    }

    qemu_mutex_lock(&cpu->work_mutex);
    while (cpu->queued_work_first != NULL) {
        wi = cpu->queued_work_first;
        cpu->queued_work_first = wi->next;
        if (!cpu->queued_work_first) {
            cpu->queued_work_last = NULL;
        }
        if (wi->safe) {
            while (tcg_pending_threads) {
                qemu_cond_wait(&qemu_exclusive_cond,
                               qemu_get_cpu_work_mutex());
            }
        }
        qemu_mutex_unlock(&cpu->work_mutex);
        wi->func(cpu, wi->data);
        qemu_mutex_lock(&cpu->work_mutex);
        if (wi->safe) {
            if (!atomic_dec_fetch(&safe_work_pending)) {
                qemu_cond_broadcast(&qemu_safe_work_cond);
            }
        }
        if (wi->free) {
            g_free(wi);
        } else {
            atomic_mb_set(&wi->done, true);
        }
    }
    qemu_mutex_unlock(&cpu->work_mutex);
    qemu_cond_broadcast(&qemu_work_cond);
}
