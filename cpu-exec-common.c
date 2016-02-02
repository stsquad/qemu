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
#include "exec/memory-internal.h"

/* Number of safe work pending for all VCPUs. */
int safe_work_pending;

/* exit the current TB from a signal handler. The host registers are
   restored in a state compatible with the CPU emulator
 */
#if defined(CONFIG_SOFTMMU)
void cpu_resume_from_signal(CPUState *cpu, void *puc)
{
    /* XXX: restore cpu registers saved in host registers */

    cpu->exception_index = -1;
    siglongjmp(cpu->jmp_env, 1);
}

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
    cpu->current_tb = NULL;
    siglongjmp(cpu->jmp_env, 1);
}

void cpu_loop_exit_restore(CPUState *cpu, uintptr_t pc)
{
    if (pc) {
        cpu_restore_state(cpu, pc);
    }
    cpu->current_tb = NULL;
    siglongjmp(cpu->jmp_env, 1);
}

/* DUP: qemu_cpu_is_self from cpus.c */
static bool qemu_cpu_thread_is_self(CPUState *cpu)
{
    return qemu_thread_is_self(cpu->thread);
}

void async_run_on_cpu(CPUState *cpu, void (*func)(void *data), void *data)
{
    struct qemu_work_item *wi;

    if (qemu_cpu_thread_is_self(cpu)) {
        func(data);
        return;
    }

    wi = g_malloc0(sizeof(struct qemu_work_item));
    wi->func = func;
    wi->data = data;
    wi->free = true;

    qemu_mutex_lock(&cpu->work_mutex);
    if (cpu->queued_work_first == NULL) {
        cpu->queued_work_first = wi;
    } else {
        cpu->queued_work_last->next = wi;
    }
    cpu->queued_work_last = wi;
    wi->next = NULL;
    wi->done = false;
    qemu_mutex_unlock(&cpu->work_mutex);

    /* FIXME: what to do in the user case? */
#ifdef CONFIG_SOFTMMU
    qemu_cpu_kick(cpu);
#endif
}

void async_run_safe_work_on_cpu(CPUState *cpu, void (*func)(void *data),
                                void *data)
{
    struct qemu_work_item *wi;

    wi = g_malloc0(sizeof(struct qemu_work_item));
    wi->func = func;
    wi->data = data;
    wi->free = true;

    atomic_inc(&safe_work_pending);
    qemu_mutex_lock(&cpu->work_mutex);
    if (cpu->queued_safe_work_first == NULL) {
        cpu->queued_safe_work_first = wi;
    } else {
        cpu->queued_safe_work_last->next = wi;
    }
    cpu->queued_safe_work_last = wi;
    wi->next = NULL;
    wi->done = false;
    qemu_mutex_unlock(&cpu->work_mutex);
}

/* FIXME: systememu does this from waitio, when would linux-user do
 * this? */
void flush_queued_safe_work(CPUState *cpu)
{
    struct qemu_work_item *wi;
    CPUState *other_cpu;

    if (cpu->queued_safe_work_first == NULL) {
        return;
    }

    CPU_FOREACH(other_cpu) {
        if (!tcg_cpu_try_block_execution(other_cpu)) {
            return;
        }
    }

    qemu_mutex_lock(&cpu->work_mutex);
    while ((wi = cpu->queued_safe_work_first)) {
        cpu->queued_safe_work_first = wi->next;
        qemu_mutex_unlock(&cpu->work_mutex);
        wi->func(wi->data);
        qemu_mutex_lock(&cpu->work_mutex);
        wi->done = true;
        if (wi->free) {
            g_free(wi);
        }
        atomic_dec(&safe_work_pending);
    }
    cpu->queued_safe_work_last = NULL;
    qemu_mutex_unlock(&cpu->work_mutex);

    /* FIXME: is this only relevant for softmmu?
       qemu_cond_broadcast(&qemu_work_cond);
    */
}

bool async_safe_work_pending(void)
{
    return safe_work_pending != 0;
}
