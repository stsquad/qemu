/*
 * CPU thread main loop - common bits for user and system mode emulation
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
#include "qemu/main-loop.h"
#include "exec/cpu-common.h"
#include "qom/cpu.h"
#include "sysemu/cpus.h"

static QemuMutex qemu_cpu_list_lock;
static QemuCond exclusive_resume;

/* >= 1 if a thread is inside start_exclusive/end_exclusive.  Written
 * under qemu_cpu_list_lock, read with atomic operations.
 */
static int pending_cpus;
static bool exclusive_work_ongoing;

void qemu_init_cpu_list(void)
{
    /* This is needed because qemu_init_cpu_list is also called by the
     * child process in a fork.  */
    pending_cpus = 0;
    exclusive_work_ongoing = false;

    qemu_mutex_init(&qemu_cpu_list_lock);
    qemu_cond_init(&exclusive_resume);
}

void cpu_list_lock(void)
{
    qemu_mutex_lock(&qemu_cpu_list_lock);
}

void cpu_list_unlock(void)
{
    qemu_mutex_unlock(&qemu_cpu_list_lock);
}

static bool cpu_index_auto_assigned;

static int cpu_get_free_index(void)
{
    CPUState *some_cpu;
    int cpu_index = 0;

    cpu_index_auto_assigned = true;
    CPU_FOREACH(some_cpu) {
        cpu_index++;
    }
    return cpu_index;
}

static void finish_safe_work(CPUState *cpu)
{
    cpu_exec_start(cpu);
    cpu_exec_end(cpu);
}

/* Wait for pending exclusive operations to complete.  The CPU list lock
   must be held.  */
static inline void exclusive_idle(void)
{
    while (exclusive_work_ongoing) {
        qemu_cond_wait(&exclusive_resume, &qemu_cpu_list_lock);
    }
}

void cpu_list_add(CPUState *cpu)
{
    qemu_mutex_lock(&qemu_cpu_list_lock);
    if (cpu->cpu_index == UNASSIGNED_CPU_INDEX) {
        cpu->cpu_index = cpu_get_free_index();
        assert(cpu->cpu_index != UNASSIGNED_CPU_INDEX);
    } else {
        assert(!cpu_index_auto_assigned);
    }

    /* make sure no exclusive jobs are running before touching the list */
    exclusive_idle();

    QTAILQ_INSERT_TAIL_RCU(&cpus, cpu, node);
    qemu_mutex_unlock(&qemu_cpu_list_lock);

    finish_safe_work(cpu);
}

void cpu_list_remove(CPUState *cpu)
{
    qemu_mutex_lock(&qemu_cpu_list_lock);
    if (!QTAILQ_IN_USE(cpu, node)) {
        /* there is nothing to undo since cpu_exec_init() hasn't been called */
        qemu_mutex_unlock(&qemu_cpu_list_lock);
        return;
    }

    assert(!(cpu_index_auto_assigned && cpu != QTAILQ_LAST(&cpus, CPUTailQ)));

    /* make sure no exclusive jobs are running before touching the list */
    exclusive_idle();

    QTAILQ_REMOVE_RCU(&cpus, cpu, node);
    cpu->cpu_index = UNASSIGNED_CPU_INDEX;
    qemu_mutex_unlock(&qemu_cpu_list_lock);
}

struct qemu_work_item {
    QSIMPLEQ_ENTRY(qemu_work_item) node;
    run_on_cpu_func func;
    run_on_cpu_data data;
    bool free, exclusive, done;
    bool bql;
};

/* Called with the CPU's lock held */
static void queue_work_on_cpu_locked(CPUState *cpu, struct qemu_work_item *wi)
{
    QSIMPLEQ_INSERT_TAIL(&cpu->work_list, wi, node);
    wi->done = false;

    qemu_cpu_kick(cpu);
}

static void queue_work_on_cpu(CPUState *cpu, struct qemu_work_item *wi)
{
    cpu_mutex_lock(cpu);
    queue_work_on_cpu_locked(cpu, wi);
    cpu_mutex_unlock(cpu);
}

void run_on_cpu(CPUState *cpu, run_on_cpu_func func, run_on_cpu_data data)
{
    struct qemu_work_item wi;
    bool has_bql = qemu_mutex_iothread_locked();

    g_assert(no_cpu_mutex_locked());

    if (qemu_cpu_is_self(cpu)) {
        if (has_bql) {
            func(cpu, data);
        } else {
            qemu_mutex_lock_iothread();
            func(cpu, data);
            qemu_mutex_unlock_iothread();
        }
        return;
    }

    /* We are going to sleep on the CPU lock, so release the BQL */
    if (has_bql) {
        qemu_mutex_unlock_iothread();
    }

    wi.func = func;
    wi.data = data;
    wi.done = false;
    wi.free = false;
    wi.exclusive = false;
    wi.bql = true;

    cpu_mutex_lock(cpu);
    queue_work_on_cpu_locked(cpu, &wi);
    while (!atomic_mb_read(&wi.done)) {
        CPUState *self_cpu = current_cpu;

        qemu_cond_wait(&cpu->cond, &cpu->lock);
        current_cpu = self_cpu;
    }
    cpu_mutex_unlock(cpu);

    if (has_bql) {
        qemu_mutex_lock_iothread();
    }
}

void async_run_on_cpu(CPUState *cpu, run_on_cpu_func func, run_on_cpu_data data)
{
    struct qemu_work_item *wi;

    wi = g_malloc0(sizeof(struct qemu_work_item));
    wi->func = func;
    wi->data = data;
    wi->free = true;
    wi->bql = true;

    queue_work_on_cpu(cpu, wi);
}

void async_run_on_cpu_no_bql(CPUState *cpu, run_on_cpu_func func,
                             run_on_cpu_data data)
{
    struct qemu_work_item *wi;

    wi = g_malloc0(sizeof(struct qemu_work_item));
    wi->func = func;
    wi->data = data;
    wi->free = true;
    /* wi->bql initialized to false */

    queue_work_on_cpu(cpu, wi);
}

/* Start an exclusive operation.
   Must only be called from outside cpu_exec.  */
void start_exclusive(void)
{
    CPUState *other_cpu;

    qemu_mutex_lock(&qemu_cpu_list_lock);
    exclusive_idle();
    exclusive_work_ongoing = true;
    qemu_mutex_unlock(&qemu_cpu_list_lock);

    /* Make all other cpus stop executing.  */
    CPU_FOREACH(other_cpu) {
        cpu_mutex_lock(other_cpu);
        if (other_cpu->running) {
            g_assert(!other_cpu->exclusive_waiter);
            other_cpu->exclusive_waiter = true;
            qemu_cpu_kick(other_cpu);
        }
        other_cpu->exclusive_ongoing = true;
        cpu_mutex_unlock(other_cpu);
    }

    /* wait for CPUs that were running to clear us */
    CPU_FOREACH(other_cpu) {
        cpu_mutex_lock(other_cpu);
        while (other_cpu->exclusive_waiter) {
            qemu_cond_wait(&other_cpu->cond, &other_cpu->lock);
        }
        cpu_mutex_unlock(other_cpu);
    }
}

/* Finish an exclusive operation.  */
void end_exclusive(void)
{
    CPUState *other_cpu;

    CPU_FOREACH(other_cpu) {
        cpu_mutex_lock(other_cpu);
        g_assert(!other_cpu->exclusive_waiter);
        g_assert(other_cpu->exclusive_ongoing);
        other_cpu->exclusive_ongoing = false;
        qemu_cond_signal(&other_cpu->cond);
        cpu_mutex_unlock(other_cpu);
    }

    qemu_mutex_lock(&qemu_cpu_list_lock);
    exclusive_work_ongoing = false;
    qemu_cond_broadcast(&exclusive_resume);
    qemu_mutex_unlock(&qemu_cpu_list_lock);
}

static void cpu_exec_exclusive_locked(CPUState *cpu)
{
    g_assert(cpu_mutex_locked(cpu));

    if (cpu->exclusive_waiter) {
        cpu->exclusive_waiter = false;
        qemu_cond_signal(&cpu->cond);
    }
    while (cpu->exclusive_ongoing) {
        qemu_cond_wait(&cpu->cond, &cpu->lock);
    }
}

/* Wait for exclusive ops to finish, and begin cpu execution.  */
void cpu_exec_start(CPUState *cpu)
{
    cpu_mutex_lock(cpu);
    cpu_exec_exclusive_locked(cpu);
    cpu->running = true;
    cpu_mutex_unlock(cpu);
}

/* Mark cpu as not executing, and release pending exclusive ops.  */
void cpu_exec_end(CPUState *cpu)
{
    cpu_mutex_lock(cpu);
    cpu->running = false;
    cpu_exec_exclusive_locked(cpu);
    cpu_mutex_unlock(cpu);
}

void async_safe_run_on_cpu(CPUState *cpu, run_on_cpu_func func,
                           run_on_cpu_data data)
{
    struct qemu_work_item *wi;

    wi = g_malloc0(sizeof(struct qemu_work_item));
    wi->func = func;
    wi->data = data;
    wi->free = true;
    wi->exclusive = true;
    /* wi->bql initialized to false */

    queue_work_on_cpu(cpu, wi);
}

/* Called with the CPU's lock held */
static void process_queued_cpu_work_locked(CPUState *cpu)
{
    struct qemu_work_item *wi;
    bool has_bql = qemu_mutex_iothread_locked();

    if (QSIMPLEQ_EMPTY(&cpu->work_list)) {
        return;
    }
    while (!QSIMPLEQ_EMPTY(&cpu->work_list)) {
        wi = QSIMPLEQ_FIRST(&cpu->work_list);
        QSIMPLEQ_REMOVE_HEAD(&cpu->work_list, node);
        cpu_mutex_unlock(cpu);
        if (wi->exclusive) {
            /* Running work items outside the BQL avoids the following deadlock:
             * 1) start_exclusive() is called with the BQL taken while another
             * CPU is running; 2) cpu_exec in the other CPU tries to takes the
             * BQL, so it goes to sleep; start_exclusive() is sleeping too, so
             * neither CPU can proceed.
             */
            g_assert(!wi->bql);
            if (has_bql) {
                qemu_mutex_unlock_iothread();
            }
            start_exclusive();
            wi->func(cpu, wi->data);
            end_exclusive();
            if (has_bql) {
                qemu_mutex_lock_iothread();
            }
        } else {
            if (wi->bql) {
                if (has_bql) {
                    wi->func(cpu, wi->data);
                } else {
                    qemu_mutex_lock_iothread();
                    wi->func(cpu, wi->data);
                    qemu_mutex_unlock_iothread();
                }
            } else {
                if (has_bql) {
                    qemu_mutex_unlock_iothread();
                    wi->func(cpu, wi->data);
                    qemu_mutex_lock_iothread();
                } else {
                    wi->func(cpu, wi->data);
                }
            }
        }
        cpu_mutex_lock(cpu);
        if (wi->free) {
            g_free(wi);
        } else {
            atomic_mb_set(&wi->done, true);
        }
    }
    qemu_cond_broadcast(&cpu->cond);
}

void process_queued_cpu_work(CPUState *cpu)
{
    cpu_mutex_lock(cpu);
    process_queued_cpu_work_locked(cpu);
    cpu_mutex_unlock(cpu);
}
