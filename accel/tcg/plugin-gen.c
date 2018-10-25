#include "qemu/osdep.h"
#include "cpu.h"
#include "tcg/tcg.h"
#include "tcg/tcg-op.h"
#include "exec/exec-all.h"
#include "exec/plugin-gen.h"

static void gen_inline_op(struct qemu_plugin_dyn_cb *cb)
{
    TCGv_i64 val = tcg_temp_new_i64();
    TCGv_ptr ptr = tcg_const_ptr(cb->userp);

    tcg_gen_ld_i64(val, ptr, 0);
    switch (cb->inline_insn.op) {
    case QEMU_PLUGIN_INLINE_ADD_U64:
        tcg_gen_addi_i64(val, val, cb->inline_insn.imm);
        break;
    default:
        g_assert_not_reached();
    }
    tcg_gen_st_i64(val, ptr, 0);

    tcg_temp_free_ptr(ptr);
    tcg_temp_free_i64(val);
}

static void gen_helper_mem_cb(const char *name, unsigned flags,
                              qemu_plugin_vcpu_mem_cb_t cb, TCGv_i32 cpu_index,
                              TCGv_i32 meminfo, TCGv_i64 vaddr, TCGv_ptr udata)
{
    TCGHelperInfo info = {
        .func = cb,
        .name = name,
        .flags = flags,
        .sizemask = dh_sizemask(void, 0) |
                    dh_sizemask(i32, 1) |
                    dh_sizemask(i32, 2) |
                    dh_sizemask(i64, 3) |
                    dh_sizemask(ptr, 4),

    };
    TCGTemp *args[] = {
        tcgv_i32_temp(cpu_index),
        tcgv_i32_temp(meminfo),
        tcgv_i64_temp(vaddr),
        tcgv_ptr_temp(udata),
    };

    tcg_gen_runtime_helper(&info, NULL, ARRAY_SIZE(args), args);
}

static void gen_helper_mem_haddr_cb(const char *name, unsigned flags,
                                    qemu_plugin_vcpu_mem_haddr_cb_t cb,
                                    TCGv_i32 cpu_index, TCGv_i32 meminfo,
                                    TCGv_i64 vaddr, TCGv_ptr haddr,
                                    TCGv_ptr udata)
{
    TCGHelperInfo info = {
        .func = cb,
        .name = name,
        .flags = flags,
        .sizemask = dh_sizemask(void, 0) |
                    dh_sizemask(i32, 1) |
                    dh_sizemask(i32, 2) |
                    dh_sizemask(i64, 3) |
                    dh_sizemask(ptr, 4) |
                    dh_sizemask(ptr, 5),

    };
    TCGTemp *args[] = {
        tcgv_i32_temp(cpu_index),
        tcgv_i32_temp(meminfo),
        tcgv_i64_temp(vaddr),
        tcgv_ptr_temp(haddr),
        tcgv_ptr_temp(udata),
    };

    tcg_gen_runtime_helper(&info, NULL, ARRAY_SIZE(args), args);
}

static void gen_mem_cb(struct qemu_plugin_dyn_cb *cb, TCGv vaddr, uint8_t info)
{
    TCGv_i32 cpu_index = tcg_temp_new_i32();
    TCGv_i32 meminfo = tcg_const_i32(info);
    TCGv_i64 vaddr64 = tcg_temp_new_i64();
    TCGv_ptr udata = tcg_const_ptr(cb->userp);
    TCGv_ptr haddr;

    tcg_gen_ld_i32(cpu_index, cpu_env,
                   -ENV_OFFSET + offsetof(CPUState, cpu_index));
    tcg_gen_extu_tl_i64(vaddr64, vaddr);

    if (cb->mem.haddr) {
#ifdef CONFIG_SOFTMMU
        haddr = tcg_temp_new_ptr();
        tcg_gen_ld_ptr(haddr, cpu_env, offsetof(CPUArchState, hostaddr));
#else
        haddr = tcg_const_ptr(NULL);
#endif
        gen_helper_mem_haddr_cb("helper_plugin_vcpu_mem_haddr_cb",
                                cb->tcg_flags, cb->f.vcpu_mem_haddr,
                                cpu_index, meminfo, vaddr64, haddr, udata);
        tcg_temp_free_ptr(haddr);
    } else {
        gen_helper_mem_cb("helper_plugin_vcpu_mem_cb", cb->tcg_flags,
                          cb->f.vcpu_mem, cpu_index, meminfo, vaddr64,
                          udata);
    }

    tcg_temp_free_ptr(udata);
    tcg_temp_free_i64(vaddr64);
    tcg_temp_free_i32(meminfo);
    tcg_temp_free_i32(cpu_index);
}

void qemu_plugin_gen_vcpu_mem_callbacks(struct qemu_plugin_dyn_cb_arr *arr,
                                        TCGv vaddr, uint8_t info)
{
    size_t i;

    for (i = 0; i < arr->n; i++) {
        struct qemu_plugin_dyn_cb *cb = &arr->data[i];

        switch (cb->type) {
        case QEMU_PLUGIN_DYN_CB_TYPE_REGULAR:
            gen_mem_cb(cb, vaddr, info);
            break;
        case QEMU_PLUGIN_DYN_CB_TYPE_INLINE:
            gen_inline_op(cb);
            break;
        default:
            g_assert_not_reached();
        }
    }
}

static void gen_helper_vcpu_udata_cb(const char *name, unsigned flags,
                                     qemu_plugin_vcpu_udata_cb_t cb,
                                     TCGv_i32 cpu_index, TCGv_ptr udata)
{
    TCGHelperInfo info = {
        .func = cb,
        .name = name,
        .flags = flags,
        .sizemask = dh_sizemask(void, 0) |
                    dh_sizemask(i32, 1) |
                    dh_sizemask(ptr, 2),
    };
    TCGTemp *args[] = {
        tcgv_i32_temp(cpu_index),
        tcgv_ptr_temp(udata),
    };

    tcg_gen_runtime_helper(&info, NULL, ARRAY_SIZE(args), args);
}

static void gen_vcpu_udata_cb(struct qemu_plugin_dyn_cb *cb)
{
    TCGv_i32 cpu_index = tcg_temp_new_i32();
    TCGv_ptr udata = tcg_const_ptr(cb->userp);

    tcg_gen_ld_i32(cpu_index, cpu_env,
                   -ENV_OFFSET + offsetof(CPUState, cpu_index));

    gen_helper_vcpu_udata_cb("helper_plugin_vcpu_udata_cb", cb->tcg_flags,
                             cb->f.vcpu_udata, cpu_index, udata);

    tcg_temp_free_ptr(udata);
    tcg_temp_free_i32(cpu_index);
}

void qemu_plugin_gen_vcpu_udata_callbacks(struct qemu_plugin_dyn_cb_arr *arr)
{
    size_t i;

    for (i = 0; i < arr->n; i++) {
        struct qemu_plugin_dyn_cb *cb = &arr->data[i];

        switch (cb->type) {
        case QEMU_PLUGIN_DYN_CB_TYPE_REGULAR:
            gen_vcpu_udata_cb(cb);
            break;
        case QEMU_PLUGIN_DYN_CB_TYPE_INLINE:
            gen_inline_op(cb);
            break;
        default:
            g_assert_not_reached();
        }
    }
}

/*
 * Tracking memory accesses performed from helpers requires extra work.
 * If an instruction is emulated with helpers, struct qemu_plugin_insn's
 * .calls_helpers is set. If so, this function is called. Here we do two
 * things: (1) copy the CB descriptor, and keep track of it so that it can be
 * freed later on, and (2) point CPUState.plugin_mem_cbs to the descriptor, so
 * that we can read it at run-time (i.e. when the helper executes).
 * This run-time access is performed from qemu_plugin_vcpu_mem_cb.
 *
 * Note that qemu_plugin_gen_disable_mem_helpers undoes (2).
 */
void
qemu_plugin_gen_enable_mem_helpers(const struct qemu_plugin_dyn_cb_arr *orig)
{
    struct qemu_plugin_dyn_cb_arr *arr;
    TCGv_ptr ptr;

    arr = g_new(struct qemu_plugin_dyn_cb_arr, 1);
    arr->capacity = orig->n;
    arr->n = orig->n;
    arr->data = g_new(struct qemu_plugin_dyn_cb, arr->n);
    memcpy(arr->data, orig->data, sizeof(*arr->data) * arr->n);
    qemu_plugin_add_dyn_cb_arr(arr);

    ptr = tcg_const_ptr(arr);
    tcg_gen_st_ptr(ptr, cpu_env, -ENV_OFFSET + offsetof(CPUState,
                                                        plugin_mem_cbs));
    tcg_temp_free_ptr(ptr);
}

/* Called once we're done instrumenting an instruction that calls helpers */
void qemu_plugin_gen_disable_mem_helpers(void)
{
    TCGv_ptr ptr = tcg_const_ptr(NULL);

    tcg_gen_st_ptr(ptr, cpu_env, -ENV_OFFSET + offsetof(CPUState,
                                                        plugin_mem_cbs));
    tcg_temp_free_ptr(ptr);
}
