/*
 * Registers API
 *
 * Copyright (c) 2022 Linaro Ltd
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

/**
 * reg_add_env: add register stored in per-vCPU env
 * @name: register name
 * @group: if !NULL name of register group
 * @offset: offset into the cpu_env structure
 * @bytes: size in bytes (? use MEMOP ?)
 *
 * Register a normal global register stored in the cpu_env structure.
 * The register API will access it's value directly when needed. The
 * order of registration matters within each group.
 */
void reg_add_env(const char *name, const char *group, intptr_t offset, int bytes);

/*
 * function pointer prototypes for helper functions
 */
typedef uint64_t reg_read64_fn(CPUState *cs, const void *opaque);
typedef void reg_write64_fn(CPUState *cs, const void *opaque, uint64_t value);
typedef void reg_fmt64_fn(GString *s, CPUState *cs, const void *opaque);

/**
 * reg_add_i64_virt: add a virtual 64 bit register
 * @name: register name
 * @group: name of register group
 * @opaque: opaque ptr for helper
 * @rfn: read function
 * @wfn: optional write function
 * @fmt: optional format function
 *
 * A virtual register is one that isn't stored directly but must rely
 * on helpers from the front end to access their values. We also
 * include an optional format function.
 */
void reg_add_i64_virt(const char *name, const char *group, void *opaque,
                      reg_read64_fn rfn, reg_write64_fn wfn, reg_fmt64_fn ffn);

/*
 * function pointer prototypes for helper functions
 */
typedef uint32_t reg_read32_fn(CPUState *cs, const void *opaque);
typedef void reg_write32_fn(CPUState *cs, const void *opaque, uint32_t value);
typedef void reg_fmt32_fn(GString *s, CPUState *cs, const void *opaque);

/**
 * reg_add_i32_virt: add a virtual 32 bit register
 * @name: register name
 * @grp: name of register group
 * @opaque: opaque ptr for helper
 * @rfn: read function
 * @wfn: optional write function
 * @fmt: optional format function
 *
 * A virtual register is one that isn't stored directly but must rely
 * on helpers from the front end to access their values. We also
 * include an optional format function.
 */
void reg_add_i32_virt(const char *name, const char *grp, void *opaque,
                      reg_read32_fn rfn, reg_write32_fn wfn, reg_fmt32_fn ffn);

/*
 * Bitmask definitions for supported widths
 */
typedef enum {
    REG_VEC_UINT8      = 1 << 0,
    REG_VEC_INT8       = 1 << 1,
    REG_VEC_ALL8       = REG_VEC_UINT8 | REG_VEC_INT8,

    REG_VEC_UINT16     = 1 << 2,
    REG_VEC_INT16      = 1 << 3,
    REG_VEC_IEEEHALF   = 1 << 4,
    REG_VEC_ALL16      = REG_VEC_UINT16 | REG_VEC_INT16 | REG_VEC_IEEEHALF,

    REG_VEC_UINT32     = 1 << 5,
    REG_VEC_INT32      = 1 << 6,
    REG_VEC_IEEESINGLE = 1 << 7,
    REG_VEC_ALL32      = REG_VEC_UINT32 | REG_VEC_INT32 | REG_VEC_IEEESINGLE,

    REG_VEC_UINT64     = 1 << 8,
    REG_VEC_INT64      = 1 << 9,
    REG_VEC_IEEEDOUBLE = 1 << 10,
    REG_VEC_ALL64      = REG_VEC_UINT64 | REG_VEC_INT64 | REG_VEC_IEEEDOUBLE,

    REG_VEC_UINT128    = 1 << 11,
    REG_VEC_INT128     = 1 << 12,
    REG_VEC_ALL128     = REG_VEC_UINT128 | REG_VEC_INT128,
} RegVecFormats;

/*
 * function pointer prototypes for helper functions
 */
typedef GByteArray *reg_readvec_fn(CPUState *cs, const void *opaque);
typedef void reg_writevec_fn(CPUState *cs, const void *opaque, GByteArray *reg);
typedef void reg_fmtvec_fn(GString *s, CPUState *cs, const void *opaque);

/**
 * reg_add_vector: add a vector register
 * @name: name of register
 * @grp: register group
 * @opaque: data for callback
 * @size: total size in bits
 * @fmts: bitmask of lane sizes
 * @rfn: read function
 * @wfn: write function
 * @fmt: optional format function
 *
 * As vectors may be stored in all sorts of formats internally you
 * must prove read and write access functions. These work with
 * GByteArray's for the data and always pass the full length of the
 * vector.
 */

void reg_add_vector(const char *name, const char *grp, void *opaque,
                    int size, RegVecFormats fmts,
                    reg_readvec_fn rfn, reg_writevec_fn wfn, reg_fmtvec_fn ffn);

/**
 * reg_finalize_definitions(): finalize the register definitions
 * @cs: CPUState
 *
 * Call once all the registers have been declared so the system can
 * finalize the definitions and inform other sub-systems (like gdb).
 */
void reg_finalize_definitions(CPUState *cs);

/**
 * reg_get_number() - return the number of registers in a group.
 * @grp: name of register group, NULL indicates core registers
 *
 * Return the number of registers in a group. This is mostly helpful
 * for unit tests and verifying
 */
int reg_get_number(const char *grp);

/**
 * reg_dump_cpu_state() - dump the current register state to a FD
 * @cs: abstract CPU state pointer
 * @f: FILE descriptor for output
 * @flags: register type selection
 *
 * This function is meant to be a drop in replacement for architecture
 * specific CPUClass dump_state function.
 */
void reg_cpu_dump_state(CPUState *cs, FILE *f, int flags);

/**
 * reg_set_group_gdb_name() - set the GDB name for a group of registers
 * @grp: the QEMU name for the group
 * @gdb: the org.qemu.gdb.ARCH.FEATURE name
 *
 * While the XML for a group can use any name it wants there are some
 * times when we want to report a specific gdb feature name. This
 * allows for GDB to use
 */


/**
 * reg_get_value_hmp(): get the value of a register for HMP
 * @cs: CPU for which we want the value
 * @name: name of the register
 * @val: pointer to int64_t for value
 *
 * This is a temporary API function for the slightly clunky HMP API
 * which can be replaced with nicer code once the old
 * target_monitor_defs code is replaced.
 *
 * Returns true if found and set, false otherwise.
 */
bool reg_get_value_hmp(CPUState *cs, const char *name, int64_t *val);

/* Anonymous handle for a register group */
struct RegGroupHandle;

/**
 * reg_get_group_handle(): return a handle for a group of register
 * @name: name of group
 *
 * Returns a handle for a group of registers or NULL if not found.
 */
struct RegGroupHandle *reg_get_group_handle(const char *name);

/**
 * reg_group_as_string(): return string with register state for group
 * @cs: CPU State pointer
 * @handle: opaque RegGroupHandle
 *
 * Returns an allocated GString, caller frees
 */
GString *reg_group_as_string(CPUState *cs, struct RegGroupHandle *handle);

/**
 * reg_group_list_as_string(): return string with list of groups
 *
 * Returns an allocated GString, caller frees
 */
GString *reg_group_list_as_string(void);

/**
 * reg_gdb_read_register(): read register into byte array
 * @cs: CPU state pointer
 * @buf: GByteArray to fill with data
 * @reg: gdb register number
 *
 * returns the number of bytes read
 */
int reg_gdb_read_register(CPUState *cpu, GByteArray *buf, int reg);

/**
 * reg_gdb_write_register(): write register from buffer into CPU state
 * @cs: CPU state pointer
 * @buf: byte array of data
 * @reg: gdb register number
 *
 * returns the number of bytes written
 */
int reg_gdb_write_register(CPUState *cpu, uint8_t *buf, int reg);

/**
 * reg_gdb_get_dynamic_xml(): return gdb register XML for a group of registers
 * @cs: CPU state pointer
 * @xmlname: name of group (suffixed with .xml)
 *
 * Return the XML for a register group. The core library will generate
 * it *once* if it does not already hold a copy and the returned
 * pointer is stable for the lifetime of the process. The caller may
 * not alter the returned memory.
 */
const char *reg_gdb_get_dynamic_xml(CPUState *cs, const char *xmlname);

