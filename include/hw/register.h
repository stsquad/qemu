/*
 * Register Definition API
 *
 * Copyright (c) 2016 Xilinx Inc.
 * Copyright (c) 2013 Peter Crosthwaite <peter.crosthwaite@xilinx.com>
 *
 * This work is licensed under the terms of the GNU GPL, version 2.  See
 * the COPYING file in the top-level directory.
 */

#ifndef REGISTER_H
#define REGISTER_H

#include "hw/qdev-core.h"
#include "exec/memory.h"
#include "hw/irq.h"

typedef struct RegisterInfo RegisterInfo;
typedef struct RegisterAccessInfo RegisterAccessInfo;
typedef struct RegisterInfoArray RegisterInfoArray;

#define REG_GPIO_POL_HIGH 0
#define REG_GPIO_POL_LOW  1

typedef struct RegisterGPIOMapping {
    const char *name;
    uint8_t bit_pos;
    bool input;
    bool polarity;
    uint8_t num;
    uint8_t width;
} RegisterGPIOMapping;

/**
 * Access description for a register that is part of guest accessible device
 * state.
 *
 * @name: String name of the register
 * @ro: whether or not the bit is read-only
 * @w1c: bits with the common write 1 to clear semantic.
 * @reset: reset value.
 * @cor: Bits that are clear on read
 * @rsvd: Bits that are reserved and should not be changed
 *
 * @pre_write: Pre write callback. Passed the value that's to be written,
 * immediately before the actual write. The returned value is what is written,
 * giving the handler a chance to modify the written value.
 * @post_write: Post write callback. Passed the written value. Most write side
 * effects should be implemented here.
 *
 * @post_read: Post read callback. Passes the value that is about to be returned
 * for a read. The return value from this function is what is ultimately read,
 * allowing this function to modify the value before return to the client.
 */

#define REG_DECODE_READ (1 << 0)
#define REG_DECODE_WRITE (1 << 1)
#define REG_DECODE_EXECUTE (1 << 2)
#define REG_DECODE_RW (REG_DECODE_READ | REG_DECODE_WRITE)

struct RegisterAccessInfo {
    const char *name;
    uint64_t ro;
    uint64_t w1c;
    uint64_t reset;
    uint64_t cor;
    uint64_t rsvd;
    uint64_t unimp;

    uint64_t (*pre_write)(RegisterInfo *reg, uint64_t val);
    void (*post_write)(RegisterInfo *reg, uint64_t val);

    uint64_t (*post_read)(RegisterInfo *reg, uint64_t val);

    const RegisterGPIOMapping *gpios;

    struct {
        hwaddr addr;
    } decode;
};

/**
 * A register that is part of guest accessible state
 * @data: pointer to the register data. Will be cast
 * to the relevant uint type depending on data_size.
 * @data_size: Size of the register in bytes. Must be
 * 1, 2, 4 or 8
 *
 * @access: Access description of this register
 *
 * @debug: Whether or not verbose debug is enabled
 * @prefix: String prefix for log and debug messages
 *
 * @opaque: Opaque data for the register
 */

struct RegisterInfo {
    /* <private> */
    DeviceState parent_obj;

    /* <public> */
    void *data;
    int data_size;

    const RegisterAccessInfo *access;

    bool debug;
    const char *prefix;

    void *opaque;
};

#define TYPE_REGISTER "qemu,register"
#define REGISTER(obj) OBJECT_CHECK(RegisterInfo, (obj), TYPE_REGISTER)

/**
 * This structure is used to group all of the individual registers which are
 * modeled using the RegisterInfo strucutre.
 *
 * @r is an aray containing of all the relevent RegisterInfo structures.
 *
 * @num_elements is the number of elements in the array r
 *
 * @mem: optional Memory region for the register
 */

struct RegisterInfoArray {
    /* <private> */
    MemoryRegion mem;

    /* <public> */
    int num_elements;
    RegisterInfo **r;
};

/**
 * write a value to a register, subject to its restrictions
 * @reg: register to write to
 * @val: value to write
 * @we: write enable mask
 */

void register_write(RegisterInfo *reg, uint64_t val, uint64_t we);

/**
 * read a value from a register, subject to its restrictions
 * @reg: register to read from
 * returns: value read
 */

uint64_t register_read(RegisterInfo *reg);

/**
 * reset a register
 * @reg: register to reset
 */

void register_reset(RegisterInfo *reg);

/**
 * Initialize a register. GPIO's are setup as IOs to the specified device.
 * Fast paths for eligible registers are enabled.
 * @reg: Register to initialize
 */

void register_init(RegisterInfo *reg);

/**
 * Refresh GPIO outputs based on diff between old value register current value.
 * GPIOs are refreshed for fields where the old value differs to the current
 * value.
 *
 * @reg: Register to refresh GPIO outs
 * @old_value: previous value of register
 */

void register_refresh_gpios(RegisterInfo *reg, uint64_t old_value);

/**
 * Memory API MMIO write handler that will write to a Register API register.
 *  _be for big endian variant and _le for little endian.
 * @opaque: RegisterInfo to write to
 * @addr: Address to write
 * @value: Value to write
 * @size: Number of bytes to write
 */

void register_write_memory_be(void *opaque, hwaddr addr, uint64_t value,
                              unsigned size);
void register_write_memory_le(void *opaque, hwaddr addr, uint64_t value,
                              unsigned size);

/**
 * Memory API MMIO read handler that will read from a Register API register.
 *  _be for big endian variant and _le for little endian.
 * @opaque: RegisterInfo to read from
 * @addr: Address to read
 * @size: Number of bytes to read
 * returns: Value read from register
 */

uint64_t register_read_memory_be(void *opaque, hwaddr addr, unsigned size);
uint64_t register_read_memory_le(void *opaque, hwaddr addr, unsigned size);

/**
 * Init a block of consecutive registers into a container MemoryRegion. A
 * number of constant register definitions are parsed to create a corresponding
 * array of RegisterInfo's.
 *
 * @owner: device owning the registers
 * @rae: Register definitions to init
 * @num: number of registers to init (length of @rae)
 * @ri: Register array to init
 * @data: Array to use for register data
 * @container: Memory region to contain new registers
 * @ops: Memory region ops to access registers.
 * @debug enabled: turn on/off verbose debug information
 */

void register_init_block32(DeviceState *owner, const RegisterAccessInfo *rae,
                           int num, RegisterInfo *ri, uint32_t *data,
                           MemoryRegion *container, const MemoryRegionOps *ops,
                           bool debug_enabled, uint64_t memory_size);

/* Define constants for a 32 bit register */
#define REG32(reg, addr)                                                  \
    enum { A_ ## reg = (addr) };                                          \
    enum { R_ ## reg = (addr) / 4 };

/* Define SHIFT, LEGTH and MASK constants for a field within a register */
#define FIELD(reg, field, shift, length)                                  \
    enum { R_ ## reg ## _ ## field ## _SHIFT = (shift)};                  \
    enum { R_ ## reg ## _ ## field ## _LENGTH = (length)};                \
    enum { R_ ## reg ## _ ## field ## _MASK = (((1ULL << (length)) - 1)   \
                                          << (shift)) };

/* Extract a field from a register */

#define F_EX32(storage, reg, field)                                       \
    extract32((storage), R_ ## reg ## _ ## field ## _SHIFT,               \
              R_ ## reg ## _ ## field ## _LENGTH)

/* Extract a field from an array of registers */

#define AF_EX32(regs, reg, field)                                         \
    F_EX32((regs)[R_ ## reg], reg, field)

/* Deposit a register field.  */

#define F_DP32(storage, reg, field, val) ({                               \
    struct {                                                              \
        unsigned int v:R_ ## reg ## _ ## field ## _LENGTH;                \
    } v = { .v = val };                                                   \
    uint32_t d;                                                           \
    d = deposit32((storage), R_ ## reg ## _ ## field ## _SHIFT,           \
                  R_ ## reg ## _ ## field ## _LENGTH, v.v);               \
    d; })

/* Deposit a field to array of registers.  */

#define AF_DP32(regs, reg, field, val)                                    \
    (regs)[R_ ## reg] = F_DP32((regs)[R_ ## reg], reg, field, val);
#endif
