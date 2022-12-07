/*
 * GDB Support for accessing registers
 *
 * Copyright (c) 2022 Linaro Ltd
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include "qemu/osdep.h"
#include "qemu/typedefs.h"
#include "hw/core/cpu.h"
#include "exec/gdbstub.h"
#include "registers/api.h"
#include "internals.h"

int reg_gdb_read_register(CPUState *cpu, GByteArray *buf, int index)
{
    RegDef * reg = reg_get_definition(index);

    switch (reg->type) {
    case REG_DIRECT_ENV:
    {
        uintptr_t env = (uintptr_t) cpu->env_ptr;
        uintptr_t ptr = env + reg->access.env.offset;
        switch (reg->size) {
        case 4:
        {
            /* uint32_t to_long = tswap32(*(uint32_t)ptr); */
            g_byte_array_append(buf, (uint8_t *) ptr, 4);
            return 4;
        }
        case 8:
        {
            /* uint64_t to_quad = tswap64(val); */
            g_byte_array_append(buf, (uint8_t *) ptr, 8);
            return 8;
        }
        default:
            g_assert_not_reached();
        }
    }
    case REG_32BIT_HELPER:
    {
        void *opaque = reg->access.helper32.opaque;
        uint32_t value = reg->access.helper32.read(cpu, opaque);
        g_byte_array_append(buf, (uint8_t *) &value, 4);
        return 4;
    }
    case REG_64BIT_HELPER:
    {
        void *opaque = reg->access.helper64.opaque;
        uint64_t value = reg->access.helper64.read(cpu, opaque);
        g_byte_array_append(buf, (uint8_t *) &value, 8);
        return 8;
    }
    default:
        g_assert_not_reached();
    }

    return 0;
}

int reg_gdb_write_register(CPUState *cpu, uint8_t *buf, int index)
{
    return 0;
}

/*
 * GDB XML Handling
 */


/* GDB types and how they map onto our register definitions */
struct TypeSize {
    const char *gdb_type;
    int  size;
    const char sz, suffix;
    RegVecFormats fmts;
};

static const struct TypeSize gdb_types[] = {
    /* quads */
    { "uint128", 128, 'q', 'u', REG_VEC_UINT128 },
    { "int128", 128, 'q', 's', REG_VEC_INT128 },
    /* 64 bit */
    { "uint64", 64, 'd', 'u', REG_VEC_UINT64 },
    { "int64", 64, 'd', 's', REG_VEC_INT64 },
    { "ieee_double", 64, 'd', 'f', REG_VEC_IEEEDOUBLE },
    /* 32 bit */
    { "uint32", 32, 's', 'u', REG_VEC_UINT32 },
    { "int32", 32, 's', 's', REG_VEC_INT32 },
    { "ieee_single", 32, 's', 'f', REG_VEC_IEEESINGLE },
    /* 16 bit */
    { "uint16", 16, 'h', 'u', REG_VEC_UINT16 },
    { "int16", 16, 'h', 's', REG_VEC_INT16 },
    /*
     * TODO: currently there is no reliable way of telling
     * if the remote gdb actually understands ieee_half so
     * we don't expose it in the target description for now.
     * { "ieee_half", 16, 'h', 'f' },
     */
    /* bytes */
    { "uint8", 8, 'b', 'u', REG_VEC_UINT8 },
    { "int8", 8, 'b', 's', REG_VEC_INT8 },
};

/*
 * Returns first gdb type for a non-vector register
 * FIXME: handle floats better
 */
static const char * get_gdb_type(RegDef *reg)
{
    int i;
    g_assert(reg->type != REG_VECTOR_HELPER);
    for (i = 0; i < ARRAY_SIZE(gdb_types); i++) {
        if (reg->size * 8 == gdb_types[i].size) {
            return gdb_types[i].gdb_type;
        }
    }
    return "unknown";
}

/*
 * For vectors we need to generate the appropriate union types for
 * every combination of union possible in this set of registers. If
 * there are no vector registers we can skip this step.
 */
typedef struct {
    int size;
    RegVecFormats fmts;
} RegVecUnion;

static GArray *maybe_add_new_union(GArray *unions, RegDef *reg)
{
    RegVecFormats this_fmt = reg->access.helpervec.fmts;
    int this_size = reg->size;
    bool already_matched = false;
    int j;

    for (j = 0; j < unions->len && !already_matched; j++) {
        RegVecUnion *check = &g_array_index(unions, RegVecUnion, j);
        if (check->size == this_size) {
            /* We can merge formats if the same size but missing some formats */
            check->fmts |= this_fmt;
            already_matched = true;
        }
    }

    /* another format to add? */
    if (!already_matched) {
        RegVecUnion new_union = {
            .size = this_size,
            .fmts = this_fmt
        };
        unions = g_array_append_val(unions, new_union);
    }

    return unions;
}

static GString * generate_vector_unions(RegGroup *rg, GString *xml)
{
    g_autoptr(GArray) unions = g_array_new(true, true, sizeof(RegVecUnion));
    int i;

    for (i = 0; i < rg->registers->len; i++) {
        RegDef *reg = reg_get_indirect_definition(rg->registers, i);
        if (reg->type == REG_VECTOR_HELPER) {
            unions = maybe_add_new_union(unions, reg);
        }
    }

    /*
     * If we found any vector formats we need to generate the types of
     * form:
     *  <vector id="vector8u" type="uint16" count="8"/>
     * where:
     *   id = vNT (N is number, T is base type)
     * this will be related to the maximum vector size.
     *
     * We then generate the forms all fields of the same size:
     *   <union id="vnh">
     *     <field name="u" type="v8u"/>
     *     <field name="s" type="v8i"/>
     *   </union>
     * where:
     *  id = unique for given vector size
     *  fields = each field in the vector
     *  type = the vector type from above.
     * then a final type union where:
     *  type = includes the previous union names
     *
     * Finally the individual registers:
     *   <reg name="v0" bitsize="128" type="final_type"/>
     */
    if (unions->len) {
        g_autoptr(GString) ts = g_string_new("");
        int bits, j, k;

        for (i = 0; i < unions->len; i++) {
            RegVecUnion *u = &g_array_index(unions, RegVecUnion, i);

            /* e.g. <vector id="vector8u" type="uint16" count="8"/> */
            for (j = 0; j < ARRAY_SIZE(gdb_types); j++) {
                int count = u->size / gdb_types[j].size;
                if (gdb_types[j].fmts & u->fmts) {
                    g_string_printf(ts, "%-s_%d_%c%c", rg->name, count,
                                    gdb_types[j].sz, gdb_types[j].suffix);
                    g_string_append_printf(xml,
                                           "\t<vector id=\"%s\" type=\"%s\" count=\"%d\"/>\n",
                                           ts->str, gdb_types[j].gdb_type, count);
                }
            }

            /*
             * Now define a union for each size group containing unsigned and
             * signed and potentially float versions of each size from 128 to
             * 8 bits depending on the formats required by the vector register.
             */
            for (bits = 128, j = 0; bits >= 8; bits /= 2, j++) {
                const char suf[] = { 'q', 'd', 's', 'h', 'b' };
                g_autoptr(GString) fields = g_string_new("");
                for (k = 0; k < ARRAY_SIZE(gdb_types); k++) {
                    if (gdb_types[k].size == bits && gdb_types[k].fmts & u->fmts) {
                        int count = u->size / gdb_types[k].size;
                        g_string_printf(ts, "%-s_%d_%c%c", rg->name,
                                        count,
                                        gdb_types[k].sz, gdb_types[k].suffix);
                        g_string_append_printf(fields, "\t\t<field name=\"%c\" type=\"%s\"/>\n",
                                               gdb_types[j].suffix, ts->str);
                    }
                }
                /* only emit union if we have fields that matched the formats we need */
                if (fields->len) {
                    g_string_append_printf(xml, "\t<union id=\"%-s_%c\">\n%s\t</union>\n",
                                           rg->name, suf[j], fields->str);
                }
            }

            /* final union on unions for the size of the vector */
            g_string_append_printf(xml, "\t<union id=\"%-s_%d\">\n", rg->name, u->size * 8);
            for (bits = 128, j = 0; bits >= 8; bits /= 2, j++) {
                const char suf[] = { 'q', 'd', 's', 'h', 'b' };
                g_string_append_printf(xml, "\t\t<field name=\"%c\" type=\"%-s_%c\"/>\n",
                                       suf[j], rg->name, suf[j]);
            }
            g_string_append(xml, "\t</union>\n");
        }
    }

    return xml;
}

/* Generate some gdb XML for a group of registers */
static const char * generate_gdb_xml(RegGroup *rg)
{
    GString *xml = g_string_new("<?xml version=\"1.0\"?>");
    int i;

    g_string_append_printf(xml, "<!DOCTYPE target SYSTEM \"gdb-target.dtd\">\n");
    if (rg->gdb_name) {
        g_string_append_printf(xml, "<feature name=\"%s\">\n", rg->gdb_name);
    } else {
        g_string_append_printf(xml, "<feature name=\"org.qemu.gdb.%s\">\n", rg->name);
    }

    /* Generate any unions we need */
    xml = generate_vector_unions(rg, xml);

    /* Iterate over the registers with an entry for each */
    for (i = 0; i < rg->registers->len; i++) {
        RegDef *reg = reg_get_indirect_definition(rg->registers, i);
        switch (reg->type) {
        case REG_DIRECT_ENV:
        case REG_32BIT_HELPER:
        case REG_64BIT_HELPER:
            g_string_append_printf(xml,
                                   "<reg name=\"%s\" bitsize=\"%d\""
                                   " regnum=\"%d\" type=\"%s\"/>\n", reg->name, reg->size * 8,
                                   i, get_gdb_type(reg));
            break;
        case REG_VECTOR_HELPER:
        {
            int bits = reg->size * 8;
            g_string_append_printf(xml,
                                   "<reg name=\"%s\" bitsize=\"%d\""
                                   " regnum=\"%d\" type=\"%-s_%d\"/>\n",
                                   reg->name, bits, i, rg->name, bits);
            break;
        }
        default:
            g_assert_not_reached();
        }
    }

    g_string_append_printf(xml, "</feature>");

    return g_string_free(xml, false);
}

const char *reg_gdb_get_dynamic_xml(CPUState *cs, const char *xmlname)
{
    g_autofree char *group = NULL;
    char *xml_suffix = g_strrstr(xmlname, ".xml");
    RegGroup *rg;

    if (xml_suffix) {
        group = g_strndup(xmlname, xml_suffix - xmlname);
    } else {
        group = g_strdup(xmlname);
    }

    rg = reg_find_group(group);

    if (!rg) {
        return NULL;
    }

    if (!rg->xml) {
        rg->xml = generate_gdb_xml(rg);
    }

    return rg->xml;
}

/* static int reg_gdb_get_reg(CPUArchState *env, GByteArray *buf, int reg) */
/* { */
/*     return 0; */
/* } */

/* static int reg_gdb_set_reg(CPUArchState *env, uint8_t *buf, int reg) */
/* { */
/*     return 0; */
/* } */

void reg_register_with_gdb(CPUState *cs)
{
    /*
     * When unit testing cs isn't real so we have to skip this bit
     */
    if (cs) {
        CPUClass *cc = CPU_GET_CLASS(cs);
        GArray *groups = reg_get_groups();
        int i;

        /*
         * Having registered the incorrect number of core registers
         * would be a bug.
         */
        g_assert(cc->gdb_num_core_regs == reg_get_number(NULL));

        for (i = 0; i < groups->len; i++) {
            RegGroup *rg = &g_array_index(groups, RegGroup, i);
            char *xml_name = g_strdup_printf("%s.xml", rg->name);
            gdb_register_coprocessor(cs, NULL, NULL,
                                     rg->registers->len, xml_name, rg->global_base);
        }
    }
}
