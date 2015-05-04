/* Support for generating ACPI tables and passing them to Guests
 *
 * ARM virt ACPI generation
 *
 * Copyright (C) 2008-2010  Kevin O'Connor <kevin@koconnor.net>
 * Copyright (C) 2006 Fabrice Bellard
 * Copyright (C) 2013 Red Hat Inc
 *
 * Author: Michael S. Tsirkin <mst@redhat.com>
 *
 * Copyright (c) 2015 HUAWEI TECHNOLOGIES CO.,LTD.
 *
 * Author: Shannon Zhao <zhaoshenglong@huawei.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License along
 * with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#include "hw/arm/virt-acpi-build.h"
#include <stddef.h>
#include <glib.h>
#include "qemu-common.h"
#include "qemu/bitmap.h"
#include "trace.h"
#include "qom/cpu.h"
#include "target-arm/cpu.h"
#include "hw/acpi/acpi-defs.h"
#include "hw/acpi/acpi.h"
#include "hw/nvram/fw_cfg.h"
#include "hw/acpi/bios-linker-loader.h"
#include "hw/loader.h"
#include "hw/hw.h"
#include "hw/acpi/aml-build.h"
#include "hw/pci/pcie_host.h"
#include "hw/pci/pci.h"

typedef struct VirtAcpiCpuInfo {
    DECLARE_BITMAP(found_cpus, VIRT_ACPI_CPU_ID_LIMIT);
} VirtAcpiCpuInfo;

static void virt_acpi_get_cpu_info(VirtAcpiCpuInfo *cpuinfo)
{
    CPUState *cpu;

    memset(cpuinfo->found_cpus, 0, sizeof cpuinfo->found_cpus);
    CPU_FOREACH(cpu) {
        set_bit(cpu->cpu_index, cpuinfo->found_cpus);
    }
}

static void acpi_dsdt_add_cpus(Aml *scope, int smp_cpus)
{
    uint16_t i;

    for (i = 0; i < smp_cpus; i++) {
        Aml *dev = aml_device("C%03x", i);
        aml_append(dev, aml_name_decl("_HID", aml_string("ACPI0007")));
        aml_append(dev, aml_name_decl("_UID", aml_int(i)));
        Aml *crs = aml_resource_template();
        aml_append(dev, aml_name_decl("_CRS", crs));
        aml_append(scope, dev);
    }
}

static void acpi_dsdt_add_uart(Aml *scope, const MemMap *uart_memmap,
                                           const int *uart_irq)
{
    Aml *dev = aml_device("COM0");
    aml_append(dev, aml_name_decl("_HID", aml_string("ARMH0011")));
    aml_append(dev, aml_name_decl("_UID", aml_int(0)));

    Aml *crs = aml_resource_template();
    aml_append(crs, aml_memory32_fixed(uart_memmap->addr,
                                       uart_memmap->size, aml_ReadWrite));
    aml_append(crs,
               aml_interrupt(aml_consumer, aml_level, aml_active_high,
               aml_exclusive, aml_not_wake_capable, *uart_irq + 32));
    aml_append(dev, aml_name_decl("_CRS", crs));
    aml_append(scope, dev);
}

static void acpi_dsdt_add_rtc(Aml *scope, const MemMap *rtc_memmap,
                                          const int *rtc_irq)
{
    Aml *dev = aml_device("RTC0");
    aml_append(dev, aml_name_decl("_HID", aml_string("LNRO0013")));
    aml_append(dev, aml_name_decl("_UID", aml_int(0)));

    Aml *crs = aml_resource_template();
    aml_append(crs, aml_memory32_fixed(rtc_memmap->addr,
                                       rtc_memmap->size, aml_ReadWrite));
    aml_append(crs,
               aml_interrupt(aml_consumer, aml_level, aml_active_high,
               aml_exclusive, aml_not_wake_capable, *rtc_irq + 32));
    aml_append(dev, aml_name_decl("_CRS", crs));
    aml_append(scope, dev);
}

static void acpi_dsdt_add_flash(Aml *scope, const MemMap *flash_memmap)
{
    Aml *dev, *crs;
    hwaddr base = flash_memmap->addr;
    hwaddr size = flash_memmap->size;

    dev = aml_device("FLS0");
    aml_append(dev, aml_name_decl("_HID", aml_string("LNRO0015")));
    aml_append(dev, aml_name_decl("_UID", aml_int(0)));

    crs = aml_resource_template();
    aml_append(crs, aml_memory32_fixed(base, size, aml_ReadWrite));
    aml_append(dev, aml_name_decl("_CRS", crs));
    aml_append(scope, dev);

    dev = aml_device("FLS1");
    aml_append(dev, aml_name_decl("_HID", aml_string("LNRO0015")));
    aml_append(dev, aml_name_decl("_UID", aml_int(1)));
    crs = aml_resource_template();
    aml_append(crs, aml_memory32_fixed(base + size, size, aml_ReadWrite));
    aml_append(dev, aml_name_decl("_CRS", crs));
    aml_append(scope, dev);
}

static void acpi_dsdt_add_virtio(Aml *scope, const MemMap *virtio_mmio_memmap,
                                             const int *mmio_irq, int num)
{
    hwaddr base = virtio_mmio_memmap->addr;
    hwaddr size = virtio_mmio_memmap->size;
    int irq = *mmio_irq + 32;
    int i;

    for (i = 0; i < num; i++) {
        Aml *dev = aml_device("VR%02u", i);
        aml_append(dev, aml_name_decl("_HID", aml_string("LNRO0005")));
        aml_append(dev, aml_name_decl("_UID", aml_int(i)));

        Aml *crs = aml_resource_template();
        aml_append(crs, aml_memory32_fixed(base, size, aml_ReadWrite));
        aml_append(crs,
                   aml_interrupt(aml_consumer, aml_level, aml_active_high,
                   aml_exclusive, aml_not_wake_capable, irq + i));
        aml_append(dev, aml_name_decl("_CRS", crs));
        aml_append(scope, dev);
        base += size;
    }
}

static void acpi_dsdt_add_pci(Aml *scope, AcpiPcieInfo *info)
{
    Aml *method, *crs, *ifctx, *UUID, *ifctx1, *elsectx, *buf;
    int i, bus_no;
    int irq = *info->pcie_irq + 32;

    Aml *dev = aml_device("%s", "PCI0");
    aml_append(dev, aml_name_decl("_HID", aml_string("PNP0A08")));
    aml_append(dev, aml_name_decl("_CID", aml_string("PNP0A03")));
    aml_append(dev, aml_name_decl("_SEG", aml_int(0)));
    aml_append(dev, aml_name_decl("_BBN", aml_int(0)));
    aml_append(dev, aml_name_decl("_ADR", aml_int(0)));
    aml_append(dev, aml_name_decl("_UID", aml_string("PCI0")));
    aml_append(dev, aml_name_decl("_STR", aml_unicode("PCIe 0 Device")));

    /* Declare the PCI Routing Table. */
    Aml *rt_pkg = aml_package(info->nr_pcie_buses * PCI_NUM_PINS);
    for (bus_no = 0; bus_no < info->nr_pcie_buses; bus_no++) {
        for (i = 0; i < PCI_NUM_PINS; i++) {
            int gsi = (i + bus_no) % PCI_NUM_PINS;
            Aml *pkg = aml_package(4);
            aml_append(pkg, aml_int((bus_no << 16) | 0xFFFF));
            aml_append(pkg, aml_int(i));
            aml_append(pkg, aml_name("GSI%d", gsi));
            aml_append(pkg, aml_int(0));
            aml_append(rt_pkg, pkg);
        }
    }
    aml_append(dev, aml_name_decl("_PRT", rt_pkg));

    /* Create GSI link device */
    for (i = 0; i < PCI_NUM_PINS; i++) {
        Aml *dev_gsi = aml_device("GSI%d", i);
        aml_append(dev_gsi, aml_name_decl("_HID", aml_string("PNP0C0F")));
        aml_append(dev_gsi, aml_name_decl("_UID", aml_int(0)));
        crs = aml_resource_template();
        aml_append(crs,
                   aml_interrupt(aml_consumer, aml_level, aml_active_high,
                   aml_exclusive, aml_not_wake_capable, irq + i));
        aml_append(dev_gsi, aml_name_decl("_PRS", crs));
        crs = aml_resource_template();
        aml_append(crs,
                   aml_interrupt(aml_consumer, aml_level, aml_active_high,
                   aml_exclusive, aml_not_wake_capable, irq + i));
        aml_append(dev_gsi, aml_name_decl("_CRS", crs));
        method = aml_method("_SRS", 1);
        aml_append(dev_gsi, method);
        aml_append(dev, dev_gsi);
    }

    method = aml_method("_CBA", 0);
    aml_append(method, aml_return(aml_int(info->pcie_ecam.addr)));
    aml_append(dev, method);

    method = aml_method("_CRS", 0);
    Aml *rbuf = aml_resource_template();
    aml_append(rbuf,
        aml_word_bus_number(aml_min_fixed, aml_max_fixed, aml_pos_decode,
                            0x0000, 0x0000, info->nr_pcie_buses - 1,
                            0x0000, info->nr_pcie_buses));
    aml_append(rbuf,
        aml_dword_memory(aml_pos_decode, aml_min_fixed, aml_max_fixed,
                         aml_non_cacheable, aml_ReadWrite,
                         0x0000, info->pcie_mmio.addr,
                         info->pcie_mmio.addr + info->pcie_mmio.size - 1,
                         0x0000, info->pcie_mmio.size));
    aml_append(rbuf,
        aml_dword_io(aml_min_fixed, aml_max_fixed,
                     aml_pos_decode, aml_entire_range,
                     0x0000, 0x0000, info->pcie_ioport.size - 1,
                     info->pcie_ioport.addr, info->pcie_ioport.size));

    aml_append(method, aml_name_decl("RBUF", rbuf));
    aml_append(method, aml_return(rbuf));
    aml_append(dev, method);

    /* Declare an _OSC (OS Control Handoff) method */
    aml_append(dev, aml_name_decl("SUPP", aml_int(0)));
    aml_append(dev, aml_name_decl("CTRL", aml_int(0)));
    method = aml_method("_OSC", 4);
    aml_append(method,
        aml_create_dword_field(aml_arg(3), aml_int(0), "CDW1"));

    /* PCI Firmware Specification 3.0
     * 4.5.1. _OSC Interface for PCI Host Bridge Devices
     * The _OSC interface for a PCI/PCI-X/PCI Express hierarchy is
     * identified by the Universal Unique IDentifier (UUID)
     * 33DB4D5B-1FF7-401C-9657-7441C03DD766
     */
    UUID = aml_touuid("33DB4D5B-1FF7-401C-9657-7441C03DD766");
    ifctx = aml_if(aml_equal(aml_arg(0), UUID));
    aml_append(ifctx,
        aml_create_dword_field(aml_arg(3), aml_int(4), "CDW2"));
    aml_append(ifctx,
        aml_create_dword_field(aml_arg(3), aml_int(8), "CDW3"));
    aml_append(ifctx, aml_store(aml_name("CDW2"), aml_name("SUPP")));
    aml_append(ifctx, aml_store(aml_name("CDW3"), aml_name("CTRL")));
    aml_append(ifctx, aml_store(aml_and(aml_name("CTRL"), aml_int(0x1D)),
                                aml_name("CTRL")));

    ifctx1 = aml_if(aml_lnot(aml_equal(aml_arg(1), aml_int(0x1))));
    aml_append(ifctx1, aml_store(aml_or(aml_name("CDW1"), aml_int(0x08)),
                                 aml_name("CDW1")));
    aml_append(ifctx, ifctx1);

    ifctx1 = aml_if(aml_lnot(aml_equal(aml_name("CDW3"), aml_name("CTRL"))));
    aml_append(ifctx1, aml_store(aml_or(aml_name("CDW1"), aml_int(0x10)),
                                 aml_name("CDW1")));
    aml_append(ifctx, ifctx1);

    aml_append(ifctx, aml_store(aml_name("CTRL"), aml_name("CDW3")));
    aml_append(ifctx, aml_return(aml_arg(3)));
    aml_append(method, ifctx);

    elsectx = aml_else();
    aml_append(elsectx, aml_store(aml_or(aml_name("CDW1"), aml_int(4)),
                                  aml_name("CDW1")));
    aml_append(elsectx, aml_return(aml_arg(3)));
    aml_append(method, elsectx);
    aml_append(dev, method);

    method = aml_method("_DSM", 4);

    /* PCI Firmware Specification 3.0
     * 4.6.1. _DSM for PCI Express Slot Information
     * The UUID in _DSM in this context is
     * {E5C937D0-3553-4D7A-9117-EA4D19C3434D}
     */
    UUID = aml_touuid("E5C937D0-3553-4D7A-9117-EA4D19C3434D");
    ifctx = aml_if(aml_equal(aml_arg(0), UUID));
    ifctx1 = aml_if(aml_equal(aml_arg(2), aml_int(0)));
    uint8_t byte_list[1] = {1};
    buf = aml_buffer(1, byte_list);
    aml_append(ifctx1, aml_return(buf));
    aml_append(ifctx, ifctx1);
    aml_append(method, ifctx);

    byte_list[0] = 0;
    buf = aml_buffer(1, byte_list);
    aml_append(method, aml_return(buf));
    aml_append(dev, method);

    Aml *dev_rp0 = aml_device("%s", "RP0");
    aml_append(dev_rp0, aml_name_decl("_ADR", aml_int(0)));
    aml_append(dev, dev_rp0);
    aml_append(scope, dev);
}

/* RSDP */
static GArray *
build_rsdp(GArray *rsdp_table, GArray *linker, unsigned rsdt)
{
    AcpiRsdpDescriptor *rsdp = acpi_data_push(rsdp_table, sizeof *rsdp);

    bios_linker_loader_alloc(linker, ACPI_BUILD_RSDP_FILE, 16,
                             true /* fseg memory */);

    memcpy(&rsdp->signature, "RSD PTR ", sizeof(rsdp->signature));
    memcpy(rsdp->oem_id, ACPI_BUILD_APPNAME6, sizeof(rsdp->oem_id));
    rsdp->length = cpu_to_le32(sizeof(*rsdp));
    rsdp->revision = 0x02;

    /* Point to RSDT */
    rsdp->rsdt_physical_address = cpu_to_le32(rsdt);
    /* Address to be filled by Guest linker */
    bios_linker_loader_add_pointer(linker, ACPI_BUILD_RSDP_FILE,
                                   ACPI_BUILD_TABLE_FILE,
                                   rsdp_table, &rsdp->rsdt_physical_address,
                                   sizeof rsdp->rsdt_physical_address);
    rsdp->checksum = 0;
    /* Checksum to be filled by Guest linker */
    bios_linker_loader_add_checksum(linker, ACPI_BUILD_RSDP_FILE,
                                    rsdp, rsdp, sizeof *rsdp, &rsdp->checksum);

    return rsdp_table;
}

static void
build_mcfg(GArray *table_data, GArray *linker, VirtGuestInfo *guest_info)
{
    AcpiTableMcfg *mcfg;
    AcpiPcieInfo *info = guest_info->pcie_info;
    int len = sizeof(*mcfg) + sizeof(mcfg->allocation[0]);

    mcfg = acpi_data_push(table_data, len);
    mcfg->allocation[0].address = cpu_to_le64(info->pcie_ecam.addr);

    /* Only a single allocation so no need to play with segments */
    mcfg->allocation[0].pci_segment = cpu_to_le16(0);
    mcfg->allocation[0].start_bus_number = 0;
    mcfg->allocation[0].end_bus_number = info->nr_pcie_buses - 1;

    build_header(linker, table_data, (void *)mcfg, "MCFG", len, 1);
}

/* GTDT */
static void
build_gtdt(GArray *table_data, GArray *linker, VirtGuestInfo *guest_info)
{
    int gtdt_start = table_data->len;
    const struct AcpiGtdtInfo *info = guest_info->gtdt_info;
    AcpiGenericTimerTable *gtdt;

    gtdt = acpi_data_push(table_data, sizeof *gtdt);
    /* The interrupt values are the same with the device tree when adding 16 */
    gtdt->secure_el1_interrupt = info->timer_s_el1;
    gtdt->secure_el1_flags = ACPI_EDGE_SENSITIVE;

    gtdt->non_secure_el1_interrupt = info->timer_ns_el1;
    gtdt->non_secure_el1_flags = ACPI_EDGE_SENSITIVE;

    gtdt->virtual_timer_interrupt = info->timer_virt;
    gtdt->virtual_timer_flags = ACPI_EDGE_SENSITIVE;

    gtdt->non_secure_el2_interrupt = info->timer_ns_el2;
    gtdt->non_secure_el2_flags = ACPI_EDGE_SENSITIVE;

    build_header(linker, table_data,
                 (void *)(table_data->data + gtdt_start), "GTDT",
                 table_data->len - gtdt_start, 5);
}

/* MADT */
static void
build_madt(GArray *table_data, GArray *linker, VirtGuestInfo *guest_info,
           VirtAcpiCpuInfo *cpuinfo)
{
    int madt_start = table_data->len;
    const struct AcpiMadtInfo *info = guest_info->madt_info;
    AcpiMultipleApicTable *madt;
    AcpiMadtGenericDistributor *gicd;
    int i;

    madt = acpi_data_push(table_data, sizeof *madt);

    for (i = 0; i < guest_info->smp_cpus; i++) {
        AcpiMadtGenericInterrupt *gicc = acpi_data_push(table_data,
                                                     sizeof *gicc);
        gicc->type = ACPI_APIC_GENERIC_INTERRUPT;
        gicc->length = sizeof(*gicc);
        gicc->base_address = info->gic_cpu_memmap->addr;
        gicc->cpu_interface_number = i;
        gicc->arm_mpidr = i;
        gicc->uid = i;
        if (test_bit(i, cpuinfo->found_cpus)) {
            gicc->flags = cpu_to_le32(ACPI_GICC_ENABLED);
        }
    }

    gicd = acpi_data_push(table_data, sizeof *gicd);
    gicd->type = ACPI_APIC_GENERIC_DISTRIBUTOR;
    gicd->length = sizeof(*gicd);
    gicd->base_address = info->gic_dist_memmap->addr;

    build_header(linker, table_data,
                 (void *)(table_data->data + madt_start), "APIC",
                 table_data->len - madt_start, 5);
}

/* FADT */
static void
build_fadt(GArray *table_data, GArray *linker, unsigned dsdt)
{
    AcpiFadtDescriptorRev5_1 *fadt = acpi_data_push(table_data, sizeof(*fadt));

    /* Hardware Reduced = 1 and use PSCI 0.2+ and with HVC */
    fadt->flags = cpu_to_le32(1 << ACPI_FADT_F_HW_REDUCED_ACPI);
    fadt->arm_boot_flags = cpu_to_le16((1 << ACPI_FADT_ARM_USE_PSCI_G_0_2) |
                                       (1 << ACPI_FADT_ARM_PSCI_USE_HVC));

    /* ACPI v5.1 (fadt->revision.fadt->minor_revision) */
    fadt->minor_revision = 0x1;

    fadt->dsdt = cpu_to_le32(dsdt);
    /* DSDT address to be filled by Guest linker */
    bios_linker_loader_add_pointer(linker, ACPI_BUILD_TABLE_FILE,
                                   ACPI_BUILD_TABLE_FILE,
                                   table_data, &fadt->dsdt,
                                   sizeof fadt->dsdt);

    build_header(linker, table_data,
                 (void *)fadt, "FACP", sizeof(*fadt), 5);
}

/* DSDT */
static void
build_dsdt(GArray *table_data, GArray *linker, VirtGuestInfo *guest_info)
{
    Aml *scope, *dsdt;
    AcpiDsdtInfo *info = guest_info->dsdt_info;

    dsdt = init_aml_allocator();
    /* Reserve space for header */
    acpi_data_push(dsdt->buf, sizeof(AcpiTableHeader));

    scope = aml_scope("\\_SB");
    acpi_dsdt_add_cpus(scope, guest_info->smp_cpus);
    acpi_dsdt_add_uart(scope, info->uart_memmap, info->uart_irq);
    acpi_dsdt_add_rtc(scope, info->rtc_memmap, info->rtc_irq);
    acpi_dsdt_add_flash(scope, info->flash_memmap);
    acpi_dsdt_add_virtio(scope, info->virtio_mmio_memmap,
             info->virtio_mmio_irq, info->virtio_mmio_num);
    acpi_dsdt_add_pci(scope, guest_info->pcie_info);

    aml_append(dsdt, scope);

    /* copy AML table into ACPI tables blob and patch header there */
    g_array_append_vals(table_data, dsdt->buf->data, dsdt->buf->len);
    build_header(linker, table_data,
        (void *)(table_data->data + table_data->len - dsdt->buf->len),
        "DSDT", dsdt->buf->len, 5);
    free_aml_allocator();
}

typedef
struct AcpiBuildState {
    /* Copy of table in RAM (for patching). */
    MemoryRegion *table_mr;
    MemoryRegion *rsdp_mr;
    MemoryRegion *linker_mr;
    /* Is table patched? */
    uint8_t patched;
    VirtGuestInfo *guest_info;
} AcpiBuildState;

static
void virt_acpi_build(VirtGuestInfo *guest_info, AcpiBuildTables *tables)
{
    GArray *table_offsets;
    unsigned dsdt, rsdt;
    VirtAcpiCpuInfo cpuinfo;
    GArray *tables_blob = tables->table_data;

    virt_acpi_get_cpu_info(&cpuinfo);

    table_offsets = g_array_new(false, true /* clear */,
                                        sizeof(uint32_t));

    bios_linker_loader_alloc(tables->linker, ACPI_BUILD_TABLE_FILE,
                             64, false /* high memory */);

    /*
     * The ACPI v5.1 tables for Hardware-reduced ACPI platform are:
     * RSDP
     * RSDT
     * FADT
     * GTDT
     * MADT
     * DSDT
     */

    /* DSDT is pointed to by FADT */
    dsdt = tables_blob->len;
    build_dsdt(tables_blob, tables->linker, guest_info);

    /* FADT MADT GTDT pointed to by RSDT */
    acpi_add_table(table_offsets, tables_blob);
    build_fadt(tables_blob, tables->linker, dsdt);

    acpi_add_table(table_offsets, tables_blob);
    build_madt(tables_blob, tables->linker, guest_info, &cpuinfo);

    acpi_add_table(table_offsets, tables_blob);
    build_gtdt(tables_blob, tables->linker, guest_info);

    acpi_add_table(table_offsets, tables_blob);
    build_mcfg(tables_blob, tables->linker, guest_info);

    /* RSDT is pointed to by RSDP */
    rsdt = tables_blob->len;
    build_rsdt(tables_blob, tables->linker, table_offsets);

    /* RSDP is in FSEG memory, so allocate it separately */
    build_rsdp(tables->rsdp, tables->linker, rsdt);

    /* Cleanup memory that's no longer used. */
    g_array_free(table_offsets, true);
}

static void acpi_ram_update(MemoryRegion *mr, GArray *data)
{
    uint32_t size = acpi_data_len(data);

    /* Make sure RAM size is correct - in case it got changed
     * e.g. by migration */
    memory_region_ram_resize(mr, size, &error_abort);

    memcpy(memory_region_get_ram_ptr(mr), data->data, size);
    memory_region_set_dirty(mr, 0, size);
}

static void virt_acpi_build_update(void *build_opaque, uint32_t offset)
{
    AcpiBuildState *build_state = build_opaque;
    AcpiBuildTables tables;

    /* No state to update or already patched? Nothing to do. */
    if (!build_state || build_state->patched) {
        return;
    }
    build_state->patched = 1;

    acpi_build_tables_init(&tables);

    virt_acpi_build(build_state->guest_info, &tables);

    acpi_ram_update(build_state->table_mr, tables.table_data);
    acpi_ram_update(build_state->rsdp_mr, tables.rsdp);
    acpi_ram_update(build_state->linker_mr, tables.linker);


    acpi_build_tables_cleanup(&tables, true);
}

static void virt_acpi_build_reset(void *build_opaque)
{
    AcpiBuildState *build_state = build_opaque;
    build_state->patched = 0;
}

static MemoryRegion *acpi_add_rom_blob(AcpiBuildState *build_state,
                                       GArray *blob, const char *name,
                                       uint64_t max_size)
{
    return rom_add_blob(name, blob->data, acpi_data_len(blob), max_size, -1,
                        name, virt_acpi_build_update, build_state);
}

static const VMStateDescription vmstate_virt_acpi_build = {
    .name = "virt_acpi_build",
    .version_id = 1,
    .minimum_version_id = 1,
    .fields = (VMStateField[]) {
        VMSTATE_UINT8(patched, AcpiBuildState),
        VMSTATE_END_OF_LIST()
    },
};

void virt_acpi_setup(VirtGuestInfo *guest_info)
{
    AcpiBuildTables tables;
    AcpiBuildState *build_state;

    if (!guest_info->fw_cfg) {
        trace_virt_acpi_setup();
        return;
    }

    if (!acpi_enabled) {
        trace_virt_acpi_setup();
        return;
    }

    build_state = g_malloc0(sizeof *build_state);
    build_state->guest_info = guest_info;

    acpi_build_tables_init(&tables);
    virt_acpi_build(build_state->guest_info, &tables);

    /* Now expose it all to Guest */
    build_state->table_mr = acpi_add_rom_blob(build_state, tables.table_data,
                                               ACPI_BUILD_TABLE_FILE,
                                               ACPI_BUILD_TABLE_MAX_SIZE);
    assert(build_state->table_mr != NULL);

    build_state->linker_mr =
        acpi_add_rom_blob(build_state, tables.linker, "etc/table-loader", 0);

    fw_cfg_add_file(guest_info->fw_cfg, ACPI_BUILD_TPMLOG_FILE,
                    tables.tcpalog->data, acpi_data_len(tables.tcpalog));

    build_state->rsdp_mr = acpi_add_rom_blob(build_state, tables.rsdp,
                                              ACPI_BUILD_RSDP_FILE, 0);

    qemu_register_reset(virt_acpi_build_reset, build_state);
    virt_acpi_build_reset(build_state);
    vmstate_register(NULL, 0, &vmstate_virt_acpi_build, build_state);

    /* Cleanup tables but don't free the memory: we track it
     * in build_state.
     */
    acpi_build_tables_cleanup(&tables, false);
}
