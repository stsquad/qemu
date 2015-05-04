/*
 *
 * Copyright (c) 2015 HUAWEI TECHNOLOGIES CO.,LTD.
 *
 * Author: Shannon Zhao <zhaoshenglong@huawei.com>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms and conditions of the GNU General Public License,
 * version 2 or later, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef QEMU_VIRT_ACPI_BUILD_H
#define QEMU_VIRT_ACPI_BUILD_H

#include "qemu-common.h"

#define VIRT_ACPI_CPU_ID_LIMIT 8
#define ACPI_GICC_ENABLED 1

typedef struct MemMap {
    hwaddr addr;
    hwaddr size;
} MemMap;

typedef struct AcpiGtdtInfo {
    uint32_t timer_virt;
    uint32_t timer_s_el1;
    uint32_t timer_ns_el1;
    uint32_t timer_ns_el2;
} AcpiGtdtInfo;

typedef struct AcpiMadtInfo {
    const MemMap *gic_cpu_memmap;
    const MemMap *gic_dist_memmap;
} AcpiMadtInfo;

typedef struct AcpiDsdtInfo {
    const MemMap *uart_memmap;
    const int *uart_irq;
    const MemMap *virtio_mmio_memmap;
    const int *virtio_mmio_irq;
    int virtio_mmio_num;
    const MemMap *rtc_memmap;
    const int *rtc_irq;
    const MemMap *flash_memmap;
} AcpiDsdtInfo;

typedef struct AcpiPcieInfo {
    const int *pcie_irq;
    MemMap pcie_mmio;
    MemMap pcie_ioport;
    MemMap pcie_ecam;
    int nr_pcie_buses;
} AcpiPcieInfo;

typedef struct VirtGuestInfo {
    int smp_cpus;
    int max_cpus;
    FWCfgState *fw_cfg;
    AcpiMadtInfo *madt_info;
    AcpiDsdtInfo *dsdt_info;
    AcpiGtdtInfo *gtdt_info;
    AcpiPcieInfo *pcie_info;
} VirtGuestInfo;


typedef struct VirtGuestInfoState {
    VirtGuestInfo info;
    Notifier machine_done;
} VirtGuestInfoState;

void virt_acpi_setup(VirtGuestInfo *guest_info);

#endif
