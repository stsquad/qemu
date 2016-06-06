#include "qemu/osdep.h"
#include "qemu-common.h"
#include "sysemu/kvm.h"

int kvm_arch_irqchip_create(MachineState *ms, KVMState *s)
{
    return 0;
}

int kvm_arm_pmu_create(CPUState *cs, int irq)
{
    return 0;
}
