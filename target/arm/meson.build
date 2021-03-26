arm_ss = ss.source_set()
arm_ss.add(files(
  'cpregs.c',
  'cpu.c',
  'cpu32.c',
  'cpu-common.c',
  'cpu-mmu.c',
  'cpu-vfp.c',
  'cpustate-list.c',
  'gdbstub.c',
))
arm_ss.add(zlib)

arm_ss.add(when: 'CONFIG_KVM', if_true: files('kvm.c', 'kvm64.c'), if_false: files('kvm-stub.c'))

arm_ss.add(when: 'TARGET_AARCH64', if_true: files(
  'cpu64.c',
  'gdbstub64.c',
))

arm_softmmu_ss = ss.source_set()
arm_softmmu_ss.add(files(
  'arch_dump.c',
  'arm-powerctl.c',
  'cpu-mmu-sysemu.c',
  'cpu-sysemu.c',
  'machine.c',
  'monitor.c',
))

arm_softmmu_ss.add(when: 'CONFIG_TCG', if_true: files(
  'psci.c',
))

arm_user_ss = ss.source_set()
arm_user_ss.add(files(
  'cpu-user.c',
))

subdir('tcg')

target_arch += {'arm': arm_ss}
target_softmmu_arch += {'arm': arm_softmmu_ss}
target_user_arch += {'arm': arm_user_ss}
