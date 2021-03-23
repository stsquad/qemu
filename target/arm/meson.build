arm_ss = ss.source_set()
arm_ss.add(files(
  'cpu.c',
  'cpu32.c',
  'gdbstub.c',
  'cpu-mmu.c',
  'cpregs.c',
  'cpustate-list.c',
  'cpu-common.c',
  'cpu-vfp.c',
))
arm_ss.add(zlib)

arm_ss.add(when: 'TARGET_AARCH64', if_true: files(
  'cpu64.c',
  'gdbstub64.c',
))

arm_softmmu_ss = ss.source_set()
arm_softmmu_ss.add(files(
  'arch_dump.c',
  'arm-powerctl.c',
  'machine.c',
  'monitor.c',
  'cpu-sysemu.c',
  'cpu-mmu-sysemu.c',
))

arm_softmmu_ss.add(when: 'CONFIG_TCG', if_true: files(
  'psci.c',
))

arm_user_ss = ss.source_set()
arm_user_ss.add(files(
  'cpu-user.c',
))

subdir('tcg')
subdir('kvm')

target_arch += {'arm': arm_ss}
target_softmmu_arch += {'arm': arm_softmmu_ss}
target_user_arch += {'arm': arm_user_ss}
