arm_softmmu_ss.add(when: 'CONFIG_TCG', if_true: files(
  'debug_helper.c',
  'mte_helper.c',
  'tcg-cpu.c',
  'tlb_helper.c',
))

arm_softmmu_ss.add(when: 'CONFIG_ARM_V7M', if_true: files(
  'm_helper.c',
))
