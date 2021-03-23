arm_ss.add(when: 'CONFIG_TCG', if_false: files(
  'helper-stubs.c',
))

arm_ss.add(when: 'CONFIG_KVM', if_true: files(
  'kvm.c',
  'kvm64.c',
))