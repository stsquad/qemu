gen = [
  decodetree.process('sve.decode', extra_args: '--decode=disas_sve'),
  decodetree.process('neon-shared.decode', extra_args: '--static-decode=disas_neon_shared'),
  decodetree.process('neon-dp.decode', extra_args: '--static-decode=disas_neon_dp'),
  decodetree.process('neon-ls.decode', extra_args: '--static-decode=disas_neon_ls'),
  decodetree.process('vfp.decode', extra_args: '--static-decode=disas_vfp'),
  decodetree.process('vfp-uncond.decode', extra_args: '--static-decode=disas_vfp_uncond'),
  decodetree.process('m-nocp.decode', extra_args: '--static-decode=disas_m_nocp'),
  decodetree.process('a32.decode', extra_args: '--static-decode=disas_a32'),
  decodetree.process('a32-uncond.decode', extra_args: '--static-decode=disas_a32_uncond'),
  decodetree.process('t32.decode', extra_args: '--static-decode=disas_t32'),
  decodetree.process('t16.decode', extra_args: ['-w', '16', '--static-decode=disas_t16']),
]

arm_ss.add(when: 'CONFIG_TCG', if_true: gen)

arm_ss.add(when: 'CONFIG_TCG', if_true: files(
  'translate.c',
  'helper.c',
  'cpregs.c',
  'cpu-vfp.c',
  'iwmmxt_helper.c',
  'm_helper.c',
  'neon_helper.c',
  'op_helper.c',
  'tlb_helper.c',
  'vec_helper.c',
  'vfp_helper.c',
  'crypto_helper.c',
  'debug_helper.c',
  'tcg-cpu.c',
  'tcg-cpu-models.c',

), if_false: files(
  'tcg-stubs.c',
))

arm_ss.add(when: ['TARGET_AARCH64','CONFIG_TCG'], if_true: files(
  'translate-a64.c',
  'translate-sve.c',
  'helper-a64.c',
  'mte_helper.c',
  'pauth_helper.c',
  'sve_helper.c',
))

subdir('user')
subdir('sysemu')
