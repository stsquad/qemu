subdir('qemu-api-macros')
subdir('qemu-api')

subdir('hw')

cargo = find_program('cargo')

run_target('clippy',
  command: [config_host['MESON'], 'devenv',
            '--workdir', '@CURRENT_SOURCE_DIR@',
            cargo, 'clippy', '--tests'],
  depends: bindings_rs)

run_target('rustfmt',
  command: [config_host['MESON'], 'devenv',
            '--workdir', '@CURRENT_SOURCE_DIR@',
            cargo, 'fmt'],
  depends: bindings_rs)
