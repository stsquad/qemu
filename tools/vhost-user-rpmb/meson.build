executable('vhost-user-rpmb', files(
  'main.c',
  'hmac_sha256.c'),
  dependencies: [qemuutil, glib, gio, vhost_user],
  install: true,
  install_dir: get_option('libexecdir'))

configure_file(input: '50-qemu-rpmb.json.in',
               output: '50-qemu-rpmb.json',
               configuration: { 'libexecdir' : get_option('prefix') / get_option('libexecdir') },
               install_dir: qemu_datadir / 'vhost-user')
