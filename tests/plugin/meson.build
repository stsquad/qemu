t = []
if get_option('plugins')
  foreach i : ['bb', 'empty', 'insn', 'mem', 'syscall']
    t += shared_module(i, files(i + '.c'),
                       include_directories: '../../include/qemu',
                       dependencies: glib)
  endforeach
  if 'cpp' in all_languages
    t += shared_module('cc', files('cc.cc'),
                       include_directories: '../../include/qemu',
                       dependencies: glib)
  endif
endif
if t.length() > 0
  alias_target('test-plugins', t)
else
  run_target('test-plugins', command: find_program('true'))
endif
