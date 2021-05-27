project('pygments', 'c',
  version : '0.1',
  default_options : ['warning_level=3']
)
import('fs')
builder = build_machine
target = target_machine
host = host_machine

# This is a comment
cc = meson.get_compiler('c')
cc.compiles(
'''
/*This
''
is a multiline string

# with an embedded line that looks like a comment

and some other elements like

true false
0 1
project()
*/
#include <stdio.h>
int main(void) {
  printf("hello\n");
}
'''
)
exe = executable('pygments', 'pygments.c', install : true)
dep = cc.find_library('foo', disabler: true)

foreach x : [42, 0x55000, 0o6, exe]
  if not is_disabler(dep)
    library('lib', 'lib.c', include_directories: [], dependencies: dep)
  elif false
    both_libraries('lib', 'lib2.c')
  else
    message('failed')
    continue
  endif
  break
endforeach

test('basic', exe)
subdir('dir')
if not get_option('build-docs')
  summary({'docs': [false, '(disabled by config)']}, section: 'Configuration')
  subdir_done()
endif
