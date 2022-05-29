"""
    scripts/gen_mapfiles.py
    ~~~~~~~~~~~~~~~~~~~~~~~

    Regenerate mapping files.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from importlib import import_module
from pathlib import Path
import re
import sys

top_src_dir = Path(__file__).parent.parent
pygments_package = top_src_dir / 'pygments'
sys.path.insert(0, pygments_package.parent.resolve())

from pygments.util import docstring_headline

for key in ['lexers', 'formatters']:
    lines = []
    for file in (pygments_package / key).glob('[!_]*.py'):
        module_name = '.'.join(file.relative_to(pygments_package.parent).with_suffix('').parts)
        print(module_name)
        module = import_module(module_name)
        for obj_name in module.__all__:
            obj = getattr(module, obj_name)
            desc = (module_name, obj.name, tuple(obj.aliases), tuple(obj.filenames))
            if key == 'lexers':
                desc += (tuple(obj.mimetypes),)
            else: # key == 'formatters'
                desc += (docstring_headline(obj),)
            lines.append(f'    {obj_name!r}: {desc!r},')
    # Sort to make diffs minimal.
    lines.sort()
    new_dict = '\n'.join(lines)
    content = f'''# Automatically generated by scripts/gen_mapfiles.py.
# DO NOT EDIT BY HAND; run `make mapfiles` instead.

{key.upper()} = {{
{new_dict}
}}
'''
    (pygments_package / key / '_mapping.py').write_text(content, encoding='utf8')
    print(f'=== {len(lines)} {key} processed.')