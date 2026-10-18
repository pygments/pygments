"""
    pygments.modeline
    ~~~~~~~~~~~~~~~~~

    A simple modeline parser (based on pymodeline).

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

__all__ = ['get_filetype_from_buffer']


modeline_start_re = re.compile(r'''
    (?: vi | vim | ex ) (?: [<=>]? \d* )? :
''', re.VERBOSE)

modeline_re = re.compile(r'''
    (?: vi | vim | ex ) (?: [<=>]? \d* )? :
    .* (?: ft | filetype | syn | syntax ) = ( [^:\s]+ )
''', re.VERBOSE)


def get_filetype_from_line(l): # noqa: E741
    # Only try the leftmost modeline start: the ".*" can skip over any later
    # one, so searching from each of them would just repeat the same scan.
    start = modeline_start_re.search(l)
    if start:
        m = modeline_re.match(l, start.start())
        if m:
            return m.group(1)


def get_filetype_from_buffer(buf, max_lines=5):
    """
    Scan the buffer for modelines and return filetype if one is found.
    """
    lines = buf.splitlines()
    for line in lines[-1:-max_lines-1:-1]:
        ret = get_filetype_from_line(line)
        if ret:
            return ret
    for i in range(max_lines, -1, -1):
        if i < len(lines):
            ret = get_filetype_from_line(lines[i])
            if ret:
                return ret

    return None
