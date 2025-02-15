#!/usr/bin/env python
"""
    Checker for file headers
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Make sure each Python file has a correct file header
    including copyright and license information.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import io
import os
import re
import sys
import getopt
from os.path import join, splitext, abspath


checkers = {}


def checker(*suffixes, **kwds):
    only_pkg = kwds.pop('only_pkg', False)

    def deco(func):
        for suffix in suffixes:
            checkers.setdefault(suffix, []).append(func)
        func.only_pkg = only_pkg
        return func
    return deco


name_mail_re = r'[\w ]+(<.*?>)?'
copyright_re = re.compile(r'^    :copyright: Copyright 2006-2025 by '
                          r'the Pygments team, see AUTHORS\.$')
copyright_2_re = re.compile(rf'^                {name_mail_re}(, {name_mail_re})*[,.]$')
is_const_re = re.compile(r'if.*?==\s+(None|False|True)\b')

misspellings = ["developement", "adress", "verificate",  # ALLOW-MISSPELLING
                "informations", "unlexer"]               # ALLOW-MISSPELLING


@checker('.py')
def check_syntax(fn, lines):
    if not lines:
        yield 0, "empty file"
        return
    if '#!/' in lines[0]:
        lines = lines[1:]
    try:
        compile('\n'.join(lines), fn, "exec")
    except SyntaxError as err:
        yield 0, f"not compilable: {err}"


@checker('.py')
def check_style_and_encoding(fn, lines):
    for lno, line in enumerate(lines):
        if is_const_re.search(line):
            yield lno+1, 'using == None/True/False'


@checker('.py', only_pkg=True)
def check_fileheader(fn, lines):
    # line number correction
    c = 1
    if lines[0:1] == ['#!/usr/bin/env python']:
        lines = lines[1:]
        c = 2

    llist = []
    docopen = False
    for lno, line in enumerate(lines):
        llist.append(line)
        if lno == 0:
            if line != '"""' and line != 'r"""':
                yield 2, f'missing docstring begin ("""), found {line!r}'
            else:
                docopen = True
        elif docopen:
            if line == '"""':
                # end of docstring
                if lno <= 3:
                    yield lno+c, "missing module name in docstring"
                break

            if line != "" and line[:4] != '    ' and docopen:
                yield lno+c, "missing correct docstring indentation"

            if lno == 1:
                # if not in package, don't check the module name
                modname = fn[:-3].replace('/', '.').replace('.__init__', '')
                while modname:
                    if line.lower()[4:] == modname:
                        break
                    modname = '.'.join(modname.split('.')[1:])
                else:
                    yield 3, "wrong module name in docstring heading"
                modnamelen = len(line.strip())
            elif lno == 2:
                if line.strip() != modnamelen * "~":
                    yield 4, "wrong module name underline, should be ~~~...~"

    else:
        yield 0, "missing end and/or start of docstring..."

    # check for copyright and license fields
    license = llist[-2:-1]
    if license != ["    :license: BSD, see LICENSE for details."]:
        yield 0, "no correct license info"

    ci = -3
    copyright = llist[ci:ci+1]
    while copyright and copyright_2_re.match(copyright[0]):
        ci -= 1
        copyright = llist[ci:ci+1]
    if not copyright or not copyright_re.match(copyright[0]):
        yield 0, "no correct copyright info"


def main(argv):
    try:
        gopts, args = getopt.getopt(argv[1:], "vi:")
    except getopt.GetoptError:
        print(f"Usage: {argv[0]} [-v] [-i ignorepath]* [path]")
        return 2
    opts = {}
    for opt, val in gopts:
        if opt == '-i':
            val = abspath(val)
        opts.setdefault(opt, []).append(val)

    if len(args) == 0:
        path = '.'
    elif len(args) == 1:
        path = args[0]
    else:
        print(f"Usage: {argv[0]} [-v] [-i ignorepath]* [path]")
        return 2

    verbose = '-v' in opts

    num = 0
    out = io.StringIO()

    for root, dirs, files in os.walk(path):
        for excl in ['.tox', '.git', '.venv', 'examplefiles']:
            if excl in dirs:
                dirs.remove(excl)
        if '-i' in opts and abspath(root) in opts['-i']:
            del dirs[:]
            continue
        # XXX: awkward: for the Makefile call: don't check non-package
        #      files for file headers
        in_pygments_pkg = root.startswith('./pygments')
        for fn in files:

            fn = join(root, fn)
            if fn[:2] == './':
                fn = fn[2:]

            if '-i' in opts and abspath(fn) in opts['-i']:
                continue

            ext = splitext(fn)[1]
            checkerlist = checkers.get(ext, None)
            if not checkerlist:
                continue

            if verbose:
                print(f"Checking {fn}...")

            try:
                with open(fn, 'rb') as f:
                    lines = f.read().decode('utf-8').splitlines()
            except OSError as err:
                print(f"{fn}: cannot open: {err}")
                num += 1
                continue
            except UnicodeDecodeError as err:
                print(f"{fn}: error decoding: {err}")
                num += 1
                continue

            for checker in checkerlist:
                if not in_pygments_pkg and checker.only_pkg:
                    continue
                for lno, msg in checker(fn, lines):
                    print('%s:%d: %s' % (fn, lno, msg), file=out)
                    num += 1
    if verbose:
        print()
    if num == 0:
        print("No errors found.")
    else:
        print(out.getvalue().rstrip('\n'))
        print("%d error%s found." % (num, num > 1 and "s" or ""))
    return int(num > 0)


if __name__ == '__main__':
    sys.exit(main(sys.argv))
