# -*- coding: utf-8 -*-
"""
    Command line test
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import io
import os
import re
import sys
import tempfile
from io import BytesIO
from os import path

from pytest import raises

from pygments import cmdline, highlight

TESTDIR = path.dirname(path.abspath(__file__))
TESTFILE = path.join(TESTDIR, 'test_cmdline.py')

TESTCODE = '''\
def func(args):
    pass
'''


def _decode_output(text):
    try:
        return text.decode('utf-8')
    except UnicodeEncodeError:  # implicit encode on Python 2 with data loss
        return text


def run_cmdline(*args, **kwds):
    saved_stdin = sys.stdin
    saved_stdout = sys.stdout
    saved_stderr = sys.stderr
    stdin_buffer = BytesIO()
    stdout_buffer = BytesIO()
    stderr_buffer = BytesIO()
    new_stdin = sys.stdin = io.TextIOWrapper(stdin_buffer, 'utf-8')
    new_stdout = sys.stdout = io.TextIOWrapper(stdout_buffer, 'utf-8')
    new_stderr = sys.stderr = io.TextIOWrapper(stderr_buffer, 'utf-8')
    new_stdin.write(kwds.get('stdin', ''))
    new_stdin.seek(0, 0)
    try:
        ret = cmdline.main(['pygmentize'] + list(args))
    finally:
        sys.stdin = saved_stdin
        sys.stdout = saved_stdout
        sys.stderr = saved_stderr
    new_stdout.flush()
    new_stderr.flush()
    out, err = stdout_buffer.getvalue(), \
        stderr_buffer.getvalue()
    return (ret, _decode_output(out), _decode_output(err))


def check_success(*cmdline, **kwds):
    code, out, err = run_cmdline(*cmdline, **kwds)
    assert code == 0
    assert err == ''
    return out


def check_failure(*cmdline, **kwds):
    expected_code = kwds.pop('code', 1)
    code, out, err = run_cmdline(*cmdline, **kwds)
    assert code == expected_code
    assert out == ''
    return err


def test_normal():
    # test that cmdline gives the same output as library api
    from pygments.lexers import PythonLexer
    from pygments.formatters import HtmlFormatter
    filename = TESTFILE
    with open(filename, 'rb') as fp:
        code = fp.read()

    output = highlight(code, PythonLexer(), HtmlFormatter())

    o = check_success('-lpython', '-fhtml', filename)
    assert o == output


def test_stdin():
    o = check_success('-lpython', '-fhtml', stdin=TESTCODE)
    o = re.sub('<[^>]*>', '', o)
    # rstrip is necessary since HTML inserts a \n after the last </div>
    assert o.rstrip() == TESTCODE.rstrip()

    # guess if no lexer given
    o = check_success('-fhtml', stdin=TESTCODE)
    o = re.sub('<[^>]*>', '', o)
    # rstrip is necessary since HTML inserts a \n after the last </div>
    assert o.rstrip() == TESTCODE.rstrip()


def test_outfile():
    # test that output file works with and without encoding
    fd, name = tempfile.mkstemp()
    os.close(fd)
    for opts in [['-fhtml', '-o', name, TESTFILE],
                 ['-flatex', '-o', name, TESTFILE],
                 ['-fhtml', '-o', name, '-O', 'encoding=utf-8', TESTFILE]]:
        try:
            check_success(*opts)
        finally:
            os.unlink(name)


def test_load_from_file():
    lexer_file = os.path.join(TESTDIR, 'support', 'python_lexer.py')
    formatter_file = os.path.join(TESTDIR, 'support', 'html_formatter.py')

    # By default, use CustomLexer
    o = check_success('-l', lexer_file, '-f', 'html', '-x', stdin=TESTCODE)
    o = re.sub('<[^>]*>', '', o)
    # rstrip is necessary since HTML inserts a \n after the last </div>
    assert o.rstrip() == TESTCODE.rstrip()

    # If user specifies a name, use it
    o = check_success('-f', 'html', '-x', '-l',
                      lexer_file + ':LexerWrapper', stdin=TESTCODE)
    o = re.sub('<[^>]*>', '', o)
    # rstrip is necessary since HTML inserts a \n after the last </div>
    assert o.rstrip() == TESTCODE.rstrip()

    # Should also work for formatters
    o = check_success('-lpython', '-f',
                      formatter_file + ':HtmlFormatterWrapper',
                      '-x', stdin=TESTCODE)
    o = re.sub('<[^>]*>', '', o)
    # rstrip is necessary since HTML inserts a \n after the last </div>
    assert o.rstrip() == TESTCODE.rstrip()


def test_stream_opt():
    o = check_success('-lpython', '-s', '-fterminal', stdin=TESTCODE)
    o = re.sub(r'\x1b\[.*?m', '', o)
    assert o.replace('\r\n', '\n') == TESTCODE


def test_h_opt():
    o = check_success('-h')
    assert 'Usage:' in o


def test_L_opt():
    o = check_success('-L')
    assert 'Lexers' in o and 'Formatters' in o and 'Filters' in o and 'Styles' in o
    o = check_success('-L', 'lexer')
    assert 'Lexers' in o and 'Formatters' not in o
    check_success('-L', 'lexers')


def test_O_opt():
    filename = TESTFILE
    o = check_success('-Ofull=1,linenos=true,foo=bar', '-fhtml', filename)
    assert '<html' in o
    assert 'class="linenos"' in o

    # "foobar" is invalid for a bool option
    e = check_failure('-Ostripnl=foobar', TESTFILE)
    assert 'Error: Invalid value' in e
    e = check_failure('-Ostripnl=foobar', '-lpy')
    assert 'Error: Invalid value' in e


def test_P_opt():
    filename = TESTFILE
    o = check_success('-Pfull', '-Ptitle=foo, bar=baz=,', '-fhtml', filename)
    assert '<title>foo, bar=baz=,</title>' in o


def test_F_opt():
    filename = TESTFILE
    o = check_success('-Fhighlight:tokentype=Name.Blubb,'
                      'names=TESTFILE filename', '-fhtml', filename)
    assert '<span class="n n-Blubb' in o


def test_H_opt():
    o = check_success('-H', 'formatter', 'html')
    assert 'HTML' in o
    o = check_success('-H', 'lexer', 'python')
    assert 'Python' in o
    o = check_success('-H', 'filter', 'raiseonerror')
    assert 'raiseonerror' in o
    e = check_failure('-H', 'lexer', 'foobar')
    assert 'not found' in e


def test_S_opt():
    o = check_success('-S', 'default', '-f', 'html', '-O', 'linenos=1')
    lines = o.splitlines()
    for line in lines[5:]:
        # every line is for a token class, except for the first 5 lines,
        # which define styles for `pre` and line numbers
        parts = line.split()
        assert parts[0].startswith('.')
        assert parts[1] == '{'
        if parts[0] != '.hll':
            assert parts[-4] == '}'
            assert parts[-3] == '/*'
            assert parts[-1] == '*/'
    check_failure('-S', 'default', '-f', 'foobar')


def test_N_opt():
    o = check_success('-N', 'test.py')
    assert 'python' == o.strip()
    o = check_success('-N', 'test.unknown')
    assert 'text' == o.strip()


def test_invalid_opts():
    for opts in [
        ('-X',),
        ('-L', '-lpy'),
        ('-L', '-fhtml'),
        ('-L', '-Ox'),
        ('-S', 'default', '-l', 'py', '-f', 'html'),
        ('-S', 'default'),
        ('-a', 'arg'),
        ('-H',),
        (TESTFILE, TESTFILE),
        ('-H', 'formatter'),
        ('-H', 'foo', 'bar'),
        ('-s',),
        ('-s', TESTFILE),
    ]:
        check_failure(*opts, code=2)


def test_errors():
    # input file not found
    e = check_failure('-lpython', 'nonexistent.py')
    assert 'Error: cannot read infile' in e
    assert 'nonexistent.py' in e

    # lexer not found
    e = check_failure('-lfooo', TESTFILE)
    assert 'Error: no lexer for alias' in e

    # cannot load .py file without load_from_file flag
    e = check_failure('-l', 'nonexistent.py', TESTFILE)
    assert 'Error: no lexer for alias' in e

    # lexer file is missing/unreadable
    e = check_failure('-l', 'nonexistent.py', '-x', TESTFILE)
    assert 'Error: cannot read' in e

    # lexer file is malformed
    e = check_failure('-l', path.join(TESTDIR, 'support', 'empty.py'),
                      '-x', TESTFILE)
    assert 'Error: no valid CustomLexer class found' in e

    # formatter not found
    e = check_failure('-lpython', '-ffoo', TESTFILE)
    assert 'Error: no formatter found for name' in e

    # formatter for outfile not found
    e = check_failure('-ofoo.foo', TESTFILE)
    assert 'Error: no formatter found for file name' in e

    # cannot load .py file without load_from_file flag
    e = check_failure('-f', 'nonexistent.py', TESTFILE)
    assert 'Error: no formatter found for name' in e

    # formatter file is missing/unreadable
    e = check_failure('-f', 'nonexistent.py', '-x', TESTFILE)
    assert 'Error: cannot read' in e

    # formatter file is malformed
    e = check_failure('-f', path.join(TESTDIR, 'support', 'empty.py'),
                      '-x', TESTFILE)
    assert 'Error: no valid CustomFormatter class found' in e

    # output file not writable
    e = check_failure('-o', os.path.join('nonexistent', 'dir', 'out.html'),
                      '-lpython', TESTFILE)
    assert 'Error: cannot open outfile' in e
    assert 'out.html' in e

    # unknown filter
    e = check_failure('-F', 'foo', TESTFILE)
    assert 'Error: filter \'foo\' not found' in e


def test_exception():
    cmdline.highlight = None  # override callable to provoke TypeError
    try:
        # unexpected exception while highlighting
        e = check_failure('-lpython', TESTFILE)
        assert '*** Error while highlighting:' in e
        assert 'TypeError' in e

        # same with -v: should reraise the exception
        assert raises(Exception, check_failure, '-lpython', '-v', TESTFILE)
    finally:
        cmdline.highlight = highlight


def test_parse_opts():
    assert cmdline._parse_options(['  ', 'keyonly,key = value ']) == \
        {'keyonly': True, 'key': 'value'}
