# -*- coding: utf-8 -*-
"""
    Command line test
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import io
import os
import sys
import tempfile
import unittest

from pygments import highlight, cmdline
from pygments.util import StringIO, BytesIO

import support

TESTFILE, TESTDIR = support.location(__file__)


def run_cmdline(*args):
    saved_stdout = sys.stdout
    saved_stderr = sys.stderr
    if sys.version_info > (3,):
        stdout_buffer = BytesIO()
        stderr_buffer = BytesIO()
        new_stdout = sys.stdout = io.TextIOWrapper(stdout_buffer, 'utf-8')
        new_stderr = sys.stderr = io.TextIOWrapper(stderr_buffer, 'utf-8')
    else:
        stdout_buffer = new_stdout = sys.stdout = StringIO()
        stderr_buffer = new_stderr = sys.stderr = StringIO()
    try:
        ret = cmdline.main(["pygmentize"] + list(args))
    finally:
        sys.stdout = saved_stdout
        sys.stderr = saved_stderr
    new_stdout.flush()
    new_stderr.flush()
    out, err = stdout_buffer.getvalue().decode('utf-8'), \
        stderr_buffer.getvalue().decode('utf-8')
    return (ret, out, err)


class CmdLineTest(unittest.TestCase):

    def test_L_opt(self):
        c, o, e = run_cmdline("-L")
        self.assertEqual(c, 0)
        self.assertTrue("Lexers" in o and "Formatters" in o and
                        "Filters" in o and "Styles" in o)
        c, o, e = run_cmdline("-L", "lexer")
        self.assertEqual(c, 0)
        self.assertTrue("Lexers" in o and "Formatters" not in o)
        c, o, e = run_cmdline("-L", "lexers")
        self.assertEqual(c, 0)

    def test_O_opt(self):
        filename = TESTFILE
        c, o, e = run_cmdline("-Ofull=1,linenos=true,foo=bar",
                              "-fhtml", filename)
        self.assertEqual(c, 0)
        self.assertTrue("<html" in o)
        self.assertTrue('class="linenos"' in o)

    def test_P_opt(self):
        filename = TESTFILE
        c, o, e = run_cmdline("-Pfull", "-Ptitle=foo, bar=baz=,",
                              "-fhtml", filename)
        self.assertEqual(c, 0)
        self.assertTrue("<title>foo, bar=baz=,</title>" in o)

    def test_F_opt(self):
        filename = TESTFILE
        c, o, e = run_cmdline("-Fhighlight:tokentype=Name.Blubb,"
                              "names=TESTFILE filename",
                              "-fhtml", filename)
        self.assertEqual(c, 0)
        self.assertTrue('<span class="n-Blubb' in o)

    def test_H_opt(self):
        c, o, e = run_cmdline("-H", "formatter", "html")
        self.assertEqual(c, 0)
        self.assertTrue('HTML' in o)

    def test_S_opt(self):
        c, o, e = run_cmdline("-S", "default", "-f", "html", "-O", "linenos=1")
        self.assertEqual(c, 0)

    def test_invalid_opts(self):
        for opts in [("-L", "-lpy"), ("-L", "-fhtml"), ("-L", "-Ox"),
                     ("-a",), ("-Sst", "-lpy"), ("-H",),
                     ("-H", "formatter")]:
            self.assertTrue(run_cmdline(*opts)[0] == 2)

    def test_normal(self):
        # test that cmdline gives the same output as library api
        from pygments.lexers import PythonLexer
        from pygments.formatters import HtmlFormatter
        filename = TESTFILE
        with open(filename, 'rb') as fp:
            code = fp.read()

        output = highlight(code, PythonLexer(), HtmlFormatter())

        c, o, e = run_cmdline("-lpython", "-fhtml", filename)

        self.assertEqual(o, output)
        self.assertEqual(e, "")
        self.assertEqual(c, 0)

    def test_outfile(self):
        # test that output file works with and without encoding
        fd, name = tempfile.mkstemp()
        os.close(fd)
        for opts in [['-fhtml', '-o', name, TESTFILE],
                     ['-flatex', '-o', name, TESTFILE],
                     ['-fhtml', '-o', name, '-O', 'encoding=utf-8', TESTFILE]]:
            try:
                self.assertEqual(run_cmdline(*opts)[0], 0)
            finally:
                os.unlink(name)

    def check_failure(self, *cmdline):
        c, o, e = run_cmdline(*cmdline)
        self.assertEqual(c, 1)
        self.assertEqual(o, '')
        return e

    def test_errors(self):
        # input file not found
        e = self.check_failure('-lpython', 'nonexistent.py')
        self.assertTrue('Error: cannot read infile' in e)
        self.assertTrue('nonexistent.py' in e)

        # lexer not found
        e = self.check_failure('-lfooo', TESTFILE)
        self.assertTrue('Error: no lexer for alias' in e)

        # formatter not found
        e = self.check_failure('-lpython', '-ffoo', TESTFILE)
        self.assertTrue('Error: no formatter found for name' in e)

        # output file not writable
        e = self.check_failure('-o', os.path.join('nonexistent', 'dir', 'out.html'),
                               '-lpython', TESTFILE)
        self.assertTrue('Error: cannot open outfile' in e)
        self.assertTrue('out.html' in e)

        # unknown filter
        e = self.check_failure('-F', 'foo', TESTFILE)
        self.assertTrue('Error: filter \'foo\' not found' in e)

    def test_exception(self):
        # unexpected exception while highlighting
        cmdline.highlight = None  # override callable
        try:
            e = self.check_failure('-lpython', TESTFILE)
        finally:
            cmdline.highlight = highlight
        self.assertTrue('*** Error while highlighting:' in e)
