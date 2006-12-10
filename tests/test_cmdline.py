# -*- coding: utf-8 -*-
"""
    Command line test
    ~~~~~~~~~~~~~~~~~

    :copyright: 2006 by Georg Brandl.
    :license: BSD, see LICENSE for more details.
"""

# Test the command line interface

import sys, os
import unittest
import StringIO
from pygments import cmdline_main, highlight


def run_cmdline(*args):
    saved_stdout = sys.stdout
    saved_stderr = sys.stderr
    new_stdout = sys.stdout = StringIO.StringIO()
    new_stderr = sys.stderr = StringIO.StringIO()
    try:
        ret = cmdline_main(["pygmentize"] + list(args))
    finally:
        sys.stdout = saved_stdout
        sys.stderr = saved_stderr
    return (ret, new_stdout.getvalue(), new_stderr.getvalue())


class CmdLineTest(unittest.TestCase):

    def test_L_opt(self):
        c, o, e = run_cmdline("-L")
        self.assert_(c == 0)
        self.assert_(o.find("Lexers") and o.find("Formatters"))

    def test_invalid_opts(self):
        for opts in [("-L", "-lpy"), ("-L", "-fhtml"), ("-L", "-Ox"),
                     ("-a",), ("-Sst", "-lpy")]:
            self.assert_(run_cmdline(*opts)[0] == 2)

    def test_normal(self):
        # test that cmdline gives the same output as library api
        from pygments.lexers import PythonLexer
        from pygments.formatters import HtmlFormatter
        filename = os.path.join(testdir, testfile)
        code = file(filename).read()

        output = highlight(code, PythonLexer(), HtmlFormatter())

        c, o, e = run_cmdline("-lpython", "-fhtml", filename)

        self.assertEquals(o, output)
        self.assertEquals(e, "")
        self.assertEquals(c, 0)


if __name__ == '__main__':
    unittest.main()
