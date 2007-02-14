# -*- coding: utf-8 -*-
"""
    Pygments HTML formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: 2006-2007 by Georg Brandl.
    :license: BSD, see LICENSE for more details.
"""

import os
import unittest
import StringIO
import random
import tempfile
from os.path import join, dirname, isfile

from pygments import lexers, formatters
from pygments.token import _TokenType
from pygments.formatters import HtmlFormatter
from pygments.lexers import PythonLexer

tokensource = list(PythonLexer().get_tokens(file(os.path.join(testdir, testfile)).read()))

class HtmlFormatterTest(unittest.TestCase):

    def test_external_css(self):
        # test correct behavior
        # CSS should be in /tmp directory
        fmt1 = HtmlFormatter(full=True, cssfile='fmt1.css')
        # CSS should be in testdir (testdir is absolute)
        fmt2 = HtmlFormatter(full=True, cssfile=join(testdir, 'fmt2.css'))
        tfile = tempfile.NamedTemporaryFile(suffix='.html')
        fmt1.format(tokensource, tfile)
        try:
            fmt2.format(tokensource, tfile)
        except IOError:
            # test directory not writable
            pass
        tfile.close()

        self.assert_(isfile(join(dirname(tfile.name), 'my.css')))

        os.unlink(join(dirname(tfile.name), 'fmt1.css'))
        try:
            os.unlink(join(testdir, 'fmt2.css'))
        except OSError:
            pass


    def test_all_options(self):
        for optdict in [dict(nowrap=True),
                        dict(linenos=True),
                        dict(linenos=True, full=True),
                        dict(linenos=True, full=True, noclasses=True)]:
        
            outfile = StringIO.StringIO()
            fmt = HtmlFormatter(**optdict)
            fmt.format(tokensource, outfile)

    def test_valid_output(self):
        # test all available wrappers
        fmt = HtmlFormatter(full=True, linenos=True, noclasses=True)

        handle, pathname = tempfile.mkstemp('.html')
        tfile = os.fdopen(handle, 'w+b')
        fmt.format(tokensource, tfile)
        tfile.close()
        catname = os.path.join(testdir, 'dtds', 'HTML4.soc')
        try:
            try:
                import subprocess
                ret = subprocess.Popen(['nsgmls', '-s', '-c', catname, pathname],
                                       stdout=subprocess.PIPE).wait()
            except ImportError:
                # Python 2.3 - no subprocess module
                ret = os.popen('nsgmls -s -c "%s" "%s"' % (catname, pathname)).close()
                if ret == 32512: raise OSError  # not found
        except OSError:
            # latex not available
            pass
        else:
            self.failIf(ret, 'nsgmls run reported errors')

        os.unlink(pathname)
