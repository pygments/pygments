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

from pygments import lexers, formatters
from pygments.token import _TokenType
from pygments.formatters import HtmlFormatter
from pygments.lexers import PythonLexer

class HtmlFormatterTest(unittest.TestCase):

# TODO: write this test.
#    def test_external_css(self):
#        pass

    def test_all_options(self):
        tokensource = list(PythonLexer().get_tokens(file(os.path.join(testdir, testfile)).read()))
        
        for optdict in [dict(nowrap=True),
                        dict(linenos=True),
                        dict(linenos=True, full=True),
                        dict(linenos=True, full=True, noclasses=True)]:
        
            outfile = StringIO.StringIO()
            fmt = HtmlFormatter(**optdict)
            fmt.format(tokensource, outfile)

    def test_valid_output(self):
        tokensource = list(PythonLexer().get_tokens(file(os.path.join(testdir, testfile)).read()))
        fmt = HtmlFormatter(full=True, linenos=True, noclasses=True)

        handle, pathname = tempfile.mkstemp('.html')
        # place all output files in /tmp too
        old_wd = os.getcwd()
        os.chdir(os.path.dirname(pathname))
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
        os.chdir(old_wd)
