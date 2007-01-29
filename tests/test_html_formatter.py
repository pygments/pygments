# -*- coding: utf-8 -*-
"""
    Pygments HTML formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: 2006 by Georg Brandl.
    :license: BSD, see LICENSE for more details.
"""

import os
import unittest
import StringIO
import random

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
        
