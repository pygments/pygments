# -*- coding: utf-8 -*-
"""
    Pygments IRC formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

from pygments.util import StringIO
from pygments.lexers import PythonLexer
from pygments.formatters import IRCFormatter

tokensource = list(PythonLexer().get_tokens("lambda x: 123"))


def test_correct_output():
    hfmt = IRCFormatter()
    houtfile = StringIO()
    hfmt.format(tokensource, houtfile)

    assert u'\x0302lambda\x03 x: \x0302123\x03\n' == houtfile.getvalue()
