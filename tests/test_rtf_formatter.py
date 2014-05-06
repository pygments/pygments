# -*- coding: utf-8 -*-
"""
    Pygments RTF formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest
from string_asserts import StringTests

from pygments.util import StringIO
from pygments.formatters import RtfFormatter
from pygments.lexers.special import TextLexer

class RtfFormatterTest(StringTests, unittest.TestCase):

    def format_rtf(self, t):
        tokensource = list(TextLexer().get_tokens(t))
        fmt = RtfFormatter()
        buf = StringIO()
        fmt.format(tokensource, buf)
        result = buf.getvalue()
        buf.close()
        return result

    def test_rtf_header(self):
        t = u''
        result = self.format_rtf(t)
        expected = r'{\rtf1\ansi\uc0'
        self.assertStartsWith(result, expected)

    def test_ascii_characters(self):
        t = u'a b c d ~'
        result = self.format_rtf(t)
        expected = (r'a b c d ~\par' '\n' r'}')
        self.assertEndsWith(result, expected)

    def test_escape_characters(self):
        t = u'\ {{'
        result = self.format_rtf(t)
        expected = (r'\\ \{\{\par' '\n' r'}')
        self.assertEndsWith(result, expected)

    def test_single_characters(self):
        t = u'â € ¤ каждой'
        result = self.format_rtf(t)
        expected = (r'{\u226} {\u8364} {\u164} '
                    r'{\u1082}{\u1072}{\u1078}{\u1076}{\u1086}{\u1081}'
                    r'\par' '\n' r'}')
        self.assertEndsWith(result, expected)

    def test_double_characters(self):
        t = u'က 힣 ↕ ↕︎ 鼖'
        result = self.format_rtf(t)
        expected = (r'{\u4096} {\u55203} {\u8597} '
                    r'{\u8597}{\u65038} {\u55422}{\u56859}'
                    r'\par' '\n' r'}')
        self.assertEndsWith(result, expected)
