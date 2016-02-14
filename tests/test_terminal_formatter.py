# -*- coding: utf-8 -*-
"""
    Pygments terminal formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import unittest
import sys
import re

from pygments.util import StringIO
from pygments.lexers.sql import PlPgsqlLexer
from pygments.formatters import TerminalFormatter,Terminal256Formatter, HtmlFormatter, LatexFormatter

from pygments.style import Style
from pygments.token import Token
from pygments.lexers import Python3Lexer
from pygments import highlight

DEMO_TEXT = '''\
-- comment
select
* from bar;
'''
DEMO_LEXER = PlPgsqlLexer
DEMO_TOKENS = list(DEMO_LEXER().get_tokens(DEMO_TEXT))

ANSI_RE = re.compile(r'\x1b[\w\W]*?m')

def strip_ansi(x):
    return ANSI_RE.sub('', x)

class TerminalFormatterTest(unittest.TestCase):
    def test_reasonable_output(self):
        out = StringIO()
        TerminalFormatter().format(DEMO_TOKENS, out)
        plain = strip_ansi(out.getvalue())
        self.assertEqual(DEMO_TEXT.count('\n'), plain.count('\n'))
        print(repr(plain))

        for a, b in zip(DEMO_TEXT.splitlines(), plain.splitlines()):
            self.assertEqual(a, b)

    def test_reasonable_output_lineno(self):
        out = StringIO()
        TerminalFormatter(linenos=True).format(DEMO_TOKENS, out)
        plain = strip_ansi(out.getvalue())
        self.assertEqual(DEMO_TEXT.count('\n') + 1, plain.count('\n'))
        print(repr(plain))

        for a, b in zip(DEMO_TEXT.splitlines(), plain.splitlines()):
            self.assertTrue(a in b)






class MyStyle(Style):

    styles = {
        Token.Comment:    '#ansidarkgray',
        Token.String:     '#ansiblue bg:#ansidarkred', 
        Token.Number    : '#ansigreen bg:#ansidarkgreen', 
        Token.Number.Hex: '#ansidarkgreen bg:#ansired', 
    }



code = '''
# this should be a comment
print("Hello World")
async def function(a,b,c, *d, **kwarg:Bool)->Bool:
    pass
    return 123, 0xb3e3

'''

class MyTest(unittest.TestCase):

    def assertIn(self, member, container, msg=None):
        """Just like self.assertTrue(a in b), but with a nicer default message."""
        if member not in container:
            standardMsg = '%s not found in %s' % (safe_repr(member),
                                                  safe_repr(container))
            self.fail(self._formatMessage(msg, standardMsg))


termtest = lambda x: highlight(x, Python3Lexer(), Terminal256Formatter(style=MyStyle))
class Terminal256FormatterTest(MyTest):


    def test_style_html(self):
        style = HtmlFormatter(style=MyStyle).get_style_defs()
        self.assertIn('#555555',style, "ansigray for comment not html css style")

    def test_tex_works(self):
        """check tex Formatter don't crash"""
        highlight(code, Python3Lexer(), LatexFormatter(style=MyStyle))

    def test_html_works(self):
        highlight(code, Python3Lexer(), HtmlFormatter(style=MyStyle))

    def test_256esc_seq(self):
        """
        test that a few escape sequences are actualy used when using #ansi<> color codes
        """
        self.assertIn('32;41',termtest('0x123'))
        self.assertIn('32;42',termtest('123'))
        self.assertIn('30;01',termtest('#comment'))
        self.assertIn('34;41',termtest('"String"'))

        
