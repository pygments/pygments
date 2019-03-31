# -*- coding: utf-8 -*-
"""
    R Tests
    ~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexers import SLexer
from pygments.token import Token, Name, Punctuation


class RTest(unittest.TestCase):
    def setUp(self):
        self.lexer = SLexer()

    def testCall(self):
        fragment = u'f(1, a)\n'
        tokens = [
            (Name.Function, u'f'),
            (Punctuation, u'('),
            (Token.Literal.Number, u'1'),
            (Punctuation, u','),
            (Token.Text, u' '),
            (Token.Name, u'a'),
            (Punctuation, u')'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testName1(self):
        fragment = u'._a_2.c'
        tokens = [
            (Name, u'._a_2.c'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testName2(self):
        # Invalid names are valid if backticks are used
        fragment = u'`.1 blah`'
        tokens = [
            (Name, u'`.1 blah`'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testName3(self):
        # Internal backticks can be escaped
        fragment = u'`.1 \\` blah`'
        tokens = [
            (Name, u'`.1 \\` blah`'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testCustomOperator(self):
        fragment = u'7 % and % 8'
        tokens = [
            (Token.Literal.Number, u'7'),
            (Token.Text, u' '),
            (Token.Operator, u'% and %'),
            (Token.Text, u' '),
            (Token.Literal.Number, u'8'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
