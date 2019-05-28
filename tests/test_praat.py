# -*- coding: utf-8 -*-
"""
    Praat lexer tests
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Token
from pygments.lexers import PraatLexer

class PraatTest(unittest.TestCase):

    def setUp(self):
        self.lexer = PraatLexer()
        self.maxDiff = None

    def testNumericAssignment(self):
        fragment = u'var = -15e4\n'
        tokens = [
            (Token.Text, u'var'),
            (Token.Text, u' '),
            (Token.Operator, u'='),
            (Token.Text, u' '),
            (Token.Operator, u'-'),
            (Token.Literal.Number, u'15e4'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testStringAssignment(self):
        fragment = u'var$ = "foo"\n'
        tokens = [
            (Token.Text, u'var$'),
            (Token.Text, u' '),
            (Token.Operator, u'='),
            (Token.Text, u' '),
            (Token.Literal.String, u'"'),
            (Token.Literal.String, u'foo'),
            (Token.Literal.String, u'"'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testStringEscapedQuotes(self):
        fragment = u'"it said ""foo"""\n'
        tokens = [
            (Token.Literal.String, u'"'),
            (Token.Literal.String, u'it said '),
            (Token.Literal.String, u'"'),
            (Token.Literal.String, u'"'),
            (Token.Literal.String, u'foo'),
            (Token.Literal.String, u'"'),
            (Token.Literal.String, u'"'),
            (Token.Literal.String, u'"'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testFunctionCall(self):
        fragment = u'selected("Sound", i+(a*b))\n'
        tokens = [
            (Token.Name.Function, u'selected'),
            (Token.Punctuation, u'('),
            (Token.Literal.String, u'"'),
            (Token.Literal.String, u'Sound'),
            (Token.Literal.String, u'"'),
            (Token.Punctuation, u','),
            (Token.Text, u' '),
            (Token.Text, u'i'),
            (Token.Operator, u'+'),
            (Token.Text, u'('),
            (Token.Text, u'a'),
            (Token.Operator, u'*'),
            (Token.Text, u'b'),
            (Token.Text, u')'),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testBrokenUnquotedString(self):
        fragment = u'printline string\n... \'interpolated\' string\n'
        tokens = [
            (Token.Keyword, u'printline'),
            (Token.Text, u' '),
            (Token.Literal.String, u'string'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'...'),
            (Token.Text, u' '),
            (Token.Literal.String.Interpol, u"'"),
            (Token.Literal.String.Interpol, u'interpolated'),
            (Token.Literal.String.Interpol, u"'"),
            (Token.Text, u' '),
            (Token.Literal.String, u'string'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testInlinIf(self):
        fragment = u'var = if true == 1 then -1 else 0 fi'
        tokens = [
            (Token.Text, u'var'),
            (Token.Text, u' '),
            (Token.Operator, u'='),
            (Token.Text, u' '),
            (Token.Keyword, u'if'),
            (Token.Text, u' '),
            (Token.Text, u'true'),
            (Token.Text, u' '),
            (Token.Operator, u'=='),
            (Token.Text, u' '),
            (Token.Literal.Number, u'1'),
            (Token.Text, u' '),
            (Token.Keyword, u'then'),
            (Token.Text, u' '),
            (Token.Operator, u'-'),
            (Token.Literal.Number, u'1'),
            (Token.Text, u' '),
            (Token.Keyword, u'else'),
            (Token.Text, u' '),
            (Token.Literal.Number, u'0'),
            (Token.Text, u' '),
            (Token.Keyword, u'fi'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
