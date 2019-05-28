# -*- coding: utf-8 -*-
"""
    Python Tests
    ~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexers import PythonLexer, Python3Lexer
from pygments.token import Token


class PythonTest(unittest.TestCase):
    def setUp(self):
        self.lexer = PythonLexer()

    def test_cls_builtin(self):
        """
        Tests that a cls token gets interpreted as a Token.Name.Builtin.Pseudo

        """
        fragment = 'class TestClass():\n    @classmethod\n    def hello(cls):\n        pass\n'
        tokens = [
            (Token.Keyword, 'class'),
            (Token.Text, ' '),
            (Token.Name.Class, 'TestClass'),
            (Token.Punctuation, '('),
            (Token.Punctuation, ')'),
            (Token.Punctuation, ':'),
            (Token.Text, '\n'),
            (Token.Text, '    '),
            (Token.Name.Decorator, '@classmethod'),
            (Token.Text, '\n'),
            (Token.Text, '    '),
            (Token.Keyword, 'def'),
            (Token.Text, ' '),
            (Token.Name.Function, 'hello'),
            (Token.Punctuation, '('),
            (Token.Name.Builtin.Pseudo, 'cls'),
            (Token.Punctuation, ')'),
            (Token.Punctuation, ':'),
            (Token.Text, '\n'),
            (Token.Text, '        '),
            (Token.Keyword, 'pass'),
            (Token.Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))


class Python3Test(unittest.TestCase):
    def setUp(self):
        self.lexer = Python3Lexer()
        
    def testNeedsName(self):
        """
        Tests that '@' is recognized as an Operator
        """
        fragment = u'S = (H @ beta - r).T @ inv(H @ V @ H.T) @ (H @ beta - r)\n'
        tokens = [
            (Token.Name, u'S'),
            (Token.Text, u' '),
            (Token.Operator, u'='),
            (Token.Text, u' '),
            (Token.Punctuation, u'('),
            (Token.Name, u'H'),
            (Token.Text, u' '),
            (Token.Operator, u'@'),
            (Token.Text, u' '),
            (Token.Name, u'beta'),
            (Token.Text, u' '),
            (Token.Operator, u'-'),
            (Token.Text, u' '),
            (Token.Name, u'r'),
            (Token.Punctuation, u')'),
            (Token.Operator, u'.'),
            (Token.Name, u'T'),
            (Token.Text, u' '),
            (Token.Operator, u'@'),
            (Token.Text, u' '),
            (Token.Name, u'inv'),
            (Token.Punctuation, u'('),
            (Token.Name, u'H'),
            (Token.Text, u' '),
            (Token.Operator, u'@'),
            (Token.Text, u' '),
            (Token.Name, u'V'),
            (Token.Text, u' '),
            (Token.Operator, u'@'),
            (Token.Text, u' '),
            (Token.Name, u'H'),
            (Token.Operator, u'.'),
            (Token.Name, u'T'),
            (Token.Punctuation, u')'),
            (Token.Text, u' '),
            (Token.Operator, u'@'),
            (Token.Text, u' '),
            (Token.Punctuation, u'('),
            (Token.Name, u'H'),
            (Token.Text, u' '),
            (Token.Operator, u'@'),
            (Token.Text, u' '),
            (Token.Name, u'beta'),
            (Token.Text, u' '),
            (Token.Operator, u'-'),
            (Token.Text, u' '),
            (Token.Name, u'r'),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def test_pep_515(self):
        """
        Tests that the lexer can parse numeric literals with underscores
        """
        fragments = (
            (Token.Literal.Number.Integer, u'1_000_000'),
            (Token.Literal.Number.Float, u'1_000.000_001'),
            (Token.Literal.Number.Float, u'1_000e1_000j'),
            (Token.Literal.Number.Hex, u'0xCAFE_F00D'),
            (Token.Literal.Number.Bin, u'0b_0011_1111_0100_1110'),
            (Token.Literal.Number.Oct, u'0o_777_123'),
        )

        for token, fragment in fragments:
            tokens = [
                (token, fragment),
                (Token.Text, u'\n'),
            ]
            self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
