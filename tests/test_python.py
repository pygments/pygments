# -*- coding: utf-8 -*-
"""
    Python Tests
    ~~~~~~~~~~~~

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexers import PythonLexer
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
