# -*- coding: utf-8 -*-
"""
    Properties Tests
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexers.configs import PropertiesLexer
from pygments.token import Token


class PropertiesTest(unittest.TestCase):
    def setUp(self):
        self.lexer = PropertiesLexer()

    def test_comments(self):
        """
        Assures lines lead by either # or ! are recognized as a comment
        """
        fragment = '! a comment\n# also a comment\n'
        tokens = [
            (Token.Comment, '! a comment'),
            (Token.Text, '\n'),
            (Token.Comment, '# also a comment'),
            (Token.Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def test_leading_whitespace_comments(self):
        fragment = '    # comment\n'
        tokens = [
            (Token.Text, '    '),
            (Token.Comment, '# comment'),
            (Token.Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def test_escaped_space_in_key(self):
        fragment = 'key = value\n'
        tokens = [
            (Token.Name.Attribute, 'key'),
            (Token.Text, ' '),
            (Token.Operator, '='),
            (Token.Text, ' '),
            (Token.Literal.String, 'value'),
            (Token.Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def test_escaped_space_in_value(self):
        fragment = 'key = doubleword\\ value\n'
        tokens = [
            (Token.Name.Attribute, 'key'),
            (Token.Text, ' '),
            (Token.Operator, '='),
            (Token.Text, ' '),
            (Token.Literal.String, 'doubleword\\ value'),
            (Token.Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def test_space_delimited_kv_pair(self):
        fragment = 'key value\n'
        tokens = [
            (Token.Name.Attribute, 'key'),
            (Token.Text, ' '),
            (Token.Literal.String, 'value\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def test_just_key(self):
        fragment = 'justkey\n'
        tokens = [
            (Token.Name.Attribute, 'justkey'),
            (Token.Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def test_just_key_with_space(self):
        fragment = 'just\\ key\n'
        tokens = [
            (Token.Name.Attribute, 'just\\ key'),
            (Token.Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
