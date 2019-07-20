# -*- coding: utf-8 -*-
"""
    Data Tests
    ~~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexers import JsonLexer, JsonBareObjectLexer, YamlLexer
from pygments.token import Token


class JsonTest(unittest.TestCase):
    def setUp(self):
        self.lexer = JsonLexer()

    def testBasic(self):
        fragment = u'{"foo": "bar", "foo2": [1, 2, 3]}\n'
        tokens = [
            (Token.Punctuation, u'{'),
            (Token.Name.Tag, u'"foo"'),
            (Token.Punctuation, u':'),
            (Token.Text, u' '),
            (Token.Literal.String.Double, u'"bar"'),
            (Token.Punctuation, u','),
            (Token.Text, u' '),
            (Token.Name.Tag, u'"foo2"'),
            (Token.Punctuation, u':'),
            (Token.Text, u' '),
            (Token.Punctuation, u'['),
            (Token.Literal.Number.Integer, u'1'),
            (Token.Punctuation, u','),
            (Token.Text, u' '),
            (Token.Literal.Number.Integer, u'2'),
            (Token.Punctuation, u','),
            (Token.Text, u' '),
            (Token.Literal.Number.Integer, u'3'),
            (Token.Punctuation, u']'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

class JsonBareObjectTest(unittest.TestCase):
    def setUp(self):
        self.lexer = JsonBareObjectLexer()

    def testBasic(self):
        # This is the same as testBasic for JsonLexer above, except the
        # enclosing curly braces are removed.
        fragment = u'"foo": "bar", "foo2": [1, 2, 3]\n'
        tokens = [
            (Token.Name.Tag, u'"foo"'),
            (Token.Punctuation, u':'),
            (Token.Text, u' '),
            (Token.Literal.String.Double, u'"bar"'),
            (Token.Punctuation, u','),
            (Token.Text, u' '),
            (Token.Name.Tag, u'"foo2"'),
            (Token.Punctuation, u':'),
            (Token.Text, u' '),
            (Token.Punctuation, u'['),
            (Token.Literal.Number.Integer, u'1'),
            (Token.Punctuation, u','),
            (Token.Text, u' '),
            (Token.Literal.Number.Integer, u'2'),
            (Token.Punctuation, u','),
            (Token.Text, u' '),
            (Token.Literal.Number.Integer, u'3'),
            (Token.Punctuation, u']'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testClosingCurly(self):
        # This can be an Error token, but should not be a can't-pop-from-stack
        # exception.
        fragment = '}"a"\n'
        tokens = [
            (Token.Error, '}'),
            (Token.Name.Tag, '"a"'),
            (Token.Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testClosingCurlyInValue(self):
        fragment = '"": ""}\n'
        tokens = [
            (Token.Name.Tag, '""'),
            (Token.Punctuation, ':'),
            (Token.Text, ' '),
            (Token.Literal.String.Double, '""'),
            (Token.Error, '}'),
            (Token.Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

class YamlTest(unittest.TestCase):
    def setUp(self):
        self.lexer = YamlLexer()

    def testColonInComment(self):
        # Bug #1528: This previously parsed 'token # innocent' as a tag
        fragment = u'here: token # innocent: comment\n'
        tokens = [
            (Token.Name.Tag, u'here'),
            (Token.Punctuation, u':'),
            (Token.Text, u' '),
            (Token.Literal.Scalar.Plain, u'token'),
            (Token.Text, u' '),
            (Token.Comment.Single, u'# innocent: comment'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
