# -*- coding: utf-8 -*-
"""
    CPP Tests
    ~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexers import CppLexer
from pygments.token import Token


class CppTest(unittest.TestCase):
    def setUp(self):
        self.lexer = CppLexer()

    def testGoodComment(self):
        fragment = u'/* foo */\n'
        tokens = [
            (Token.Comment.Multiline, u'/* foo */'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testOpenComment(self):
        fragment = u'/* foo\n'
        tokens = [
            (Token.Comment.Multiline, u'/* foo\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
