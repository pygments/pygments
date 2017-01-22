# -*- coding: utf-8 -*-
"""
    Whiley Test
    ~~~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexers import WhileyLexer
from pygments.token import Token


class WhileyTest(unittest.TestCase):
    def setUp(self):
        self.lexer = WhileyLexer()

    def testWhileyOperator(self):
        fragment = u'123 \u2200 x\n'
        tokens = [
            (Token.Literal.Number.Integer, u'123'),
            (Token.Text, u' '),
            (Token.Operator, u'\u2200'),
            (Token.Text, u' '),
            (Token.Name, u'x'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
