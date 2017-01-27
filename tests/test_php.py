# -*- coding: utf-8 -*-
"""
    PHP Tests
    ~~~~~~~~~

    :copyright: Copyright 2006-2017 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.lexers import PhpLexer
from pygments.token import Token


class PhpTest(unittest.TestCase):
    def setUp(self):
        self.lexer = PhpLexer()

    def testStringEscapingRun(self):
        fragment = '<?php $x="{\\""; ?>\n'
        tokens = [
            (Token.Comment.Preproc, '<?php'),
            (Token.Text, ' '),
            (Token.Name.Variable, '$x'),
            (Token.Operator, '='),
            (Token.Literal.String.Double, '"'),
            (Token.Literal.String.Double, '{'),
            (Token.Literal.String.Escape, '\\"'),
            (Token.Literal.String.Double, '"'),
            (Token.Punctuation, ';'),
            (Token.Text, ' '),
            (Token.Comment.Preproc, '?>'),
            (Token.Other, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
