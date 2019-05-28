# -*- coding: utf-8 -*-
"""
    Basic ColdfusionHtmlLexer Test
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest
import os

from pygments.token import Token
from pygments.lexers import NasmLexer


class NasmLexerTest(unittest.TestCase):

    def setUp(self):
        self.lexer = NasmLexer()

    def testCPUID(self):
        # CPU is a valid directive, and we don't want to parse this as
        # cpu id, but as a single token. See bug #1517
        fragment = 'cpuid'
        expected = [
            (Token.Name.Function, u'cpuid'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(expected, list(self.lexer.get_tokens(fragment)))
