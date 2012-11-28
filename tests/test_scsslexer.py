# -*- coding: utf-8 -*-
"""
    ScssLexer Tests
    ~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Text, Number, Name, Operator
from pygments.lexers import ScssLexer


class ScssLexerTest(unittest.TestCase):

    def setUp(self):
        self.lexer = ScssLexer()

    def testHyphenatedVariable(self):
        code = u'$total-columns  : 4'
        wanted = [
            (Name.Variable, u'$total-columns'),
            (Operator, u'  :'),
            (Text, u' '),
            (Number.Integer, u'4'),
            (Text, u'\n'),
            ]
        self.assertEqual(list(self.lexer.get_tokens(code)), wanted)
