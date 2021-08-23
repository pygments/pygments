# -*- coding: utf-8 -*-
"""
    Basic ThingsDB Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Number, Text, Comment
from pygments.lexers import ThingsDBLexer


class ThingsDBTest(unittest.TestCase):

    def setUp(self):
        self.lexer = ThingsDBLexer()
        self.maxDiff = None

    def testNumber(self):
        fragment = u'42'
        tokens = [
            (Number.Integer, u'42'),
             (Text.Whitespace, '\n')
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testThingId(self):
        fragment = u'#42'
        tokens = [
            (Comment.Preproc, u'#42'),
             (Text.Whitespace, '\n')
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
