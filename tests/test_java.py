# -*- coding: utf-8 -*-
"""
    Basic JavaLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Text, Name, Operator, Keyword
from pygments.lexers import JavaLexer


class JavaTest(unittest.TestCase):

    def setUp(self):
        self.lexer = JavaLexer()
        self.maxDiff = None

    def testEnhancedFor(self):
        fragment = u'label:\nfor(String var2: var1) {}\n'
        tokens = [
            (Name.Label, u'label:'),
            (Text, u'\n'),
            (Keyword, u'for'),
            (Operator, u'('),
            (Name, u'String'),
            (Text, u' '),
            (Name, u'var2'),
            (Operator, u':'),
            (Text, u' '),
            (Name, u'var1'),
            (Operator, u')'),
            (Text, u' '),
            (Operator, u'{'),
            (Operator, u'}'),
            (Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

