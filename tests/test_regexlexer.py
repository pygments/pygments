# -*- coding: utf-8 -*-
"""
    Pygments regex lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2009 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Text
from pygments.lexer import RegexLexer

class TestLexer(RegexLexer):
    """Test tuple state transitions including #pop."""
    tokens = {
        'root': [
            ('a', Text.Root, 'rag'),
            ('e', Text.Root),
        ],
        'beer': [
            ('d', Text.Beer, ('#pop', '#pop')),
        ],
        'rag': [
            ('b', Text.Rag, '#push'),
            ('c', Text.Rag, ('#pop', 'beer')),
        ],
    }

class TupleTransTest(unittest.TestCase):
    def test(self):
        lx = TestLexer()
        toks = list(lx.get_tokens_unprocessed('abcde'))
        self.assertEquals(toks,
           [(0, Text.Root, 'a'), (1, Text.Rag, 'b'), (2, Text.Rag, 'c'),
            (3, Text.Beer, 'd'), (4, Text.Root, 'e')])
