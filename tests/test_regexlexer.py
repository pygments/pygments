# -*- coding: utf-8 -*-
"""
    Pygments regex lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

from pygments.token import Text
from pygments.lexer import RegexLexer
from pygments.lexer import default


class TestLexer(RegexLexer):
    """Test tuple state transitions including #pop."""
    tokens = {
        'root': [
            ('a', Text.Root, 'rag'),
            ('e', Text.Root),
            ('#', Text.Root, '#pop'),
            ('@', Text.Root, ('#pop', '#pop')),
            default(('beer', 'beer'))
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
        self.assertEqual(toks, [
            (0, Text.Root, 'a'), (1, Text.Rag, 'b'), (2, Text.Rag, 'c'),
            (3, Text.Beer, 'd'), (4, Text.Root, 'e')])

    def test_multiline(self):
        lx = TestLexer()
        toks = list(lx.get_tokens_unprocessed('a\ne'))
        self.assertEqual(toks, [
            (0, Text.Root, 'a'), (1, Text, u'\n'), (2, Text.Root, 'e')])

    def test_default(self):
        lx = TestLexer()
        toks = list(lx.get_tokens_unprocessed('d'))
        self.assertEqual(toks, [(0, Text.Beer, 'd')])


class PopEmptyTest(unittest.TestCase):
    def test_regular(self):
        lx = TestLexer()
        toks = list(lx.get_tokens_unprocessed('#e'))
        self.assertEqual(toks, [(0, Text.Root, '#'), (1, Text.Root, 'e')])

    def test_tuple(self):
        lx = TestLexer()
        toks = list(lx.get_tokens_unprocessed('@e'))
        self.assertEqual(toks, [(0, Text.Root, '@'), (1, Text.Root, 'e')])
