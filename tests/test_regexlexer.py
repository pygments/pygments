# -*- coding: utf-8 -*-
"""
    Pygments regex lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Text
from pygments.lexer import RegexLexer, default


@pytest.fixture(scope='module')
def lexer():
    yield MyLexer()


class MyLexer(RegexLexer):
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


def test_tuple(lexer):
    toks = list(lexer.get_tokens_unprocessed('abcde'))
    assert toks == [
        (0, Text.Root, 'a'), (1, Text.Rag, 'b'), (2, Text.Rag, 'c'),
        (3, Text.Beer, 'd'), (4, Text.Root, 'e')]


def test_multiline(lexer):
    toks = list(lexer.get_tokens_unprocessed('a\ne'))
    assert toks == [
        (0, Text.Root, 'a'), (1, Text, '\n'), (2, Text.Root, 'e')]


def test_default(lexer):
    toks = list(lexer.get_tokens_unprocessed('d'))
    assert toks == [(0, Text.Beer, 'd')]


def test_pop_empty_regular(lexer):
    toks = list(lexer.get_tokens_unprocessed('#e'))
    assert toks == [(0, Text.Root, '#'), (1, Text.Root, 'e')]


def test_pop_empty_tuple(lexer):
    toks = list(lexer.get_tokens_unprocessed('@e'))
    assert toks == [(0, Text.Root, '@'), (1, Text.Root, 'e')]
