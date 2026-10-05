"""
    Pygments regex lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import threading

import pytest

from pygments.token import Error, Text, Whitespace
from pygments.lexer import RegexLexer, combined, default
from pygments.util import Future


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
        (0, Text.Root, 'a'), (1, Whitespace, '\n'), (2, Text.Root, 'e')]


def test_default(lexer):
    toks = list(lexer.get_tokens_unprocessed('d'))
    assert toks == [(0, Text.Beer, 'd')]


def test_pop_empty_regular(lexer):
    toks = list(lexer.get_tokens_unprocessed('#e'))
    assert toks == [(0, Text.Root, '#'), (1, Text.Root, 'e')]


def test_pop_empty_tuple(lexer):
    toks = list(lexer.get_tokens_unprocessed('@e'))
    assert toks == [(0, Text.Root, '@'), (1, Text.Root, 'e')]


class Hook(Future):
    """Stands in for a regex, calls back when the rule is preprocessed."""

    def __init__(self, callback):
        self.callback = callback

    def get(self):
        self.callback()
        return 'h'


def test_concurrent_first_instantiation():
    other_started = threading.Event()
    main_done = threading.Event()
    results = []

    def at_start():
        # Only reached by the other thread if it preprocesses the token
        # definitions while the main thread is still at it.
        if threading.current_thread() is other:
            other_started.set()
            main_done.wait(5)

    def half_way():
        if threading.current_thread() is not other:
            other.start()
            other_started.wait(0.1)

    class ThreadedLexer(RegexLexer):
        tokens = {
            'root': [
                (Hook(at_start), Error),
                ('a', Text.Root, combined('x', 'y')),
                (Hook(half_way), Error),
                ('b', Text.Root, combined('y', 'z')),
            ],
            'x': [('x', Text.X, '#pop')],
            'y': [('y', Text.Y, '#pop')],
            'z': [('z', Text.Z, '#pop')],
        }

    def run():
        results.append(list(ThreadedLexer().get_tokens_unprocessed('ax')))

    other = threading.Thread(target=run)
    expected = [(0, Text.Root, 'a'), (1, Text.X, 'x')]
    try:
        assert list(ThreadedLexer().get_tokens_unprocessed('ax')) == expected
    finally:
        main_done.set()
        other.join()
    assert results == [expected]


def test_token_variants_table():
    visible = []

    def check():
        visible.append('variant' in VariantLexer._all_tokens)

    class VariantLexer(RegexLexer):
        token_variants = True
        tokens = {
            'variant': {
                'root': [
                    (Hook(check), Error),
                    ('a', Text.Root),
                ],
            },
        }

        def __init__(self, **options):
            if 'variant' not in self._all_tokens:
                self._tokens = self.__class__.process_tokendef('variant')
            else:
                self._tokens = self._all_tokens['variant']
            RegexLexer.__init__(self, **options)

    for _ in range(2):
        toks = list(VariantLexer().get_tokens_unprocessed('a'))
        assert toks == [(0, Text.Root, 'a')]
    # The table is processed once, and other threads instantiating the
    # lexer cannot pick it up before it is complete.
    assert visible == [False]
