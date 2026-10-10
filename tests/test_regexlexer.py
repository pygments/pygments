"""
    Pygments regex lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pickle

import pytest

from pygments.token import Text, Whitespace
from pygments.lexer import RegexLexer, default
from pygments.lexers.ruby import RubyLexer
from pygments.lexers.shell import BashLexer


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


class PickleLexer(MyLexer):
    init_calls = 0

    def __init__(self, **options):
        type(self).init_calls += 1
        super().__init__(**options)
        self.marker = ['preserved']


class PickleChildLexer(PickleLexer):
    tokens = {'root': [('z', Text.Child)]}


class SlottedPickleLexer(PickleChildLexer):
    __slots__ = ('marker',)


@pytest.mark.parametrize('protocol', range(pickle.HIGHEST_PROTOCOL + 1))
@pytest.mark.parametrize('warm', [False, True])
@pytest.mark.parametrize(('lexer_type', 'text'), [
    (MyLexer, 'abcde'),
    (BashLexer, 'echo "hello"\n'),
    (RubyLexer, 'puts "#{1 + 2}"\n'),
    (PickleChildLexer, 'z'),
])
def test_pickle_regexlexer(monkeypatch, protocol, warm, lexer_type, text):
    original = lexer_type(stripnl=False, ensurenl=False, tabsize=4)
    original.add_filter('tokenmerge')
    original.extra = {'changed': [1, 2]}
    expected = list(original.get_tokens(text))
    cached_tokens = lexer_type._tokens
    payload = pickle.dumps(original, protocol=protocol)

    if not warm:
        # Unpickling in a fresh process imports the class without constructing
        # it, so the lazily compiled class attributes are absent.
        for name in ('_tokens', '_all_tokens', '_tmpname'):
            if name in lexer_type.__dict__:
                monkeypatch.delattr(lexer_type, name)

    restored = pickle.loads(payload)
    assert list(restored.get_tokens(text)) == expected
    assert restored.options == original.options
    assert restored.extra == original.extra
    assert len(restored.filters) == 1
    assert '_tokens' in lexer_type.__dict__
    if warm:
        assert lexer_type._tokens is cached_tokens
    if isinstance(original, PickleLexer):
        assert restored.marker == original.marker


@pytest.mark.parametrize('protocol', range(pickle.HIGHEST_PROTOCOL + 1))
def test_pickle_does_not_reinitialize(monkeypatch, protocol):
    original = PickleLexer(ensurenl=False)
    original.marker.append('modified after construction')
    payload = pickle.dumps(original, protocol=protocol)
    calls = PickleLexer.init_calls
    for name in ('_tokens', '_all_tokens', '_tmpname'):
        if name in PickleLexer.__dict__:
            monkeypatch.delattr(PickleLexer, name)

    restored = pickle.loads(payload)
    assert PickleLexer.init_calls == calls
    assert restored.marker == ['preserved', 'modified after construction']
    assert list(restored.get_tokens('abcde')) == list(original.get_tokens('abcde'))


@pytest.mark.parametrize('protocol', range(2, pickle.HIGHEST_PROTOCOL + 1))
def test_pickle_slots(monkeypatch, protocol):
    original = SlottedPickleLexer(ensurenl=False)
    original.marker.append('modified slot')
    original.extra = 'instance dictionary'
    payload = pickle.dumps(original, protocol=protocol)
    for name in ('_tokens', '_all_tokens', '_tmpname'):
        if name in SlottedPickleLexer.__dict__:
            monkeypatch.delattr(SlottedPickleLexer, name)

    restored = pickle.loads(payload)
    assert restored.marker == ['preserved', 'modified slot']
    assert restored.extra == 'instance dictionary'
    assert list(restored.get_tokens('z')) == [(Text.Child, 'z')]
    assert '_tokens' in SlottedPickleLexer.__dict__
