"""Shared Haskell type signature highlighting."""

import pytest

from pygments.lexers.haskell import HaskellLexer
from pygments.token import Comment, Name, Operator, String


@pytest.mark.parametrize('source,names', [
    ('head, last :: [a] -> a', ['head', 'last']),
    ('first, middle, last :: a -> a', ['first', 'middle', 'last']),
    ('  first, last :: a -> a', ['first', 'last']),
    ('outer = first\n  where \n    first, last :: a -> a', ['first', 'last']),
    ('module Example where\n\nfirst, last :: a -> a', ['first', 'last']),
    ('first,\n  last\n  :: a -> a', ['first', 'last']),
    ("first', _last :: a -> a", ["first'", '_last']),
    ('α, β :: a -> a', ['α', 'β']),
    ('first, (<+>), last :: a -> a', ['first', 'last']),
    ('first, last :: Eq a => a -> a', ['first', 'last']),
])
def test_shared_type_signature(source, names):
    tokens = list(HaskellLexer().get_tokens(source))
    assert ''.join(value for _, value in tokens) == source + '\n'
    for name in names:
        assert (Name.Function, name) in tokens


@pytest.mark.parametrize('source', [
    'result = (first, last)',
    'result = [first, last]',
    'result = Record { first = x, last = y }',
    'result = (first, last) :: (a, a)',
    'result = f first last',
])
def test_expression_names_remain_names(source):
    tokens = list(HaskellLexer().get_tokens(source))
    assert (Name, 'first') in tokens
    assert (Name, 'last') in tokens


def test_comments_strings_and_operators():
    tokens = list(HaskellLexer().get_tokens(
        'first, (<+>), last :: a -> a\n-- first, last :: a\n"first, last :: a"'))
    assert (Operator, '<+>') in tokens
    assert any(kind in Comment and 'first, last' in value for kind, value in tokens)
    assert any(kind in String and 'first, last' in value for kind, value in tokens)
