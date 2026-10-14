"""Tests for IDL comments."""

import pytest

from pygments.lexers.idl import IDLLexer
from pygments.token import Comment, String, Whitespace


@pytest.mark.parametrize('prefix', ['', '  ', 'x = 5  '])
@pytest.mark.parametrize('ending', ['\n', ''])
def test_line_comments(prefix, ending):
    source = prefix + '; comment' + ending
    tokens = list(IDLLexer().get_tokens_unprocessed(source))
    assert (len(prefix), Comment.Single, '; comment') in tokens
    assert ''.join(value for _, _, value in tokens) == source
    if ending:
        assert tokens[-1] == (len(source) - 1, Whitespace, '\n')


@pytest.mark.parametrize(('quoted', 'token'), [
    ('"a;b"', String.Double), ("'a;b'", String.Single),
])
def test_quoted_semicolon(quoted, token):
    source = 'print, ' + quoted + ' ; comment\n'
    tokens = list(IDLLexer().get_tokens_unprocessed(source))
    assert (7, token, quoted) in tokens
    assert [value for _, kind, value in tokens if kind in Comment] == ['; comment']
