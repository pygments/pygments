# -*- coding: utf-8 -*-
"""
    Javascript tests
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.javascript import JavascriptLexer, TypeScriptLexer
from pygments.token import Number, Token


@pytest.fixture(scope='module')
def lexer():
    yield JavascriptLexer()


@pytest.mark.parametrize(
    'text',
    (
        '1', '1.', '.1', '1.1', '1e1', '1E1', '1e+1', '1E-1', '1.e1', '.1e1',
        '0888',  # octal prefix with non-octal numbers
    )
)
def test_float_literal_positive_matches(lexer, text):
    """Test literals that should be tokenized as float literals."""
    assert list(lexer.get_tokens(text))[0] == (Number.Float, text)


@pytest.mark.parametrize('text', ('.\u0b6a', '.', '1..', '1n', '1ee', '1e', '1e-', '1e--1', '1e++1', '1e1.0'))
def test_float_literals_negative_matches(lexer, text):
    """Test text that should **not** be tokenized as float literals."""
    assert list(lexer.get_tokens(text))[0] != (Number.Float, text)


@pytest.mark.parametrize('text', ('0n', '123n'))
def test_integer_literal_positive_matches(lexer, text):
    """Test literals that should be tokenized as integer literals."""
    assert list(lexer.get_tokens(text))[0] == (Number.Integer, text)


@pytest.mark.parametrize('text', ('1N', '1', '1.0'))
def test_integer_literals_negative_matches(lexer, text):
    """Test text that should **not** be tokenized as integer literals."""
    assert list(lexer.get_tokens(text))[0] != (Number.Integer, text)


@pytest.mark.parametrize('text', ('0b01', '0B10n'))
def test_binary_literal_positive_matches(lexer, text):
    """Test literals that should be tokenized as binary literals."""
    assert list(lexer.get_tokens(text))[0] == (Number.Bin, text)


@pytest.mark.parametrize('text', ('0b0N', '0b', '0bb', '0b2'))
def test_binary_literals_negative_matches(lexer, text):
    """Test text that should **not** be tokenized as binary literals."""
    assert list(lexer.get_tokens(text))[0] != (Number.Bin, text)


@pytest.mark.parametrize('text', ('017', '071n', '0o11', '0O77n'))
def test_octal_literal_positive_matches(lexer, text):
    """Test literals that should be tokenized as octal literals."""
    assert list(lexer.get_tokens(text))[0] == (Number.Oct, text)


@pytest.mark.parametrize('text', ('01N', '089', '098n', '0o', '0OO', '0o88', '0O88n'))
def test_octal_literals_negative_matches(lexer, text):
    """Test text that should **not** be tokenized as octal literals."""
    assert list(lexer.get_tokens(text))[0] != (Number.Oct, text)


@pytest.mark.parametrize('text', ('0x01', '0Xefn', '0x0EF'))
def test_hexadecimal_literal_positive_matches(lexer, text):
    """Test literals that should be tokenized as hexadecimal literals."""
    assert list(lexer.get_tokens(text))[0] == (Number.Hex, text)


@pytest.mark.parametrize('text', ('0x0N', '0x', '0Xx', '0xg', '0xhn'))
def test_hexadecimal_literals_negative_matches(lexer, text):
    """Test text that should **not** be tokenized as hexadecimal literals."""
    assert list(lexer.get_tokens(text))[0] != (Number.Hex, text)

@pytest.fixture(scope='module')
def ts_lexer():
    yield TypeScriptLexer()

def test_function_definition(ts_lexer):
    fragment = u'async function main() {\n}'
    tokens = [
        (Token.Keyword, u'async'),
        (Token.Text, u' '),
        (Token.Keyword.Declaration, u'function'),
        (Token.Text, u' '),
        (Token.Name.Other, u'main'),
        (Token.Punctuation, u'('),
        (Token.Punctuation, u')'),
        (Token.Text, u' '),
        (Token.Punctuation, u'{'),
        (Token.Text, u'\n'),
        (Token.Punctuation, u'}'),
        (Token.Text, u'\n'),
    ]
    assert list(ts_lexer.get_tokens(fragment)) == tokens
