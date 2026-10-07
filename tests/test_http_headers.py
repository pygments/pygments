"""
    HTTP header regression tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import get_lexer_by_name
from pygments.lexers.textfmts import HttpLexer
from pygments.token import Error, Literal, Name, Operator, Punctuation, Text


def assert_preserved(text, tokens):
    position = 0
    for offset, _, value in tokens:
        assert offset == position
        assert text[offset:offset + len(value)] == value
        position += len(value)
    assert position == len(text)
    assert ''.join(value for _, _, value in tokens) == text


@pytest.mark.parametrize(('name', 'value'), [
    (':method', 'GET'),
    (':scheme', 'https'),
    (':authority', 'example.com'),
    (':path', '/route?q=a:b'),
    (':status', '200'),
    (':protocol', 'websocket'),
])
@pytest.mark.parametrize('ending', ['\n', '\r\n', ''])
def test_pseudo_header_name_and_delimiter(name, value, ending):
    start = 'HTTP/2.0 200 OK\n'
    text = start + name + ': ' + value + ending
    tokens = list(HttpLexer().get_tokens_unprocessed(text))
    offset = len(start)
    assert (offset, Name.Attribute, name) in tokens
    assert (offset + len(name), Operator, ':') in tokens
    assert (offset + len(name) + 2, Literal, value) in tokens
    assert not any(token is Error for _, token, _ in tokens)
    assert_preserved(text, tokens)


@pytest.mark.parametrize('version', ['2', '2.0', '3'])
@pytest.mark.parametrize('ending', ['\n', '\r\n'])
@pytest.mark.parametrize('response', [False, True])
def test_pseudo_headers_in_complete_sessions(version, ending, response):
    start = f'HTTP/{version} 200 OK' if response else f'GET / HTTP/{version}'
    header = ':status: 200' if response else ':authority: example.com'
    text = ending.join([start, header, 'X-Empty:', '', 'body'])
    tokens = list(get_lexer_by_name('http').get_tokens_unprocessed(text))
    expected_name = ':status' if response else ':authority'
    assert any(token is Name.Attribute and value == expected_name
               for _, token, value in tokens)
    assert not any(token is Error for _, token, _ in tokens)
    assert_preserved(text, tokens)


@pytest.mark.parametrize(('header', 'expected'), [
    ('Host: example.com', ['Host', '', ':', ' ', 'example.com']),
    ('X-Empty:', ['X-Empty', '', ':', '', '']),
    ('X-Value: a:b:c', ['X-Value', '', ':', ' ', 'a:b:c']),
    ('X-Spaces  :  value', ['X-Spaces', '  ', ':', '  ', 'value']),
])
@pytest.mark.parametrize('ending', ['\n', '\r\n', ''])
def test_ordinary_headers_keep_existing_tokens(header, expected, ending):
    start = 'GET / HTTP/1.1\n'
    text = start + header + ending
    tokens = list(HttpLexer().get_tokens_unprocessed(text))
    header_tokens = [(token, value) for offset, token, value in tokens
                     if offset >= len(start)]
    assert header_tokens == list(zip(
        [Name.Attribute, Text, Operator, Text, Literal, Text],
        expected + [ending]))
    assert_preserved(text, tokens)


@pytest.mark.parametrize('ending', ['\n', '\r\n'])
def test_pseudo_headers_preserve_content_type_body_lexer(ending):
    text = ending.join(['POST / HTTP/2', ':authority: example.com',
                        'Content-Type: application/json; charset=utf-8',
                        '', '{"key": 1}'])
    tokens = list(HttpLexer().get_tokens_unprocessed(text))
    assert any(token is Name.Attribute and value == ':authority'
               for _, token, value in tokens)
    assert any(token is Punctuation and value == '{'
               for _, token, value in tokens)
    assert not any(token is Error for _, token, _ in tokens)
    assert_preserved(text, tokens)


def test_content_type_is_reset_between_sessions():
    lexer = HttpLexer()
    first = 'GET / HTTP/2\n:authority: example.com\nContent-Type: application/json\n\n{"key": 1}'
    first_tokens = list(lexer.get_tokens_unprocessed(first))
    assert any(token is Punctuation and value == '{'
               for _, token, value in first_tokens)
    second = 'GET / HTTP/2\n:authority: example.com\n\n{}'
    tokens = list(lexer.get_tokens_unprocessed(second))
    assert (second.index('{}'), Text, '{}') in tokens
    assert not any(token is Error for _, token, _ in tokens)
    assert_preserved(second, tokens)


def test_public_tokens_normalize_crlf_and_preserve_complete_name():
    text = 'GET / HTTP/2\r\n:authority: example.com\r\n\r\nbody'
    tokens = list(get_lexer_by_name('http').get_tokens(text))
    assert (Name.Attribute, ':authority') in tokens
    assert not any(token is Error for token, _ in tokens)
    assert ''.join(value for _, value in tokens) == text.replace('\r\n', '\n') + '\n'


def test_header_continuation_after_pseudo_header():
    text = 'GET / HTTP/2\n:authority: example.com\nX-Long: first\n second\n\nbody'
    tokens = list(HttpLexer().get_tokens_unprocessed(text))
    assert (text.index('second'), Literal, 'second') in tokens
    assert not any(token is Error for _, token, _ in tokens)
    assert_preserved(text, tokens)
