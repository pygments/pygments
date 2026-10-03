"""HTTP multipart delegation and preservation of original token positions.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.mime import MIMELexer
from pygments.lexers.textfmts import HttpLexer
from pygments.token import Error, Keyword, Name, String, Text


def assert_original_tokens(lexer, text):
    tokens = list(lexer.get_tokens_unprocessed(text))
    assert ''.join(value for _, _, value in tokens) == text
    position = 0
    for index, token, value in tokens:
        assert index == position, (index, position, token, value)
        assert token not in Error, (index, token, value)
        position += len(value)
    return tokens


@pytest.mark.parametrize('start', ['POST /upload HTTP/1.1', 'HTTP/1.1 200 OK'])
@pytest.mark.parametrize('newline', ['\n', '\r\n'])
@pytest.mark.parametrize('header_kind', ['unquoted', 'quoted', 'folded', 'other-header'])
def test_multipart_json_body(start, newline, header_kind):
    boundary = 'sample:boundary' if header_kind == 'quoted' else 'sample-boundary'
    if header_kind == 'folded':
        header = 'Content-Type: multipart/mixed;' + newline + ' boundary=' + boundary
    elif header_kind == 'quoted':
        header = 'Content-Type: multipart/mixed; boundary="' + boundary + '"'
    else:
        header = 'Content-Type: multipart/mixed; boundary=' + boundary
    if header_kind == 'other-header':
        header += newline + 'X-Other: example' + newline + ' boundary=wrong'
    body = (
        'preamble' + newline + '--' + boundary + newline
        + 'Content-Type: application/json' + newline + newline
        + '{"answer": false}' + newline + '--' + boundary + newline
        + 'Content-Type: text/plain' + newline + newline
        + 'plain body' + newline + '--' + boundary + '--' + newline
        + 'epilogue' + newline
    )
    text = start + newline + header + newline + newline + body
    tokens = assert_original_tokens(HttpLexer(), text)
    assert any(token is String.Delimiter and value == '--' + boundary + newline
               for _, token, value in tokens)
    assert any(token is String.Delimiter and value == '--' + boundary + '--' + newline
               for _, token, value in tokens)
    assert any(token is Keyword.Constant and value == 'false'
               for _, token, value in tokens)
    assert any(token is Name.Tag and 'answer' in value
               for _, token, value in tokens)


def test_mime_first_boundary_position():
    text = ('Content-Type: multipart/mixed; boundary=sample\n\n'
            'preamble\n--sample\nContent-Type: application/json\n\n'
            '{"answer": false}\n--sample--\n')
    assert_original_tokens(MIMELexer(), text)


@pytest.mark.parametrize('content_type', [
    'application/json; charset=utf-8', 'application/vnd.example+json; charset=utf-8'
])
def test_nonmultipart_delegation(content_type):
    text = 'HTTP/1.1 200 OK\nContent-Type: ' + content_type + '\n\n{"answer": false}\n'
    tokens = assert_original_tokens(HttpLexer(), text)
    assert any(token is Keyword.Constant and value == 'false'
               for _, token, value in tokens)


def test_reused_lexer_does_not_retain_multipart_headers():
    lexer = HttpLexer()
    first = ('HTTP/1.1 200 OK\nContent-Type: multipart/mixed; boundary=sample\n\n'
             '--sample\nContent-Type: application/json\n\n'
             '{"answer": false}\n--sample--\n')
    assert_original_tokens(lexer, first)
    second = 'HTTP/1.1 200 OK\nX-Other: example\n\n--sample\n'
    tokens = assert_original_tokens(lexer, second)
    assert tokens[-1][1] is Text and tokens[-1][2] == '--sample\n'


@pytest.mark.parametrize('newline', ['\n', '\r\n'])
def test_nested_multipart_folded_boundary(newline):
    text = (
        'HTTP/1.1 200 OK' + newline
        + 'Content-Type: multipart/mixed; boundary=outer' + newline + newline
        + '--outer' + newline + 'Content-Type: multipart/alternative;' + newline
        + ' boundary=inner' + newline + newline + '--inner' + newline
        + 'Content-Type: application/json' + newline + newline
        + '{"answer": false}' + newline + '--inner--' + newline
        + '--outer--' + newline
    )
    tokens = assert_original_tokens(HttpLexer(), text)
    assert any(token is Keyword.Constant and value == 'false'
               for _, token, value in tokens)
    boundaries = [value for _, token, value in tokens
                  if token is String.Delimiter and value.startswith('--')]
    assert boundaries == [part + newline for part in
                          ['--outer', '--inner', '--inner--', '--outer--']]
