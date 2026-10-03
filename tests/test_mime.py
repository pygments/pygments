"""Regression tests for MIME content-type boundary parameters."""

import pytest

from pygments.lexers.mime import MIMELexer


@pytest.mark.parametrize('newline', ['\n', '\r\n'])
@pytest.mark.parametrize('boundary', ['', ' \t', '""'])
def test_empty_boundary_header_preserves_token_text_and_offsets(newline, boundary):
    text = f'Content-Type: multipart/mixed; boundary={boundary}{newline}'
    lexer = MIMELexer()

    tokens = list(lexer.get_tokens_unprocessed(text))

    assert ''.join(value for _, _, value in tokens) == text
    for position, _, value in tokens:
        assert text[position:position + len(value)] == value
    assert lexer.boundary == ''


@pytest.mark.parametrize('newline', ['\n', '\r\n'])
@pytest.mark.parametrize('boundary', ['', ' \t', '""'])
def test_empty_boundary_mime_message(newline, boundary):
    text = newline.join([
        f'Content-Type: multipart/mixed; boundary={boundary}',
        '',
        'Keep this body even though its multipart boundary is empty.',
        '',
    ])

    tokens = list(MIMELexer().get_tokens(text))

    assert ''.join(value for _, value in tokens) == text.replace('\r\n', '\n')


@pytest.mark.parametrize('newline', ['\n', '\r\n'])
def test_nested_empty_boundary_mime_message(newline):
    text = newline.join([
        'Content-Type: multipart/mixed; boundary=outer',
        '',
        '--outer',
        'Content-Type: multipart/alternative; boundary=',
        '',
        'Keep the malformed child body.',
        '--outer--',
        '',
    ])

    tokens = list(MIMELexer().get_tokens(text))

    assert ''.join(value for _, value in tokens) == text.replace('\r\n', '\n')


@pytest.mark.parametrize('boundary', ['', ' \t', '""'])
def test_explicit_empty_boundary_overrides_configured_boundary(boundary):
    lexer = MIMELexer(**{'Multipart-Boundary': 'configured'})
    list(lexer.get_tokens('Content-Type: multipart/mixed\n'))
    assert lexer.boundary == 'configured'

    list(lexer.get_tokens(f'Content-Type: multipart/mixed; boundary={boundary}\n'))
    assert lexer.boundary == ''


@pytest.mark.parametrize('boundary', ['', ' \t', '""'])
def test_lexer_reuse_after_empty_boundary(boundary):
    lexer = MIMELexer()
    list(lexer.get_tokens(f'Content-Type: multipart/mixed; boundary={boundary}\n'))
    assert lexer.boundary == ''

    text = 'Content-Type: multipart/mixed; boundary="next"\n\n--next\n\nbody\n--next--\n'
    tokens = list(lexer.get_tokens(text))

    assert lexer.boundary == 'next'
    assert ''.join(value for _, value in tokens) == text
