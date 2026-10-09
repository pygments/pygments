"""
    MIME lexer constructor options
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import base64
from functools import partial

import pytest

from pygments.lexers import get_lexer_by_name
from pygments.lexers.mime import MIMELexer
from pygments.token import Keyword, Other
from tests.test_cmdline import check_success


BODY = '{"answer": false}\n'
FALLBACK_MESSAGE = 'MIME-Version: 1.0\n\n' + BODY
JSON_MESSAGE = 'Content-Type: application/json\n\n' + BODY
ENCODED_BODY = base64.b64encode(BODY.encode()).decode() + '\n'
ENCODED_MESSAGE = 'Content-Type: application/json\n\n' + ENCODED_BODY


@pytest.fixture(params=['direct', 'lookup'])
def make_lexer(request):
    if request.param == 'direct':
        return MIMELexer
    return partial(get_lexer_by_name, 'mime')


def get_tokens(lexer, message):
    tokens = list(lexer.get_tokens(message))
    assert ''.join(value for _, value in tokens) == message
    return tokens


@pytest.mark.parametrize('key', ['Content-Type', 'Content_Type'])
def test_content_type_fallback(make_lexer, key):
    tokens = get_tokens(make_lexer(**{key: 'application/json'}), FALLBACK_MESSAGE)
    assert (Keyword.Constant, 'false') in tokens


@pytest.mark.parametrize('key', [
    'Content-Transfer-Encoding', 'Content_Transfer_Encoding',
])
@pytest.mark.parametrize('encoding', [
    'base64', '7bit', '8bit', 'quoted-printable',
])
def test_transfer_encoding_fallback(make_lexer, key, encoding):
    message = ENCODED_MESSAGE if encoding == 'base64' else JSON_MESSAGE
    tokens = get_tokens(make_lexer(**{key: encoding}), message)
    if encoding == 'base64':
        assert (Other, ENCODED_BODY) in tokens
        assert (Keyword.Constant, 'false') not in tokens
    else:
        assert (Keyword.Constant, 'false') in tokens


@pytest.mark.parametrize('content_type,legacy_type,highlighted', [
    ('application/json', 'text/plain', True),
    ('text/plain', 'application/json', False),
])
def test_documented_content_type_takes_precedence(
    make_lexer, content_type, legacy_type, highlighted,
):
    lexer = make_lexer(**{'Content-Type': content_type, 'Content_Type': legacy_type})
    tokens = get_tokens(lexer, FALLBACK_MESSAGE)
    assert ((Keyword.Constant, 'false') in tokens) == highlighted


@pytest.mark.parametrize('encoding', [None, ''])
def test_explicit_documented_encoding_takes_precedence(make_lexer, encoding):
    lexer = make_lexer(**{
        'Content-Transfer-Encoding': encoding,
        'Content_Transfer_Encoding': 'base64',
    })
    tokens = get_tokens(lexer, JSON_MESSAGE)
    assert (Keyword.Constant, 'false') in tokens


@pytest.mark.parametrize('options', [
    {'Content-Type': 'text/plain', 'Content-Transfer-Encoding': 'base64'},
    {'Content_Type': 'text/plain', 'Content_Transfer_Encoding': 'base64'},
    {'Content-Type': None, 'Content_Type': 'text/plain',
     'Content-Transfer-Encoding': None, 'Content_Transfer_Encoding': 'base64'},
])
def test_actual_headers_override_fallbacks(make_lexer, options):
    message = ('Content-Type: application/json\n'
               'Content-Transfer-Encoding: 7bit\n\n' + BODY)
    tokens = get_tokens(make_lexer(**options), message)
    assert (Keyword.Constant, 'false') in tokens


def test_default_body_remains_plain_text(make_lexer):
    tokens = get_tokens(make_lexer(), FALLBACK_MESSAGE)
    assert (Keyword.Constant, 'false') not in tokens


@pytest.mark.parametrize('flag', ['-O', '-P'])
@pytest.mark.parametrize('option,message,highlighted,opaque', [
    ('Content-Type=application/json', FALLBACK_MESSAGE, True, False),
    ('Content_Type=application/json', FALLBACK_MESSAGE, True, False),
    ('Content-Transfer-Encoding=base64', ENCODED_MESSAGE, False, True),
    ('Content_Transfer_Encoding=base64', ENCODED_MESSAGE, False, True),
])
def test_cli_mime_options(flag, option, message, highlighted, opaque):
    output = check_success('-l', 'mime', '-f', 'raw', flag, option, stdin=message)
    assert ("Token.Keyword.Constant\t'false'" in output) == highlighted
    assert ('Token.Other\t' in output) == opaque
