# -*- coding: utf-8 -*-
"""
    Basic Tests for textfmts
    ~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers.textfmts import HttpLexer


@pytest.fixture(scope='module')
def lexer():
    yield HttpLexer()


def test_http_status_line(lexer):
    fragment = 'HTTP/1.1 200 OK\n'
    tokens = [
        (Token.Keyword.Reserved, 'HTTP'),
        (Token.Operator, '/'),
        (Token.Number, '1.1'),
        (Token.Text, ' '),
        (Token.Number, '200'),
        (Token.Text, ' '),
        (Token.Name.Exception, 'OK'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_http_status_line_without_reason_phrase(lexer):
    fragment = 'HTTP/1.1 200\n'
    tokens = [
        (Token.Keyword.Reserved, 'HTTP'),
        (Token.Operator, '/'),
        (Token.Number, '1.1'),
        (Token.Text, ' '),
        (Token.Number, '200'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_http_status_line_without_reason_phrase_rfc_7230(lexer):
    fragment = 'HTTP/1.1 200 \n'
    tokens = [
        (Token.Keyword.Reserved, 'HTTP'),
        (Token.Operator, '/'),
        (Token.Number, '1.1'),
        (Token.Text, ' '),
        (Token.Number, '200'),
        (Token.Text, ' '),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_application_xml(lexer):
    fragment = 'GET / HTTP/1.0\nContent-Type: application/xml\n\n<foo>\n'
    tokens = [
        (Token.Name.Tag, '<foo'),
        (Token.Name.Tag, '>'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment))[-len(tokens):] == tokens


def test_application_calendar_xml(lexer):
    fragment = 'GET / HTTP/1.0\nContent-Type: application/calendar+xml\n\n<foo>\n'
    tokens = [
        (Token.Name.Tag, '<foo'),
        (Token.Name.Tag, '>'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment))[-len(tokens):] == tokens
