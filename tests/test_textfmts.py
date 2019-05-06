# -*- coding: utf-8 -*-
"""
    Basic Tests for textfmts
    ~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers.textfmts import HttpLexer


@pytest.fixture(scope='module')
def lexer():
    yield HttpLexer()


def test_application_xml(lexer):
    fragment = u'GET / HTTP/1.0\nContent-Type: application/xml\n\n<foo>\n'
    tokens = [
        (Token.Name.Tag, u'<foo'),
        (Token.Name.Tag, u'>'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment))[-len(tokens):] == tokens


def test_application_calendar_xml(lexer):
    fragment = u'GET / HTTP/1.0\nContent-Type: application/calendar+xml\n\n<foo>\n'
    tokens = [
        (Token.Name.Tag, u'<foo'),
        (Token.Name.Tag, u'>'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment))[-len(tokens):] == tokens
