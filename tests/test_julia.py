# -*- coding: utf-8 -*-
"""
    Julia Tests
    ~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import JuliaLexer


@pytest.fixture(scope='module')
def lexer():
    yield JuliaLexer()


def test_unicode(lexer):
    """
    Test that unicode character, √, in an expression is recognized
    """
    fragment = 's = \u221a((1/n) * sum(count .^ 2) - mu .^2)\n'
    tokens = [
        (Token.Name, 's'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Operator, '\u221a'),
        (Token.Punctuation, '('),
        (Token.Punctuation, '('),
        (Token.Literal.Number.Integer, '1'),
        (Token.Operator, '/'),
        (Token.Name, 'n'),
        (Token.Punctuation, ')'),
        (Token.Text, ' '),
        (Token.Operator, '*'),
        (Token.Text, ' '),
        (Token.Name, 'sum'),
        (Token.Punctuation, '('),
        (Token.Name, 'count'),
        (Token.Text, ' '),
        (Token.Operator, '.^'),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '2'),
        (Token.Punctuation, ')'),
        (Token.Text, ' '),
        (Token.Operator, '-'),
        (Token.Text, ' '),
        (Token.Name, 'mu'),
        (Token.Text, ' '),
        (Token.Operator, '.^'),
        (Token.Literal.Number.Integer, '2'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
