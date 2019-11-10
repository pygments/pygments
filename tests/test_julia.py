# -*- coding: utf-8 -*-
"""
    Julia Tests
    ~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
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
    fragment = u's = \u221a((1/n) * sum(count .^ 2) - mu .^2)\n'
    tokens = [
        (Token.Name, u's'),
        (Token.Text, u' '),
        (Token.Operator, u'='),
        (Token.Text, u' '),
        (Token.Operator, u'\u221a'),
        (Token.Punctuation, u'('),
        (Token.Punctuation, u'('),
        (Token.Literal.Number.Integer, u'1'),
        (Token.Operator, u'/'),
        (Token.Name, u'n'),
        (Token.Punctuation, u')'),
        (Token.Text, u' '),
        (Token.Operator, u'*'),
        (Token.Text, u' '),
        (Token.Name, u'sum'),
        (Token.Punctuation, u'('),
        (Token.Name, u'count'),
        (Token.Text, u' '),
        (Token.Operator, u'.^'),
        (Token.Text, u' '),
        (Token.Literal.Number.Integer, u'2'),
        (Token.Punctuation, u')'),
        (Token.Text, u' '),
        (Token.Operator, u'-'),
        (Token.Text, u' '),
        (Token.Name, u'mu'),
        (Token.Text, u' '),
        (Token.Operator, u'.^'),
        (Token.Literal.Number.Integer, u'2'),
        (Token.Punctuation, u')'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
