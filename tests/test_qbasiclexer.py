# -*- coding: utf-8 -*-
"""
    Tests for QBasic
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers.basic import QBasicLexer


@pytest.fixture(scope='module')
def lexer():
    yield QBasicLexer()


def test_keywords_with_dollar(lexer):
    fragment = u'DIM x\nx = RIGHT$("abc", 1)\n'
    expected = [
        (Token.Keyword.Declaration, u'DIM'),
        (Token.Text.Whitespace, u' '),
        (Token.Name.Variable.Global, u'x'),
        (Token.Text, u'\n'),
        (Token.Name.Variable.Global, u'x'),
        (Token.Text.Whitespace, u' '),
        (Token.Operator, u'='),
        (Token.Text.Whitespace, u' '),
        (Token.Keyword.Reserved, u'RIGHT$'),
        (Token.Punctuation, u'('),
        (Token.Literal.String.Double, u'"abc"'),
        (Token.Punctuation, u','),
        (Token.Text.Whitespace, u' '),
        (Token.Literal.Number.Integer.Long, u'1'),
        (Token.Punctuation, u')'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected
