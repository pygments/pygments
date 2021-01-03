# -*- coding: utf-8 -*-
"""
    Tests for QBasic
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers.basic import QBasicLexer


@pytest.fixture(scope='module')
def lexer():
    yield QBasicLexer()


def test_keywords_with_dollar(lexer):
    fragment = 'DIM x\nx = RIGHT$("abc", 1)\n'
    expected = [
        (Token.Keyword.Declaration, 'DIM'),
        (Token.Text.Whitespace, ' '),
        (Token.Name.Variable.Global, 'x'),
        (Token.Text, '\n'),
        (Token.Name.Variable.Global, 'x'),
        (Token.Text.Whitespace, ' '),
        (Token.Operator, '='),
        (Token.Text.Whitespace, ' '),
        (Token.Keyword.Reserved, 'RIGHT$'),
        (Token.Punctuation, '('),
        (Token.Literal.String.Double, '"abc"'),
        (Token.Punctuation, ','),
        (Token.Text.Whitespace, ' '),
        (Token.Literal.Number.Integer.Long, '1'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected
