# -*- coding: utf-8 -*-
"""
    Whiley Test
    ~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import WhileyLexer
from pygments.token import Token


@pytest.fixture(scope='module')
def lexer():
    yield WhileyLexer()


def test_whiley_operator(lexer):
    fragment = '123 \u2200 x\n'
    tokens = [
        (Token.Literal.Number.Integer, '123'),
        (Token.Text, ' '),
        (Token.Operator, '\u2200'),
        (Token.Text, ' '),
        (Token.Name, 'x'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
