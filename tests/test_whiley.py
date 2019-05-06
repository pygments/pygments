# -*- coding: utf-8 -*-
"""
    Whiley Test
    ~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import WhileyLexer
from pygments.token import Token


@pytest.fixture(scope='module')
def lexer():
    yield WhileyLexer()


def test_whiley_operator(lexer):
    fragment = u'123 \u2200 x\n'
    tokens = [
        (Token.Literal.Number.Integer, u'123'),
        (Token.Text, u' '),
        (Token.Operator, u'\u2200'),
        (Token.Text, u' '),
        (Token.Name, u'x'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
