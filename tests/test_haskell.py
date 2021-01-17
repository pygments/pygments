# -*- coding: utf-8 -*-
"""
    Haskell Tests
    ~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import HaskellLexer
from pygments.token import Token


@pytest.fixture(scope='module')
def lexer():
    yield HaskellLexer()


def test_promoted_names(lexer):
    fragment = "'x ': '[]\n"
    tokens = [
        (Token.Name, '\'x'),
        (Token.Text, ' '),
        (Token.Keyword.Type, '\':'),
        (Token.Text, ' '),
        (Token.Keyword.Type, '\'[]'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
