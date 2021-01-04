# -*- coding: utf-8 -*-
"""
    Coq Tests
    ~~~~~~~~~~~~~
    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import CoqLexer
from pygments.token import Token

@pytest.fixture(scope='module')
def lexer():
    yield CoqLexer()

def test_coq_unicode(lexer):
    fragment = 'Check (α ≻ β).\n'
    tokens = [
        (Token.Keyword.Namespace, 'Check'),
        (Token.Text, ' '),
        (Token.Operator, '('),
        (Token.Name, 'α'),
        (Token.Text, ' '),
        (Token.Name.Builtin.Pseudo, '≻'),
        (Token.Text, ' '),
        (Token.Name, 'β'),
        (Token.Operator, ')'),
        (Token.Operator, '.'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
