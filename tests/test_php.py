# -*- coding: utf-8 -*-
"""
    PHP Tests
    ~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import PhpLexer
from pygments.token import Token


@pytest.fixture(scope='module')
def lexer():
    yield PhpLexer()


def test_string_escaping_run(lexer):
    fragment = '<?php $x="{\\""; ?>\n'
    tokens = [
        (Token.Comment.Preproc, '<?php'),
        (Token.Text, ' '),
        (Token.Name.Variable, '$x'),
        (Token.Operator, '='),
        (Token.Literal.String.Double, '"'),
        (Token.Literal.String.Double, '{'),
        (Token.Literal.String.Escape, '\\"'),
        (Token.Literal.String.Double, '"'),
        (Token.Punctuation, ';'),
        (Token.Text, ' '),
        (Token.Comment.Preproc, '?>'),
        (Token.Other, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
