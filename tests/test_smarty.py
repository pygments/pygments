# -*- coding: utf-8 -*-
"""
    Basic SmartyLexer Test
    ~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import SmartyLexer


@pytest.fixture(scope='module')
def lexer():
    yield SmartyLexer()


def test_nested_curly(lexer):
    fragment = '{templateFunction param={anotherFunction} param2=$something}\n'
    tokens = [
        (Token.Comment.Preproc, '{'),
        (Token.Name.Function, 'templateFunction'),
        (Token.Text, ' '),
        (Token.Name.Attribute, 'param'),
        (Token.Operator, '='),
        (Token.Comment.Preproc, '{'),
        (Token.Name.Attribute, 'anotherFunction'),
        (Token.Comment.Preproc, '}'),
        (Token.Text, ' '),
        (Token.Name.Attribute, 'param2'),
        (Token.Operator, '='),
        (Token.Name.Variable, '$something'),
        (Token.Comment.Preproc, '}'),
        (Token.Other, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
