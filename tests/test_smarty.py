# -*- coding: utf-8 -*-
"""
    Basic SmartyLexer Test
    ~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import SmartyLexer


@pytest.fixture(scope='module')
def lexer():
    yield SmartyLexer()


def test_nested_curly(lexer):
    fragment = u'{templateFunction param={anotherFunction} param2=$something}\n'
    tokens = [
        (Token.Comment.Preproc, u'{'),
        (Token.Name.Function, u'templateFunction'),
        (Token.Text, u' '),
        (Token.Name.Attribute, u'param'),
        (Token.Operator, u'='),
        (Token.Comment.Preproc, u'{'),
        (Token.Name.Attribute, u'anotherFunction'),
        (Token.Comment.Preproc, u'}'),
        (Token.Text, u' '),
        (Token.Name.Attribute, u'param2'),
        (Token.Operator, u'='),
        (Token.Name.Variable, u'$something'),
        (Token.Comment.Preproc, u'}'),
        (Token.Other, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
