# -*- coding: utf-8 -*-
"""
    Basic ColdfusionHtmlLexer Test
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import ColdfusionHtmlLexer


@pytest.fixture(scope='module')
def lexer():
    yield ColdfusionHtmlLexer()


def test_basic_comment(lexer):
    fragment = '<!--- cfcomment --->'
    expected = [
        (Token.Comment.Multiline, '<!---'),
        (Token.Comment.Multiline, ' cfcomment '),
        (Token.Comment.Multiline, '--->'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_nested_comment(lexer):
    fragment = '<!--- nested <!--- cfcomment ---> --->'
    expected = [
        (Token.Comment.Multiline, '<!---'),
        (Token.Comment.Multiline, ' nested '),
        (Token.Comment.Multiline, '<!---'),
        (Token.Comment.Multiline, ' cfcomment '),
        (Token.Comment.Multiline, '--->'),
        (Token.Comment.Multiline, ' '),
        (Token.Comment.Multiline, '--->'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected
