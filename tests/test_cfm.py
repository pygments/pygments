# -*- coding: utf-8 -*-
"""
    Basic ColdfusionHtmlLexer Test
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import ColdfusionHtmlLexer


@pytest.fixture(scope='module')
def lexer():
    yield ColdfusionHtmlLexer()


def test_basic_comment(lexer):
    fragment = u'<!--- cfcomment --->'
    expected = [
        (Token.Text, u''),
        (Token.Comment.Multiline, u'<!---'),
        (Token.Comment.Multiline, u' cfcomment '),
        (Token.Comment.Multiline, u'--->'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_nested_comment(lexer):
    fragment = u'<!--- nested <!--- cfcomment ---> --->'
    expected = [
        (Token.Text, u''),
        (Token.Comment.Multiline, u'<!---'),
        (Token.Comment.Multiline, u' nested '),
        (Token.Comment.Multiline, u'<!---'),
        (Token.Comment.Multiline, u' cfcomment '),
        (Token.Comment.Multiline, u'--->'),
        (Token.Comment.Multiline, u' '),
        (Token.Comment.Multiline, u'--->'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected
