# -*- coding: utf-8 -*-
"""
    CPP Tests
    ~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import CppLexer
from pygments.token import Token


@pytest.fixture(scope='module')
def lexer():
    yield CppLexer()


def test_good_comment(lexer):
    fragment = u'/* foo */\n'
    tokens = [
        (Token.Comment.Multiline, u'/* foo */'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_open_comment(lexer):
    fragment = u'/* foo\n'
    tokens = [
        (Token.Comment.Multiline, u'/* foo\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
