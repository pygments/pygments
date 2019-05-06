# -*- coding: utf-8 -*-
"""
    R Tests
    ~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import SLexer
from pygments.token import Token, Name, Punctuation


@pytest.fixture(scope='module')
def lexer():
    yield SLexer()


def test_call(lexer):
    fragment = u'f(1, a)\n'
    tokens = [
        (Name.Function, u'f'),
        (Punctuation, u'('),
        (Token.Literal.Number, u'1'),
        (Punctuation, u','),
        (Token.Text, u' '),
        (Token.Name, u'a'),
        (Punctuation, u')'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_name1(lexer):
    fragment = u'._a_2.c'
    tokens = [
        (Name, u'._a_2.c'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_name2(lexer):
    # Invalid names are valid if backticks are used
    fragment = u'`.1 blah`'
    tokens = [
        (Name, u'`.1 blah`'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_name3(lexer):
    # Internal backticks can be escaped
    fragment = u'`.1 \\` blah`'
    tokens = [
        (Name, u'`.1 \\` blah`'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_custom_operator(lexer):
    fragment = u'7 % and % 8'
    tokens = [
        (Token.Literal.Number, u'7'),
        (Token.Text, u' '),
        (Token.Operator, u'% and %'),
        (Token.Text, u' '),
        (Token.Literal.Number, u'8'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
