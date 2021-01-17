# -*- coding: utf-8 -*-
"""
    R Tests
    ~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import SLexer
from pygments.token import Token, Name, Punctuation


@pytest.fixture(scope='module')
def lexer():
    yield SLexer()


def test_call(lexer):
    fragment = 'f(1, a)\n'
    tokens = [
        (Name.Function, 'f'),
        (Punctuation, '('),
        (Token.Literal.Number, '1'),
        (Punctuation, ','),
        (Token.Text, ' '),
        (Token.Name, 'a'),
        (Punctuation, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_name1(lexer):
    fragment = '._a_2.c'
    tokens = [
        (Name, '._a_2.c'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_name2(lexer):
    # Invalid names are valid if backticks are used
    fragment = '`.1 blah`'
    tokens = [
        (Name, '`.1 blah`'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_name3(lexer):
    # Internal backticks can be escaped
    fragment = '`.1 \\` blah`'
    tokens = [
        (Name, '`.1 \\` blah`'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_custom_operator(lexer):
    fragment = '7 % and % 8'
    tokens = [
        (Token.Literal.Number, '7'),
        (Token.Text, ' '),
        (Token.Operator, '% and %'),
        (Token.Text, ' '),
        (Token.Literal.Number, '8'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_indexing(lexer):
    fragment = 'a[1]'
    tokens = [
        (Token.Name, 'a'),
        (Token.Punctuation, '['),
        (Token.Literal.Number, '1'),
        (Token.Punctuation, ']'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_dot_name(lexer):
    fragment = '. <- 1'
    tokens = [
        (Token.Name, '.'),
        (Token.Text, ' '),
        (Token.Operator, '<-'),
        (Token.Text, ' '),
        (Token.Literal.Number, '1'),
        (Token.Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_dot_indexing(lexer):
    fragment = '.[1]'
    tokens = [
        (Token.Name, '.'),
        (Token.Punctuation, '['),
        (Token.Literal.Number, '1'),
        (Token.Punctuation, ']'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
