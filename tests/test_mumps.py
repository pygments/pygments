# -*- coding: utf-8 -*-
"""
    Basic MumpsLexer Test
    ~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2020 by the Pygments team, see AUTHORS
    :license: BSD, see LICENSE for details
"""

import pytest

from pygments.token import Whitespace, Keyword, Number, Operator, Text, Name, Punctuation, Comment
from pygments.lexers import MumpsLexer

@pytest.fixture(scope='module')
def lexer():
    yield MumpsLexer()

def test_quit_line(lexer):
    fragment = ' q'
    tokens = [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_quit_value_line(lexer):
    fragment = ' QUIT 1'
    tokens = [
        (Whitespace, ' '),
        (Keyword, 'QUIT'),
        (Whitespace, ' '),
        (Number, '1'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_quit_morph_line(lexer):
    fragment = ' q:$QUIT 1 Q'
    tokens = [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Operator, ':'),
        (Keyword, '$QUIT'),
        (Whitespace, ' '),
        (Number, '1'),
        (Whitespace, ' '),
        (Keyword, 'Q'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_stub_subroutine(lexer):
    fragment = 'stub q'
    tokens = [
        (Name.Label, 'stub'),
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Text, '\n')
        ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_simple_function(lexer):
    fragment = 'true() q 1'
    tokens = [
        (Name.Function, 'true'),
        (Punctuation, '('),
        (Punctuation, ')'),
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Whitespace, ' '),
        (Number, '1'),
        (Text, '\n')
        ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_simple_function(lexer):
    fragment = 'abs(value) ; Returns the absolute value of a value\n q:value<0 -value\n q +value'
    tokens = [
        (Name.Function, 'abs'),
	(Punctuation, '('),
	(Name.Variable, 'value'),
	(Punctuation, ')'),
	(Whitespace, ' '),
	(Comment, '; Returns the absolute value of a value'),
	(Text, '\n'),
	(Whitespace, ' '),
	(Keyword, 'q'),
	(Operator, ':'),
	(Name.Variable, 'value'),
	(Operator, '<'),
	(Number, '0'),
	(Whitespace, ' '),
	(Operator, '-'),
	(Name.Variable, 'value'),
	(Text, '\n'),
	(Whitespace, ' '),
	(Keyword, 'q'),
	(Whitespace, ' '),
	(Operator, '+'),
	(Name.Variable, 'value'),
	(Text, '\n'),
	]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_compare(lexer):
    fragment = 'spaceship(left,right) ; Returns -1, 0, or 1 based on whether left is less than, equal to, or greater than the right value\n q:left=right 0\n q:left>right 1\n q -1'
    tokens = [
            (Name.Function, 'spaceship'),
            (Punctuation, '('),
            (Name.Variable, 'left'),
            (Punctuation, ','),
            (Name.Variable, 'right'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Comment, '; Returns -1, 0, or 1 based on whether left is less than, equal to, or greater than the right value'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'left'),
            (Operator, '='),
            (Name.Variable, 'right'),
            (Whitespace, ' '),
            (Number, '0'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'left'),
            (Operator, '>'),
            (Name.Variable, 'right'),
            (Whitespace, ' '),
            (Number, '1'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            # Per 7.1.4.2 - numlit - numbers do not start with a sign.
            # Therefore, negative numbers use the '-' operator...
            (Operator, '-'),
            # ...followed by a number
            (Number, '1'),
            (Text, '\n'),
            ]
    assert list(lexer.get_tokens(fragment)) == tokens
    

