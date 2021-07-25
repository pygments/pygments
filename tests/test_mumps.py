# -*- coding: utf-8 -*-
"""
    Basic MumpsLexer Test
    ~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2020 by the Pygments team, see AUTHORS
    :license: BSD, see LICENSE for details
"""

import pytest

from pygments.token import Whitespace, Keyword, Number, Operator, Text, Name, Punctuation, Comment, String
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

def test_arithmetic_unaryops(lexer):
    # 7.1.4.11 - unaryop (just + and -)
    fragment = 'abs(value) ; Returns the absolute value of a value\n'
    fragment+= ' q:value<0 -value\n'
    fragment+= ' q +value'
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
    fragment = 'spaceship(left,right) ; Returns -1, 0, or 1 based on whether left is less than, equal to, or greater than the right value\n'
    fragment+= ' q:left>right 1\n'
    fragment+= ' q:left<right -1\n'
    fragment+= ' q 0'
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
            (Operator, '>'),
            (Name.Variable, 'right'),
            (Whitespace, ' '),
            (Number, '1'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'left'),
            (Operator, '<'),
            (Name.Variable, 'right'),
            (Whitespace, ' '),
            # Per 7.1.4.2 - numlit - numbers do not start with a sign.
            # Therefore, negative numbers use the '-' operator...
            (Operator, '-'),
            # ...followed by a number
            (Number, '1'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Number, '0'),
            (Text, '\n'),
            ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_binaryops(lexer):
    # Operators defined in 7.2.1
    fragment = ' q 1_1+1/2-1*3#10**2\\3'
    tokens = [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Number, '1'),
            (Operator, '_'),
            (Number, '1'),
            (Operator, '+'),
            (Number, '1'),
            (Operator, '/'),
            (Number, '2'),
            (Operator, '-'),
            (Number, '1'),
            (Operator, '*'),
            (Number, '3'),
            (Operator, '#'),
            (Number, '10'),
            (Operator, '**'),
            (Number, '2'),
            (Operator, '\\'),
            (Number, '3'),
            (Text, '\n')
            ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_stringcomp(lexer):
    # Test string comparison operators in 7.2.2.3
    fragment = 'lowersub(a,b) ; Gets the $ORDER-wise lower subscript\n'
    fragment+= ' q:a="" b\n' # Direct equality test against empty string
    fragment+= ' q:b\']"" a\n' # "Does not follow empty string, i.e. is not the empty string
    fragment+= ' q:a]]b b\n'
    fragment+= ' q a'
    tokens = [
            (Name.Function, 'lowersub'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Comment, '; Gets the $ORDER-wise lower subscript'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'a'),
            (Operator, '='),
            (String, '""'),
            (Whitespace, ' '),
            (Name.Variable, 'b'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'b'),
            (Operator, '\''),
            (Operator, ']'),
            (String, '""'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'a'),
            (Operator, ']]'),
            (Name.Variable, 'b'),
            (Whitespace, ' '),
            (Name.Variable, 'b'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Text, '\n'),
            ]
    assert list(lexer.get_tokens(fragment)) == tokens
    
def test_string_contains(lexer):
    fragment = ' q "ABC123\\-*""[]\'"[b'
    tokens = [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (String, '"ABC123\\-*""[]\'"'),
            (Operator, '['),
            (Name.Variable, 'b'),
            (Text, '\n')
            ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_logicops(lexer):
    assert list(lexer.get_tokens('and(a,b) q a&b')) == [
            (Name.Function, 'and'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Operator, '&'),
            (Name.Variable, 'b'),
            (Text, '\n')
            ]
    assert list(lexer.get_tokens('nand(a,b) q a\'&b')) == [
            (Name.Function, 'nand'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Operator, '\''),
            (Operator, '&'),
            (Name.Variable, 'b'),
            (Text, '\n')
            ]
    assert list(lexer.get_tokens('or(a,b) q a!b')) == [
            (Name.Function, 'or'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Operator, '!'),
            (Name.Variable, 'b'),
            (Text, '\n')
            ]
    assert list(lexer.get_tokens('nor(a,b) q a\'!b')) == [
            (Name.Function, 'nor'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Operator, '\''),
            (Operator, '!'),
            (Name.Variable, 'b'),
            (Text, '\n')
            ]

def testSimplePattern(lexer):
    assert list(lexer.get_tokens(' Q var?1.AN')) == [
            (Whitespace, ' '),
            (Keyword, 'Q'),
            (Whitespace, ' '),
            (Name.Variable, 'var'),
            (Operator, '?'),
            (Number, '1'),
            (Punctuation, '.'),
            (Name.Entity, 'AN'),
            (Text, '\n')
            ]

def testSSNPattern(lexer):
    assert list(lexer.get_tokens(' Q var?3N1"-"2N1"-"4N')) == [
            (Whitespace, ' '),
            (Keyword, 'Q'),
            (Whitespace, ' '),
            (Name.Variable, 'var'),
            (Operator, '?'),
            (Number, '3'),
            (Name.Entity, 'N'),
            (Number, '1'),
            (String, '"-"'),
            (Number, '2'),
            (Name.Entity, 'N'),
            (Number, '1'),
            (String, '"-"'),
            (Number, '4'),
            (Name.Entity, 'N'),
            (Text, '\n')
            ]

def testAlternatorPattern(lexer):
    assert list(lexer.get_tokens(' quit var?1(1"%",1A).AN')) == [
            (Whitespace, ' '),
            (Keyword, 'quit'),
            (Whitespace, ' '),
            (Name.Variable, 'var'),
            (Operator, '?'),
            (Number, '1'),
            (Punctuation, '('),
            (Number, '1'),
            (String, '"%"'),
            (Punctuation, ','),
            (Number, '1'),
            (Name.Entity, 'A'),
            (Punctuation, ')'),
            (Punctuation, '.'),
            (Name.Entity, 'AN'),
            (Text, '\n'),
            ]

def testMProgrammerPatcode(lexer):
    assert list(lexer.get_tokens(' Q S?1YpizzaY')) == [
            (Whitespace, ' '),
            (Keyword, 'Q'),
            (Whitespace, ' '),
            (Name.Variable, 'S'),
            (Operator, '?'),
            (Number, '1'),
            (Name.Entity, 'YpizzaY'),
            (Text, '\n'),
            ]

def testImplementerPatcodes(lexer):
    assert list(lexer.get_tokens(' Q S?1ZbicycleZ')) == [
            (Whitespace, ' '),
            (Keyword, 'Q'),
            (Whitespace, ' '),
            (Name.Variable, 'S'),
            (Operator, '?'),
            (Number, '1'),
            (Name.Entity, 'ZbicycleZ'),
            (Text, '\n'),
            ]
