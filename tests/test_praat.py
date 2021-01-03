# -*- coding: utf-8 -*-
"""
    Praat lexer tests
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import PraatLexer


@pytest.fixture(scope='module')
def lexer():
    yield PraatLexer()


def test_numeric_assignment(lexer):
    fragment = 'var = -15e4\n'
    tokens = [
        (Token.Text, 'var'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Operator, '-'),
        (Token.Literal.Number, '15e4'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def testStringAssignment(lexer):
    fragment = 'var$ = "foo"\n'
    tokens = [
        (Token.Text, 'var$'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Literal.String, '"'),
        (Token.Literal.String, 'foo'),
        (Token.Literal.String, '"'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_string_escaped_quotes(lexer):
    fragment = '"it said ""foo"""\n'
    tokens = [
        (Token.Literal.String, '"'),
        (Token.Literal.String, 'it said '),
        (Token.Literal.String, '"'),
        (Token.Literal.String, '"'),
        (Token.Literal.String, 'foo'),
        (Token.Literal.String, '"'),
        (Token.Literal.String, '"'),
        (Token.Literal.String, '"'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function_call(lexer):
    fragment = 'selected("Sound", i+(a*b))\n'
    tokens = [
        (Token.Name.Function, 'selected'),
        (Token.Punctuation, '('),
        (Token.Literal.String, '"'),
        (Token.Literal.String, 'Sound'),
        (Token.Literal.String, '"'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Text, 'i'),
        (Token.Operator, '+'),
        (Token.Text, '('),
        (Token.Text, 'a'),
        (Token.Operator, '*'),
        (Token.Text, 'b'),
        (Token.Text, ')'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_broken_unquoted_string(lexer):
    fragment = 'printline string\n... \'interpolated\' string\n'
    tokens = [
        (Token.Keyword, 'printline'),
        (Token.Text, ' '),
        (Token.Literal.String, 'string'),
        (Token.Text, '\n'),
        (Token.Punctuation, '...'),
        (Token.Text, ' '),
        (Token.Literal.String.Interpol, "'interpolated'"),
        (Token.Text, ' '),
        (Token.Literal.String, 'string'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_inline_if(lexer):
    fragment = 'var = if true == 1 then -1 else 0 fi'
    tokens = [
        (Token.Text, 'var'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Keyword, 'if'),
        (Token.Text, ' '),
        (Token.Text, 'true'),
        (Token.Text, ' '),
        (Token.Operator, '=='),
        (Token.Text, ' '),
        (Token.Literal.Number, '1'),
        (Token.Text, ' '),
        (Token.Keyword, 'then'),
        (Token.Text, ' '),
        (Token.Operator, '-'),
        (Token.Literal.Number, '1'),
        (Token.Text, ' '),
        (Token.Keyword, 'else'),
        (Token.Text, ' '),
        (Token.Literal.Number, '0'),
        (Token.Text, ' '),
        (Token.Keyword, 'fi'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_interpolation_boundary(lexer):
    fragment = '"\'" + "\'"'
    tokens = [
        (Token.Literal.String, '"'),
        (Token.Literal.String, "'"),
        (Token.Literal.String, '"'),
        (Token.Text, ' '),
        (Token.Operator, '+'),
        (Token.Text, ' '),
        (Token.Literal.String, '"'),
        (Token.Literal.String, "'"),
        (Token.Literal.String, '"'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_interpolated_numeric_indexed(lexer):
    fragment = "'a[3]'"
    tokens = [
        (Token.Literal.String.Interpol, "'a[3]'"),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_interpolated_numeric_hash(lexer):
    fragment = "'a[\"b\"]'"
    tokens = [
        (Token.Literal.String.Interpol, "'a[\"b\"]'"),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_interpolated_string_indexed(lexer):
    fragment = "'a$[3]'"
    tokens = [
        (Token.Literal.String.Interpol, "'a$[3]'"),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_interpolated_string_hash(lexer):
    fragment = "'a$[\"b\"]'"
    tokens = [
        (Token.Literal.String.Interpol, "'a$[\"b\"]'"),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_interpolated_numeric_with_precision(lexer):
    fragment = "'a:3'"
    tokens = [
        (Token.Literal.String.Interpol, "'a:3'"),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_interpolated_indexed_numeric_with_precision(lexer):
    fragment = "'a[3]:3'"
    tokens = [
        (Token.Literal.String.Interpol, "'a[3]:3'"),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_interpolated_local_numeric_with_precision(lexer):
    fragment = "'a.a:3'"
    tokens = [
        (Token.Literal.String.Interpol, "'a.a:3'"),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
