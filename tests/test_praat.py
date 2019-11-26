# -*- coding: utf-8 -*-
"""
    Praat lexer tests
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import PraatLexer


@pytest.fixture(scope='module')
def lexer():
    yield PraatLexer()


def test_numeric_assignment(lexer):
    fragment = u'var = -15e4\n'
    tokens = [
        (Token.Text, u'var'),
        (Token.Text, u' '),
        (Token.Operator, u'='),
        (Token.Text, u' '),
        (Token.Operator, u'-'),
        (Token.Literal.Number, u'15e4'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def testStringAssignment(lexer):
    fragment = u'var$ = "foo"\n'
    tokens = [
        (Token.Text, u'var$'),
        (Token.Text, u' '),
        (Token.Operator, u'='),
        (Token.Text, u' '),
        (Token.Literal.String, u'"'),
        (Token.Literal.String, u'foo'),
        (Token.Literal.String, u'"'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_string_escaped_quotes(lexer):
    fragment = u'"it said ""foo"""\n'
    tokens = [
        (Token.Literal.String, u'"'),
        (Token.Literal.String, u'it said '),
        (Token.Literal.String, u'"'),
        (Token.Literal.String, u'"'),
        (Token.Literal.String, u'foo'),
        (Token.Literal.String, u'"'),
        (Token.Literal.String, u'"'),
        (Token.Literal.String, u'"'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function_call(lexer):
    fragment = u'selected("Sound", i+(a*b))\n'
    tokens = [
        (Token.Name.Function, u'selected'),
        (Token.Punctuation, u'('),
        (Token.Literal.String, u'"'),
        (Token.Literal.String, u'Sound'),
        (Token.Literal.String, u'"'),
        (Token.Punctuation, u','),
        (Token.Text, u' '),
        (Token.Text, u'i'),
        (Token.Operator, u'+'),
        (Token.Text, u'('),
        (Token.Text, u'a'),
        (Token.Operator, u'*'),
        (Token.Text, u'b'),
        (Token.Text, u')'),
        (Token.Punctuation, u')'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_broken_unquoted_string(lexer):
    fragment = u'printline string\n... \'interpolated\' string\n'
    tokens = [
        (Token.Keyword, u'printline'),
        (Token.Text, u' '),
        (Token.Literal.String, u'string'),
        (Token.Text, u'\n'),
        (Token.Punctuation, u'...'),
        (Token.Text, u' '),
        (Token.Literal.String.Interpol, u"'interpolated'"),
        (Token.Text, u' '),
        (Token.Literal.String, u'string'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_inline_if(lexer):
    fragment = u'var = if true == 1 then -1 else 0 fi'
    tokens = [
        (Token.Text, u'var'),
        (Token.Text, u' '),
        (Token.Operator, u'='),
        (Token.Text, u' '),
        (Token.Keyword, u'if'),
        (Token.Text, u' '),
        (Token.Text, u'true'),
        (Token.Text, u' '),
        (Token.Operator, u'=='),
        (Token.Text, u' '),
        (Token.Literal.Number, u'1'),
        (Token.Text, u' '),
        (Token.Keyword, u'then'),
        (Token.Text, u' '),
        (Token.Operator, u'-'),
        (Token.Literal.Number, u'1'),
        (Token.Text, u' '),
        (Token.Keyword, u'else'),
        (Token.Text, u' '),
        (Token.Literal.Number, u'0'),
        (Token.Text, u' '),
        (Token.Keyword, u'fi'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_interpolation_boundary(lexer):
    fragment = u'"\'" + "\'"'
    tokens = [
        (Token.Literal.String, u'"'),
        (Token.Literal.String, u"'"),
        (Token.Literal.String, u'"'),
        (Token.Text, u' '),
        (Token.Operator, u'+'),
        (Token.Text, u' '),
        (Token.Literal.String, u'"'),
        (Token.Literal.String, u"'"),
        (Token.Literal.String, u'"'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_interpolated_numeric_indexed(lexer):
    fragment = u"'a[3]'"
    tokens = [
        (Token.Literal.String.Interpol, u"'a[3]'"),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_interpolated_numeric_hash(lexer):
    fragment = u"'a[\"b\"]'"
    tokens = [
        (Token.Literal.String.Interpol, u"'a[\"b\"]'"),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_interpolated_string_indexed(lexer):
    fragment = u"'a$[3]'"
    tokens = [
        (Token.Literal.String.Interpol, u"'a$[3]'"),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_interpolated_string_hash(lexer):
    fragment = u"'a$[\"b\"]'"
    tokens = [
        (Token.Literal.String.Interpol, u"'a$[\"b\"]'"),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_interpolated_numeric_with_precision(lexer):
    fragment = u"'a:3'"
    tokens = [
        (Token.Literal.String.Interpol, u"'a:3'"),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_interpolated_indexed_numeric_with_precision(lexer):
    fragment = u"'a[3]:3'"
    tokens = [
        (Token.Literal.String.Interpol, u"'a[3]:3'"),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_interpolated_local_numeric_with_precision(lexer):
    fragment = u"'a.a:3'"
    tokens = [
        (Token.Literal.String.Interpol, u"'a.a:3'"),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
