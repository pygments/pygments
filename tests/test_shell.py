# -*- coding: utf-8 -*-
"""
    Basic Shell Tests
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import BashLexer, BashSessionLexer


@pytest.fixture(scope='module')
def lexer():
    yield BashLexer()


@pytest.fixture(scope='module')
def lexer_session():
    yield BashSessionLexer()


def test_curly_no_escape_and_quotes(lexer):
    fragment = u'echo "${a//["b"]/}"\n'
    tokens = [
        (Token.Name.Builtin, u'echo'),
        (Token.Text, u' '),
        (Token.Literal.String.Double, u'"'),
        (Token.String.Interpol, u'${'),
        (Token.Name.Variable, u'a'),
        (Token.Punctuation, u'//['),
        (Token.Literal.String.Double, u'"b"'),
        (Token.Punctuation, u']/'),
        (Token.String.Interpol, u'}'),
        (Token.Literal.String.Double, u'"'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_curly_with_escape(lexer):
    fragment = u'echo ${a//[\\"]/}\n'
    tokens = [
        (Token.Name.Builtin, u'echo'),
        (Token.Text, u' '),
        (Token.String.Interpol, u'${'),
        (Token.Name.Variable, u'a'),
        (Token.Punctuation, u'//['),
        (Token.Literal.String.Escape, u'\\"'),
        (Token.Punctuation, u']/'),
        (Token.String.Interpol, u'}'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_parsed_single(lexer):
    fragment = u"a=$'abc\\''\n"
    tokens = [
        (Token.Name.Variable, u'a'),
        (Token.Operator, u'='),
        (Token.Literal.String.Single, u"$'abc\\''"),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_short_variable_names(lexer):
    fragment = u'x="$"\ny="$_"\nz="$abc"\n'
    tokens = [
        # single lone $
        (Token.Name.Variable, u'x'),
        (Token.Operator, u'='),
        (Token.Literal.String.Double, u'"'),
        (Token.Text, u'$'),
        (Token.Literal.String.Double, u'"'),
        (Token.Text, u'\n'),
        # single letter shell var
        (Token.Name.Variable, u'y'),
        (Token.Operator, u'='),
        (Token.Literal.String.Double, u'"'),
        (Token.Name.Variable, u'$_'),
        (Token.Literal.String.Double, u'"'),
        (Token.Text, u'\n'),
        # multi-letter user var
        (Token.Name.Variable, u'z'),
        (Token.Operator, u'='),
        (Token.Literal.String.Double, u'"'),
        (Token.Name.Variable, u'$abc'),
        (Token.Literal.String.Double, u'"'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_array_nums(lexer):
    fragment = u'a=(1 2 3)\n'
    tokens = [
        (Token.Name.Variable, u'a'),
        (Token.Operator, u'='),
        (Token.Operator, u'('),
        (Token.Literal.Number, u'1'),
        (Token.Text, u' '),
        (Token.Literal.Number, u'2'),
        (Token.Text, u' '),
        (Token.Literal.Number, u'3'),
        (Token.Operator, u')'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_end_of_line_nums(lexer):
    fragment = u'a=1\nb=2 # comment\n'
    tokens = [
        (Token.Name.Variable, u'a'),
        (Token.Operator, u'='),
        (Token.Literal.Number, u'1'),
        (Token.Text, u'\n'),
        (Token.Name.Variable, u'b'),
        (Token.Operator, u'='),
        (Token.Literal.Number, u'2'),
        (Token.Text, u' '),
        (Token.Comment.Single, u'# comment\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_needs_name(lexer_session):
    fragment = u'$ echo \\\nhi\nhi\n'
    tokens = [
        (Token.Text, u''),
        (Token.Generic.Prompt, u'$'),
        (Token.Text, u' '),
        (Token.Name.Builtin, u'echo'),
        (Token.Text, u' '),
        (Token.Literal.String.Escape, u'\\\n'),
        (Token.Text, u'hi'),
        (Token.Text, u'\n'),
        (Token.Generic.Output, u'hi\n'),
    ]
    assert list(lexer_session.get_tokens(fragment)) == tokens
