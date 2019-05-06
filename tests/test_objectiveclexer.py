# -*- coding: utf-8 -*-
"""
    Basic CLexer Test
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import ObjectiveCLexer


@pytest.fixture(scope='module')
def lexer():
    yield ObjectiveCLexer()


def test_literal_number_int(lexer):
    fragment = u'@(1);\n'
    expected = [
        (Token.Literal, u'@('),
        (Token.Literal.Number.Integer, u'1'),
        (Token.Literal, u')'),
        (Token.Punctuation, u';'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_literal_number_expression(lexer):
    fragment = u'@(1+2);\n'
    expected = [
        (Token.Literal, u'@('),
        (Token.Literal.Number.Integer, u'1'),
        (Token.Operator, u'+'),
        (Token.Literal.Number.Integer, u'2'),
        (Token.Literal, u')'),
        (Token.Punctuation, u';'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_literal_number_nested_expression(lexer):
    fragment = u'@(1+(2+3));\n'
    expected = [
        (Token.Literal, u'@('),
        (Token.Literal.Number.Integer, u'1'),
        (Token.Operator, u'+'),
        (Token.Punctuation, u'('),
        (Token.Literal.Number.Integer, u'2'),
        (Token.Operator, u'+'),
        (Token.Literal.Number.Integer, u'3'),
        (Token.Punctuation, u')'),
        (Token.Literal, u')'),
        (Token.Punctuation, u';'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_literal_number_bool(lexer):
    fragment = u'@NO;\n'
    expected = [
        (Token.Literal.Number, u'@NO'),
        (Token.Punctuation, u';'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_literal_number_bool_expression(lexer):
    fragment = u'@(YES);\n'
    expected = [
        (Token.Literal, u'@('),
        (Token.Name.Builtin, u'YES'),
        (Token.Literal, u')'),
        (Token.Punctuation, u';'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_module_import(lexer):
    fragment = u'@import ModuleA;\n'
    expected = [
        (Token.Keyword, u'@import'),
        (Token.Text, u' '),
        (Token.Name, u'ModuleA'),
        (Token.Punctuation, u';'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected
