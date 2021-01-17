# -*- coding: utf-8 -*-
"""
    Basic CLexer Test
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import ObjectiveCLexer


@pytest.fixture(scope='module')
def lexer():
    yield ObjectiveCLexer()


def test_literal_number_int(lexer):
    fragment = '@(1);\n'
    expected = [
        (Token.Literal, '@('),
        (Token.Literal.Number.Integer, '1'),
        (Token.Literal, ')'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_literal_number_expression(lexer):
    fragment = '@(1+2);\n'
    expected = [
        (Token.Literal, '@('),
        (Token.Literal.Number.Integer, '1'),
        (Token.Operator, '+'),
        (Token.Literal.Number.Integer, '2'),
        (Token.Literal, ')'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_literal_number_nested_expression(lexer):
    fragment = '@(1+(2+3));\n'
    expected = [
        (Token.Literal, '@('),
        (Token.Literal.Number.Integer, '1'),
        (Token.Operator, '+'),
        (Token.Punctuation, '('),
        (Token.Literal.Number.Integer, '2'),
        (Token.Operator, '+'),
        (Token.Literal.Number.Integer, '3'),
        (Token.Punctuation, ')'),
        (Token.Literal, ')'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_literal_number_bool(lexer):
    fragment = '@NO;\n'
    expected = [
        (Token.Literal.Number, '@NO'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_literal_number_bool_expression(lexer):
    fragment = '@(YES);\n'
    expected = [
        (Token.Literal, '@('),
        (Token.Name.Builtin, 'YES'),
        (Token.Literal, ')'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected


def test_module_import(lexer):
    fragment = '@import ModuleA;\n'
    expected = [
        (Token.Keyword, '@import'),
        (Token.Text, ' '),
        (Token.Name, 'ModuleA'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == expected
