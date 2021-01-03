# -*- coding: utf-8 -*-
"""
    Basic Yang Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Operator, Number, Text, Token
from pygments.lexers import YangLexer

@pytest.fixture(scope='module')
def lexer():
    yield YangLexer()

def test_namespace_1(lexer):
    """
    Namespace `urn:test:std:yang` should not be explicitly highlighted
    """
    fragment = 'namespace urn:test:std:yang;\n'
    tokens = [
        (Token.Keyword, 'namespace'),
        (Token.Text.Whitespace, ' '),
        (Token.Name.Variable, 'urn:test:std:yang'),
        (Token.Punctuation, ';'),
        (Token.Text.Whitespace, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_namespace_2(lexer):
    """
    namespace-prefix `yang` should be explicitly highlighted
    """
    fragment = 'type yang:counter64;\n'
    tokens = [
        (Token.Keyword, 'type'),
        (Token.Text.Whitespace, ' '),
        (Token.Name.Namespace, 'yang'),
        (Token.Punctuation, ':'),
        (Token.Name.Variable, 'counter64'),
        (Token.Punctuation, ';'),
        (Token.Text.Whitespace, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_revision_date(lexer):
    """
    Revision-date `2020-08-03` should be explicitly highlighted
    """
    fragment = 'revision 2020-03-08{\n'
    tokens = [
        (Token.Keyword, 'revision'),
        (Token.Text.Whitespace, ' '),
        (Token.Name.Label, '2020-03-08'),
        (Token.Punctuation, '{'),
        (Token.Text.Whitespace, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_integer_value(lexer):
    """
    Integer value `5` should be explicitly highlighted
    """
    fragment = 'value 5;\n'
    tokens = [
        (Token.Keyword, 'value'),
        (Token.Text.Whitespace, ' '),
        (Token.Number.Integer, '5'),
        (Token.Punctuation, ';'),
        (Token.Text.Whitespace, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_string_value(lexer):
    """
    String value `"5"` should be not explicitly highlighted
    """
    fragment = 'value "5";\n'
    tokens = [
        (Token.Keyword, 'value'),
        (Token.Text.Whitespace, ' '),
        (Token.String.Double, '"5"'),
        (Token.Punctuation, ';'),
        (Token.Text.Whitespace, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_float_value(lexer):
    """
    Float value `1.1` should be explicitly highlighted
    """
    fragment = 'yang-version 1.1;\n'
    tokens = [
        (Token.Keyword, 'yang-version'),
        (Token.Text.Whitespace, ' '),
        (Token.Number.Float, '1.1'),
        (Token.Punctuation, ';'),
        (Token.Text.Whitespace, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
