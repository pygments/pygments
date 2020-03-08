# -*- coding: utf-8 -*-
"""
    Basic Yang Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
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
    fragment = u'namespace urn:test:std:yang;\n'
    tokens = [
        (Token.Keyword, u'namespace'),
        (Token.Text, u' '),
        (Token.Name.Variable, u'urn:test:std:yang'),
        (Token.Punctuation, u';'),
        (Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_namespace_2(lexer):
    """
    namespace-prefix `yang` should be explicitly highlighted
    """
    fragment = u'type yang:counter64;\n'
    tokens = [
        (Token.Keyword, u'type'),
        (Token.Text, u' '),
        (Token.Name.Namespace, u'yang'),
        (Token.Punctuation, u':'),
        (Token.Name.Variable, u'counter64'),
        (Token.Punctuation, u';'),
        (Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_revision_date(lexer):
    """
    Revision-date `2020-08-03` should be explicitly highlighted
    """
    fragment = u'revision 2020-03-08{\n'
    tokens = [
        (Token.Keyword, u'revision'),
        (Token.Text, u' '),
        (Token.Name.Label, u'2020-03-08'),
        (Token.Punctuation, u'{'),
        (Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_integer_value(lexer):
    """
    Integer value `5` should be explicitly highlighted
    """
    fragment = u'value 5;\n'
    tokens = [
        (Token.Keyword, u'value'),
        (Token.Text, u' '),
        (Token.Number.Integer, u'5'),
        (Token.Punctuation, u';'),
        (Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_string_value(lexer):
    """
    String value `"5"` should be not explicitly highlighted
    """
    fragment = u'value "5";\n'
    tokens = [
        (Token.Keyword, u'value'),
        (Token.Text, u' '),
        (Token.String, u'"5"'),
        (Token.Punctuation, u';'),
        (Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_float_value(lexer):
    """
    Float value `1.1` should be explicitly highlighted
    """
    fragment = u'yang-version 1.1;\n'
    tokens = [
        (Token.Keyword, u'yang-version'),
        (Token.Text, u' '),
        (Token.Number.Float, u'1.1'),
        (Token.Punctuation, u';'),
        (Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
