# -*- coding: utf-8 -*-
"""
    Data Tests
    ~~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import JsonLexer, JsonBareObjectLexer, YamlLexer
from pygments.token import Token


@pytest.fixture(scope='module')
def lexer_json():
    yield JsonLexer()


@pytest.fixture(scope='module')
def lexer_bare():
    yield JsonBareObjectLexer()


@pytest.fixture(scope='module')
def lexer_yaml():
    yield YamlLexer()


def test_basic_json(lexer_json):
    fragment = u'{"foo": "bar", "foo2": [1, 2, 3]}\n'
    tokens = [
        (Token.Punctuation, u'{'),
        (Token.Name.Tag, u'"foo"'),
        (Token.Punctuation, u':'),
        (Token.Text, u' '),
        (Token.Literal.String.Double, u'"bar"'),
        (Token.Punctuation, u','),
        (Token.Text, u' '),
        (Token.Name.Tag, u'"foo2"'),
        (Token.Punctuation, u':'),
        (Token.Text, u' '),
        (Token.Punctuation, u'['),
        (Token.Literal.Number.Integer, u'1'),
        (Token.Punctuation, u','),
        (Token.Text, u' '),
        (Token.Literal.Number.Integer, u'2'),
        (Token.Punctuation, u','),
        (Token.Text, u' '),
        (Token.Literal.Number.Integer, u'3'),
        (Token.Punctuation, u']'),
        (Token.Punctuation, u'}'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer_json.get_tokens(fragment)) == tokens


def test_basic_bare(lexer_bare):
    # This is the same as testBasic for JsonLexer above, except the
    # enclosing curly braces are removed.
    fragment = u'"foo": "bar", "foo2": [1, 2, 3]\n'
    tokens = [
        (Token.Name.Tag, u'"foo"'),
        (Token.Punctuation, u':'),
        (Token.Text, u' '),
        (Token.Literal.String.Double, u'"bar"'),
        (Token.Punctuation, u','),
        (Token.Text, u' '),
        (Token.Name.Tag, u'"foo2"'),
        (Token.Punctuation, u':'),
        (Token.Text, u' '),
        (Token.Punctuation, u'['),
        (Token.Literal.Number.Integer, u'1'),
        (Token.Punctuation, u','),
        (Token.Text, u' '),
        (Token.Literal.Number.Integer, u'2'),
        (Token.Punctuation, u','),
        (Token.Text, u' '),
        (Token.Literal.Number.Integer, u'3'),
        (Token.Punctuation, u']'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer_bare.get_tokens(fragment)) == tokens


def test_closing_curly(lexer_bare):
    # This can be an Error token, but should not be a can't-pop-from-stack
    # exception.
    fragment = '}"a"\n'
    tokens = [
        (Token.Error, '}'),
        (Token.Name.Tag, '"a"'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_bare.get_tokens(fragment)) == tokens


def test_closing_curly_in_value(lexer_bare):
    fragment = '"": ""}\n'
    tokens = [
        (Token.Name.Tag, '""'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Literal.String.Double, '""'),
        (Token.Error, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_bare.get_tokens(fragment)) == tokens


def test_yaml(lexer_yaml):
    # Bug #1528: This previously parsed 'token # innocent' as a tag
    fragment = u'here: token # innocent: comment\n'
    tokens = [
        (Token.Name.Tag, u'here'),
        (Token.Punctuation, u':'),
        (Token.Text, u' '),
        (Token.Literal.Scalar.Plain, u'token'),
        (Token.Text, u' '),
        (Token.Comment.Single, u'# innocent: comment'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer_yaml.get_tokens(fragment)) == tokens
