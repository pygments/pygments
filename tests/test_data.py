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
    fragment = '{"foo": "bar", "foo2": [1, 2, 3], "\\u0123": "\\u0123"}\n'
    tokens = [
        (Token.Punctuation, '{'),
        (Token.Name.Tag, '"foo"'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Literal.String.Double, '"bar"'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Name.Tag, '"foo2"'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Punctuation, '['),
        (Token.Literal.Number.Integer, '1'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '2'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '3'),
        (Token.Punctuation, ']'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Name.Tag, '"\\u0123"'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Literal.String.Double, '"\\u0123"'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_json.get_tokens(fragment)) == tokens


def test_json_escape_backtracking(lexer_json):
    # This tests that an (invalid) sequence of escapes doesn't cause the lexer
    # to fall into catastrophic backtracking. unfortunately, if it's broken
    # this test will hang and that's how we know it's broken :(
    fragment = r'{"\u00D0000\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\63CD'
    tokens = [
        (Token.Punctuation, '{'),
        (Token.Error, r'"'),
        (Token.Error, '\\'),
        (Token.Error, r'u'),
        (Token.Error, r'0'),
        (Token.Error, r'0'),
        (Token.Error, r'D'),
        (Token.Error, r'0'),
        (Token.Error, r'0'),
        (Token.Error, r'0'),
        (Token.Error, r'0')
    ] + [(Token.Error, '\\')] * 178 + [
        (Token.Error, r'6'),
        (Token.Error, r'3'),
        (Token.Error, r'C'),
        (Token.Error, r'D'),
        (Token.Text, '\n')]

    assert list(lexer_json.get_tokens(fragment)) == tokens


def test_basic_bare(lexer_bare):
    # This is the same as testBasic for JsonLexer above, except the
    # enclosing curly braces are removed.
    fragment = '"foo": "bar", "foo2": [1, 2, 3]\n'
    tokens = [
        (Token.Name.Tag, '"foo"'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Literal.String.Double, '"bar"'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Name.Tag, '"foo2"'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Punctuation, '['),
        (Token.Literal.Number.Integer, '1'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '2'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '3'),
        (Token.Punctuation, ']'),
        (Token.Text, '\n'),
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
    fragment = 'here: token # innocent: comment\n'
    tokens = [
        (Token.Name.Tag, 'here'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Literal.Scalar.Plain, 'token'),
        (Token.Text, ' '),
        (Token.Comment.Single, '# innocent: comment'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_yaml.get_tokens(fragment)) == tokens
