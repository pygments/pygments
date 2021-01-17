# -*- coding: utf-8 -*-
"""
    Properties Tests
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.configs import PropertiesLexer
from pygments.token import Token


@pytest.fixture(scope='module')
def lexer():
    yield PropertiesLexer()


def test_comments(lexer):
    """
    Assures lines lead by either # or ! are recognized as a comment
    """
    fragment = '! a comment\n# also a comment\n'
    tokens = [
        (Token.Comment, '! a comment'),
        (Token.Text, '\n'),
        (Token.Comment, '# also a comment'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_leading_whitespace_comments(lexer):
    fragment = '    # comment\n'
    tokens = [
        (Token.Text, '    '),
        (Token.Comment, '# comment'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_escaped_space_in_key(lexer):
    fragment = 'key = value\n'
    tokens = [
        (Token.Name.Attribute, 'key'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Literal.String, 'value'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_escaped_space_in_value(lexer):
    fragment = 'key = doubleword\\ value\n'
    tokens = [
        (Token.Name.Attribute, 'key'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Literal.String, 'doubleword\\ value'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_space_delimited_kv_pair(lexer):
    fragment = 'key value\n'
    tokens = [
        (Token.Name.Attribute, 'key'),
        (Token.Text, ' '),
        (Token.Literal.String, 'value\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_just_key(lexer):
    fragment = 'justkey\n'
    tokens = [
        (Token.Name.Attribute, 'justkey'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_just_key_with_space(lexer):
    fragment = 'just\\ key\n'
    tokens = [
        (Token.Name.Attribute, 'just\\ key'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
