# -*- coding: utf-8 -*-
"""
    OMG IDL Tests
    ~~~~~~~~~~~~~

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import OmgIdlLexer
from pygments.token import Token


@pytest.fixture(scope="module")
def lexer():
    yield OmgIdlLexer()


def test_annotation_named_params(lexer):
    """Asserts that annotation named parameters are different from normal
    "scoped_name =" lexing, which uses Name.Constant.
    """

    fragment = '@mod::anno(value = const_a) const short const_b = const_a;'
    tokens = [
        (Token.Name.Decorator, '@mod::anno'), (Token.Punctuation, '('),
        (Token.Name, 'value'), (Token.Text, ' '), (Token.Punctuation, '='),
        (Token.Text, ' '), (Token.Name, 'const_a'), (Token.Punctuation, ')'),
        (Token.Text, ' '), (Token.Keyword.Declaration, 'const'), (Token.Text, ' '),
        (Token.Keyword.Type, 'short'), (Token.Text, ' '), (Token.Name.Constant, 'const_b'),
        (Token.Text.Whitespace, ' '), (Token.Operator, '='), (Token.Text, ' '),
        (Token.Name, 'const_a'), (Token.Punctuation, ';'), (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_enumerators(lexer):
    """Asserts that enumerators uses Name.Constant instead of just Name.
    """

    fragment = 'enum Enum_t {enum_a, enum_b};'
    tokens = [
        (Token.Keyword, 'enum'), (Token.Text.Whitespace, ' '), (Token.Name.Class, 'Enum_t'),
        (Token.Text, ' '), (Token.Punctuation, '{'), (Token.Name.Constant, 'enum_a'),
        (Token.Punctuation, ','), (Token.Text, ' '), (Token.Name.Constant, 'enum_b'),
        (Token.Punctuation, '}'), (Token.Punctuation, ';'), (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
