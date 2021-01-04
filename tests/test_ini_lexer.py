# -*- coding: utf-8 -*-
"""
    Pygments INI lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest
from pygments.lexers.configs import IniLexer
from pygments.token import Token, String, Keyword, Name, Operator


@pytest.fixture(scope='module')
def lexer():
    yield IniLexer()


def test_indented_entries(lexer):
    fragment = \
        '[section]\n' \
        '    key1=value1\n' \
        '    key2=value2\n'
    tokens = [
        (Keyword, '[section]'),
        (Token.Text, '\n    '),
        (Name.Attribute, 'key1'),
        (Operator, '='),
        (String, 'value1'),
        (Token.Text, '\n    '),
        (Name.Attribute, 'key2'),
        (Operator, '='),
        (String, 'value2'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = \
        '[section]\n' \
        '    key1 = value1\n' \
        '    key2 = value2\n'
    tokens = [
        (Keyword, '[section]'),
        (Token.Text, '\n    '),
        (Name.Attribute, 'key1'),
        (Token.Text, ' '),
        (Operator, '='),
        (Token.Text, ' '),
        (String, 'value1'),
        (Token.Text, '\n    '),
        (Name.Attribute, 'key2'),
        (Token.Text, ' '),
        (Operator, '='),
        (Token.Text, ' '),
        (String, 'value2'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = \
        '[section]\n' \
        '    key 1 = value 1\n' \
        '    key 2 = value 2\n'
    tokens = [
        (Keyword, '[section]'),
        (Token.Text, '\n    '),
        (Name.Attribute, 'key 1'),
        (Token.Text, ' '),
        (Operator, '='),
        (Token.Text, ' '),
        (String, 'value 1'),
        (Token.Text, '\n    '),
        (Name.Attribute, 'key 2'),
        (Token.Text, ' '),
        (Operator, '='),
        (Token.Text, ' '),
        (String, 'value 2'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
