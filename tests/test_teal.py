# -*- coding: utf-8 -*-
"""
    Basic TealLexer Tests
    ~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
""" 

import pytest

from pygments.lexers import TealLexer
from pygments.token import Token

@pytest.fixture(scope="module")
def lexer():
    yield TealLexer()


def test_strings(lexer):
    fragment = r'a "abc\x123\n\"de//f"'
    tokens = [
        (Token.Name.Function, 'a'),
        (Token.Text, ' '),
        (Token.String, '"'),
        (Token.String, 'abc'),
        (Token.String.Escape, r'\x12'),
        (Token.String, '3'),
        (Token.String.Escape, r'\n'),
        (Token.String.Escape, r'\"'),
        (Token.Literal.String, 'de//f'),
        (Token.Literal.String, '"'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_comments(lexer):
    fragment = '''a//c1
 //c2
label://c3
a // c4
label: implicit comment'''
    tokens = [
        (Token.Name.Function, 'a'),
        (Token.Comment.Single, '//c1'),
        (Token.Text, '\n'),
        (Token.Text, ' '),
        (Token.Comment.Single, '//c2'),
        (Token.Text, '\n'),
        (Token.Name.Function, 'label:'),
        (Token.Comment.Single, '//c3'),
        (Token.Text, '\n'),
        (Token.Name.Function, 'a'),
        (Token.Text, ' '),
        (Token.Comment.Single, '// c4'),
        (Token.Text, '\n'),
        (Token.Name.Label, 'label:'),
        (Token.Comment.Single, ' implicit comment'),
        (Token.Text, '\n'),
    ]

def test_literals(lexer):
    fragment = '''a 0x1AaAF
a 7777777777777777777777777777777777777777777777777774MSJUVU
a base32(aB/c23=)
a b64 aB/c23='''
    tokens = [
        (Token.Name.Function, 'a'),
        (Token.Text, ' '),
        (Token.Literal.Number.Hex, '0x1AaAF'),
        (Token.Text, '\n'),
        (Token.Name.Function, 'a'),
        (Token.Text, ' '),
        (Token.Literal.Number, '7777777777777777777777777777777777777777777777777774MSJUVU'),
        (Token.Text, '\n'),
        (Token.Name.Function, 'a'),
        (Token.Text, ' '),
        (Token.Literal.String.Affix, 'base32'),
        (Token.Literal.String.Other, '(aB/c23=)'),
        (Token.Text, '\n'),
        (Token.Name.Function, 'a'),
        (Token.Text, ' '),
        (Token.Literal.String.Affix, 'b64 '),
        (Token.Literal.String.Other, 'aB/c23='),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
