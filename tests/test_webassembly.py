# -*- coding: utf-8 -*-
"""
    Basic WatLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest
from pygments.token import Token
from pygments.lexers import WatLexer

@pytest.fixture(scope='module')
def lexer():
    yield WatLexer()

def test_variable_name_pattern(lexer):
    fragment = "$ABCabc123!#$%&'*+./:<=>?@\\^_`|~-A\n"
    tokens = [
        (Token.Name.Variable, "$ABCabc123!#$%&'*+./:<=>?@\\^_`|~-A"),
        (Token.Text, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_i32_const_is_builtin(lexer):
    fragment = 'i32.const\n'
    tokens = [
        (Token.Name.Builtin, 'i32.const'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_multiline_comment(lexer):
    fragment = '(;\ncomment\n;)\n'
    tokens = [
        (Token.Comment.Multiline, '(;'),
        (Token.Comment.Multiline, '\ncomment\n'),
        (Token.Comment.Multiline, ';)'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_comment_with_open_paren(lexer):
    fragment = '(; comment with ( open paren ;)\n'
    tokens = [
        (Token.Comment.Multiline, '(;'),
        (Token.Comment.Multiline, ' comment with ( open paren '),
        (Token.Comment.Multiline, ';)'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_comment_with_semicolon(lexer):
    fragment = '(; comment with ; semicolon ;)\n'
    tokens = [
        (Token.Comment.Multiline, '(;'),
        (Token.Comment.Multiline, ' comment with ; semicolon '),
        (Token.Comment.Multiline, ';)'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_nested_comment(lexer):
    fragment = '(;\nnested(;;)comment\n;)\n'
    tokens = [
        (Token.Comment.Multiline, '(;'),
        (Token.Comment.Multiline, '\nnested'),
        (Token.Comment.Multiline, '(;'),
        (Token.Comment.Multiline, ';)'),
        (Token.Comment.Multiline, 'comment\n'),
        (Token.Comment.Multiline, ';)'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_string_with_escape(lexer):
    fragment = '"string\\t"\n'
    tokens = [
        (Token.Literal.String.Double, '"'),
        (Token.Literal.String.Double, 'string'),
        (Token.Literal.String.Escape, '\\t'),
        (Token.Literal.String.Double, '"'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_align_and_offset_accept_hexadecimal_numbers(lexer):
    fragment = 'i32.store offset=0xdeadbeef align=0x1\n'
    tokens = [
        (Token.Name.Builtin, 'i32.store'),
        (Token.Text, ' '),
        (Token.Keyword, 'offset'),
        (Token.Operator, '='),
        (Token.Literal.Number.Hex, '0xdeadbeef'),
        (Token.Text, ' '),
        (Token.Keyword, 'align'),
        (Token.Operator, '='),
        (Token.Literal.Number.Hex, '0x1'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens