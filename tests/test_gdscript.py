# -*- coding: utf-8 -*-
"""
    GDScript Tests
    ~~~~~~~~~~~~~~

    :copyright: Copyright 2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import GDScriptLexer
from pygments.token import Token


@pytest.fixture(scope="module")
def lexer():
    yield GDScriptLexer()


def test_variable_declaration_and_assigment(lexer):
    fragment = "var abc = 5.4"
    tokens = [
        (Token.Keyword, "var"),
        (Token.Text, " "),
        (Token.Name, "abc"),
        (Token.Text, " "),
        (Token.Operator, "="),
        (Token.Text, " "),
        (Token.Number.Float, "5.4"),
        (Token.Text, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_simple_function(lexer):
    fragment = "func abc(arg):\n\tprint(\"Hello, World!\")"
    tokens = [
        (Token.Keyword, "func"),
        (Token.Text, " "),
        (Token.Name, "abc"),
        (Token.Punctuation, "("),
        (Token.Name, "arg"),
        (Token.Punctuation, ")"),
        (Token.Punctuation, ":"),
        (Token.Text, "\n"),
        (Token.Text, "\t"),
        (Token.Name.Builtin, "print"),
        (Token.Punctuation, "("),
        (Token.Literal.String.Double, "\""),
        (Token.Literal.String.Double, "Hello, World!"),
        (Token.Literal.String.Double, "\""),
        (Token.Punctuation, ")"),
        (Token.Text, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function_with_types(lexer):
    fragment = "func abc(arg: String) -> void:\n\tprint(\"Hello\", arg)"
    tokens = [
        (Token.Keyword, "func"),
        (Token.Text, " "),
        (Token.Name, "abc"),
        (Token.Punctuation, "("),
        (Token.Name, "arg"),
        (Token.Punctuation, ":"),
        (Token.Text, " "),
        (Token.Name.Builtin.Type, "String"),
        (Token.Punctuation, ")"),
        (Token.Text, " "),
        (Token.Operator, "-"),
        (Token.Operator, ">"),
        (Token.Text, " "),
        (Token.Name, "void"),
        (Token.Punctuation, ":"),
        (Token.Text, "\n"),
        (Token.Text, "\t"),
        (Token.Name.Builtin, "print"),
        (Token.Punctuation, "("),
        (Token.Literal.String.Double, "\""),
        (Token.Literal.String.Double, "Hello"),
        (Token.Literal.String.Double, "\""),
        (Token.Punctuation, ","),
        (Token.Text, " "),
        (Token.Name, "arg"),
        (Token.Punctuation, ")"),
        (Token.Text, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_signal(lexer):
    fragment = "signal sig (arg1, arg2)"
    tokens = [
        (Token.Keyword, "signal"),
        (Token.Text, " "),
        (Token.Name, "sig"),
        (Token.Text, " "),
        (Token.Punctuation, "("),
        (Token.Name, "arg1"),
        (Token.Punctuation, ","),
        (Token.Text, " "),
        (Token.Name, "arg2"),
        (Token.Punctuation, ")"),
        (Token.Text, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_export_array(lexer):
    fragment = "export (Array, AudioStream) var streams"
    tokens = [
        (Token.Keyword, "export"),
        (Token.Text, " "),
        (Token.Punctuation, "("),
        (Token.Name.Builtin.Type, "Array"),
        (Token.Punctuation, ","),
        (Token.Text, " "),
        (Token.Name, "AudioStream"),
        (Token.Punctuation, ")"),
        (Token.Text, " "),
        (Token.Keyword, "var"),
        (Token.Text, " "),
        (Token.Name, "streams"),
        (Token.Text, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_inner_class(lexer):
    fragment = "class InnerClass:\n\tvar a = 5"
    tokens = [
        (Token.Keyword, "class"),
        (Token.Text, " "),
        (Token.Name, "InnerClass"),
        (Token.Punctuation, ":"),
        (Token.Text, "\n"),
        (Token.Text, "\t"),
        (Token.Keyword, "var"),
        (Token.Text, " "),
        (Token.Name, "a"),
        (Token.Text, " "),
        (Token.Operator, "="),
        (Token.Text, " "),
        (Token.Literal.Number.Integer, "5"),
        (Token.Text, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_comment(lexer):
    fragment = "# Comment"
    tokens = [
        (Token.Comment.Single, "# Comment"),
        (Token.Text, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_multiline_string(lexer):
    fragment = '"""\nMultiline\n"""'
    tokens = [
        (Token.Literal.String.Doc, '"""\nMultiline\n"""'),
        (Token.Text, "\n"),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
