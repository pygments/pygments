# -*- coding: utf-8 -*-
"""
    MATLAB Tests
    ~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import MatlabLexer


@pytest.fixture(scope='module')
def lexer():
    yield MatlabLexer()


def test_single_line(lexer):
    """
    Test that a single line with strings, a method, and numbers is parsed correctly.
    """
    fragment = "set('T',300,'P',101325);\n"
    tokens = [
        (Token.Name, 'set'),
        (Token.Punctuation, '('),
        (Token.Literal.String, "'"),
        (Token.Literal.String, "T'"),
        (Token.Punctuation, ','),
        (Token.Literal.Number.Integer, '300'),
        (Token.Punctuation, ','),
        (Token.Literal.String, "'"),
        (Token.Literal.String, "P'"),
        (Token.Punctuation, ','),
        (Token.Literal.Number.Integer, '101325'),
        (Token.Punctuation, ')'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_line_continuation(lexer):
    """
    Test that line continuation by ellipses does not produce generic
    output on the second line.
    """
    fragment = "set('T',300,...\n'P',101325);\n"
    tokens = [
        (Token.Name, 'set'),
        (Token.Punctuation, '('),
        (Token.Literal.String, "'"),
        (Token.Literal.String, "T'"),
        (Token.Punctuation, ','),
        (Token.Literal.Number.Integer, '300'),
        (Token.Punctuation, ','),
        (Token.Keyword, '...'),
        (Token.Text, '\n'),
        (Token.Literal.String, "'"),
        (Token.Literal.String, "P'"),
        (Token.Punctuation, ','),
        (Token.Literal.Number.Integer, '101325'),
        (Token.Punctuation, ')'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_keywords_ended_by_newline(lexer):
    """Test that keywords on their own line are marked as keywords."""
    fragment = "if x > 100\n    disp('x > 100')\nelse\n    disp('x < 100')\nend\n"
    tokens = [
        (Token.Keyword, 'if'),
        (Token.Text, ' '),
        (Token.Name, 'x'),
        (Token.Text, ' '),
        (Token.Operator, '>'),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '100'),
        (Token.Text, '\n'),
        (Token.Text, ' '),
        (Token.Text, ' '),
        (Token.Text, ' '),
        (Token.Text, ' '),
        (Token.Name.Builtin, 'disp'),
        (Token.Punctuation, '('),
        (Token.Literal.String, "'"),
        (Token.Literal.String, "x > 100'"),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Keyword, 'else'),
        (Token.Text, '\n'),
        (Token.Text, ' '),
        (Token.Text, ' '),
        (Token.Text, ' '),
        (Token.Text, ' '),
        (Token.Name.Builtin, 'disp'),
        (Token.Punctuation, '('),
        (Token.Literal.String, "'"),
        (Token.Literal.String, "x < 100'"),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Keyword, 'end'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_comment_after_continuation(lexer):
    """
    Test that text after the line continuation ellipses is marked as a comment.
    """
    fragment = "set('T',300,... a comment\n'P',101325);\n"
    tokens = [
        (Token.Name, 'set'),
        (Token.Punctuation, '('),
        (Token.Literal.String, "'"),
        (Token.Literal.String, "T'"),
        (Token.Punctuation, ','),
        (Token.Literal.Number.Integer, '300'),
        (Token.Punctuation, ','),
        (Token.Keyword, '...'),
        (Token.Comment, ' a comment'),
        (Token.Text, '\n'),
        (Token.Literal.String, "'"),
        (Token.Literal.String, "P'"),
        (Token.Punctuation, ','),
        (Token.Literal.Number.Integer, '101325'),
        (Token.Punctuation, ')'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_multiple_spaces_variable_assignment(lexer):
    """
    Test that multiple spaces with an equal sign doesn't get formatted to a string.
    """
    fragment = 'x  = 100;\n'
    tokens = [
        (Token.Name, 'x'),
        (Token.Text, ' '),
        (Token.Text, ' '),
        (Token.Punctuation, '='),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '100'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_operator_multiple_space(lexer):
    """
    Test that multiple spaces with an operator doesn't get formatted to a string.
    """
    fragment = 'x  > 100;\n'
    tokens = [
        (Token.Name, 'x'),
        (Token.Text, ' '),
        (Token.Text, ' '),
        (Token.Operator, '>'),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '100'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_one_space_assignment(lexer):
    """Test that one space before an equal sign is formatted correctly."""
    fragment = 'x = 100;\n'
    tokens = [
        (Token.Name, 'x'),
        (Token.Text, ' '),
        (Token.Punctuation, '='),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '100'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_command_mode(lexer):
    """
    MATLAB allows char function arguments to not be enclosed by parentheses
    or contain quote characters, as long as they are space separated. Test
    that one common such function is formatted appropriately.
    """
    fragment = 'help sin\n'
    tokens = [
        (Token.Name, 'help'),
        (Token.Text, ' '),
        (Token.Literal.String, 'sin'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
