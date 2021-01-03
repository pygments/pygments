# -*- coding: utf-8 -*-
"""
    BibTeX Test
    ~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import textwrap

import pytest

from pygments.lexers import BibTeXLexer, BSTLexer
from pygments.token import Token


@pytest.fixture(scope='module')
def lexer():
    yield BibTeXLexer()


def test_preamble(lexer):
    data = '@PREAMBLE{"% some LaTeX code here"}'
    tokens = [
        (Token.Name.Class, '@PREAMBLE'),
        (Token.Punctuation, '{'),
        (Token.String, '"'),
        (Token.String, '% some LaTeX code here'),
        (Token.String, '"'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(data)) == tokens


def test_string(lexer):
    data = '@STRING(SCI = "Science")'
    tokens = [
        (Token.Name.Class, '@STRING'),
        (Token.Punctuation, '('),
        (Token.Name.Attribute, 'SCI'),
        (Token.Text, ' '),
        (Token.Punctuation, '='),
        (Token.Text, ' '),
        (Token.String, '"'),
        (Token.String, 'Science'),
        (Token.String, '"'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(data)) == tokens


def test_entry(lexer):
    data = """
        This is a comment.

        @ARTICLE{ruckenstein-diffusion,
            author = "Liu, Hongquin" # and # "Ruckenstein, Eli",
            year = 1997,
            month = JAN,
            pages = "888-895"
        }
    """

    tokens = [
        (Token.Comment, 'This is a comment.'),
        (Token.Text, '\n\n'),
        (Token.Name.Class, '@ARTICLE'),
        (Token.Punctuation, '{'),
        (Token.Name.Label, 'ruckenstein-diffusion'),
        (Token.Punctuation, ','),
        (Token.Text, '\n    '),
        (Token.Name.Attribute, 'author'),
        (Token.Text, ' '),
        (Token.Punctuation, '='),
        (Token.Text, ' '),
        (Token.String, '"'),
        (Token.String, 'Liu, Hongquin'),
        (Token.String, '"'),
        (Token.Text, ' '),
        (Token.Punctuation, '#'),
        (Token.Text, ' '),
        (Token.Name.Variable, 'and'),
        (Token.Text, ' '),
        (Token.Punctuation, '#'),
        (Token.Text, ' '),
        (Token.String, '"'),
        (Token.String, 'Ruckenstein, Eli'),
        (Token.String, '"'),
        (Token.Punctuation, ','),
        (Token.Text, '\n    '),
        (Token.Name.Attribute, 'year'),
        (Token.Text, ' '),
        (Token.Punctuation, '='),
        (Token.Text, ' '),
        (Token.Number, '1997'),
        (Token.Punctuation, ','),
        (Token.Text, '\n    '),
        (Token.Name.Attribute, 'month'),
        (Token.Text, ' '),
        (Token.Punctuation, '='),
        (Token.Text, ' '),
        (Token.Name.Variable, 'JAN'),
        (Token.Punctuation, ','),
        (Token.Text, '\n    '),
        (Token.Name.Attribute, 'pages'),
        (Token.Text, ' '),
        (Token.Punctuation, '='),
        (Token.Text, ' '),
        (Token.String, '"'),
        (Token.String, '888-895'),
        (Token.String, '"'),
        (Token.Text, '\n'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(textwrap.dedent(data))) == tokens


def test_comment(lexer):
    data = '@COMMENT{test}'
    tokens = [
        (Token.Comment, '@COMMENT'),
        (Token.Comment, '{test}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(data)) == tokens


def test_missing_body(lexer):
    data = '@ARTICLE xxx'
    tokens = [
        (Token.Name.Class, '@ARTICLE'),
        (Token.Text, ' '),
        (Token.Error, 'x'),
        (Token.Error, 'x'),
        (Token.Error, 'x'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(data)) == tokens


def test_mismatched_brace(lexer):
    data = '@PREAMBLE(""}'
    tokens = [
        (Token.Name.Class, '@PREAMBLE'),
        (Token.Punctuation, '('),
        (Token.String, '"'),
        (Token.String, '"'),
        (Token.Error, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(data)) == tokens


def test_basic_bst():
    lexer = BSTLexer()
    data = """
        % BibTeX standard bibliography style `plain'

        INTEGERS { output.state before.all }

        FUNCTION {sort.format.title}
        { 't :=
        "A " #2
            "An " #3
            "The " #4 t chop.word
            chop.word
        chop.word
        sortify
        #1 global.max$ substring$
        }

        ITERATE {call.type$}
    """
    tokens = [
        (Token.Comment.SingleLine, "% BibTeX standard bibliography style `plain'"),
        (Token.Text, '\n\n'),
        (Token.Keyword, 'INTEGERS'),
        (Token.Text, ' '),
        (Token.Punctuation, '{'),
        (Token.Text, ' '),
        (Token.Name.Variable, 'output.state'),
        (Token.Text, ' '),
        (Token.Name.Variable, 'before.all'),
        (Token.Text, ' '),
        (Token.Punctuation, '}'),
        (Token.Text, '\n\n'),
        (Token.Keyword, 'FUNCTION'),
        (Token.Text, ' '),
        (Token.Punctuation, '{'),
        (Token.Name.Variable, 'sort.format.title'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
        (Token.Punctuation, '{'),
        (Token.Text, ' '),
        (Token.Name.Function, "'t"),
        (Token.Text, ' '),
        (Token.Name.Variable, ':='),
        (Token.Text, '\n'),
        (Token.Literal.String, '"A "'),
        (Token.Text, ' '),
        (Token.Literal.Number, '#2'),
        (Token.Text, '\n    '),
        (Token.Literal.String, '"An "'),
        (Token.Text, ' '),
        (Token.Literal.Number, '#3'),
        (Token.Text, '\n    '),
        (Token.Literal.String, '"The "'),
        (Token.Text, ' '),
        (Token.Literal.Number, '#4'),
        (Token.Text, ' '),
        (Token.Name.Variable, 't'),
        (Token.Text, ' '),
        (Token.Name.Variable, 'chop.word'),
        (Token.Text, '\n    '),
        (Token.Name.Variable, 'chop.word'),
        (Token.Text, '\n'),
        (Token.Name.Variable, 'chop.word'),
        (Token.Text, '\n'),
        (Token.Name.Variable, 'sortify'),
        (Token.Text, '\n'),
        (Token.Literal.Number, '#1'),
        (Token.Text, ' '),
        (Token.Name.Builtin, 'global.max$'),
        (Token.Text, ' '),
        (Token.Name.Builtin, 'substring$'),
        (Token.Text, '\n'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n\n'),
        (Token.Keyword, 'ITERATE'),
        (Token.Text, ' '),
        (Token.Punctuation, '{'),
        (Token.Name.Builtin, 'call.type$'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(textwrap.dedent(data))) == tokens
