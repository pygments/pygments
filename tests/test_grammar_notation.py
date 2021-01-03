# -*- coding: utf-8 -*-
"""
    Basic Grammar Notation Tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import PegLexer


@pytest.fixture(scope='module')
def lexer_peg():
    yield PegLexer()


def test_peg_basic(lexer_peg):
    fragment = 'rule<-("terminal"/nonterminal/[cls])*\n'
    tokens = [
        (Token.Name.Class, 'rule'),
        (Token.Operator, '<-'),
        (Token.Punctuation, '('),
        (Token.String.Double, '"terminal"'),
        (Token.Operator, '/'),
        (Token.Name.Class, 'nonterminal'),
        (Token.Operator, '/'),
        (Token.Punctuation, '['),
        (Token.String, 'cls'),
        (Token.Punctuation, ']'),
        (Token.Punctuation, ')'),
        (Token.Operator, '*'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_peg.get_tokens(fragment)) == tokens


def test_peg_operators(lexer_peg):
    # see for example:
    # - https://github.com/gvanrossum/pegen
    # - https://nim-lang.org/docs/pegs.html
    fragment = "rule = 'a' | 'b'\n"
    tokens = [
        (Token.Name.Class, 'rule'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.String.Single, "'a'"),
        (Token.Text, ' '),
        (Token.Operator, '|'),
        (Token.Text, ' '),
        (Token.String.Single, "'b'"),
        (Token.Text, '\n'),
    ]
    assert list(lexer_peg.get_tokens(fragment)) == tokens
    fragment = "rule: 'a' ~ 'b'\n"
    tokens = [
        (Token.Name.Class, 'rule'),
        (Token.Operator, ':'),
        (Token.Text, ' '),
        (Token.String.Single, "'a'"),
        (Token.Text, ' '),
        (Token.Operator, '~'),
        (Token.Text, ' '),
        (Token.String.Single, "'b'"),
        (Token.Text, '\n'),
    ]
    assert list(lexer_peg.get_tokens(fragment)) == tokens


def test_peg_modified_strings(lexer_peg):
    # see for example:
    # - http://textx.github.io/Arpeggio/
    # - https://nim-lang.org/docs/pegs.html
    # - https://github.com/erikrose/parsimonious
    fragment = '~"regex" i"insensitive" "multimod"ilx ("not modified")\n'
    tokens = [
        # can't handle parsimonious-style regex while ~ is a cut operator
        (Token.Operator, '~'),
        (Token.String.Double, '"regex"'),
        (Token.Text, ' '),
        (Token.String.Double, 'i"insensitive"'),
        (Token.Text, ' '),
        (Token.String.Double, '"multimod"ilx'),
        (Token.Text, ' '),
        (Token.Punctuation, '('),
        (Token.String.Double, '"not modified"'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_peg.get_tokens(fragment)) == tokens
