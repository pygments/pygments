# -*- coding: utf-8 -*-
"""
    Basic Grammar Notation Tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import PegLexer


@pytest.fixture(scope='module')
def lexer_peg():
    yield PegLexer()


def test_peg_basic(lexer_peg):
    fragment = u'rule<-("terminal"/nonterminal/[cls])*\n'
    tokens = [
        (Token.Name.Class, u'rule'),
        (Token.Operator, u'<-'),
        (Token.Punctuation, u'('),
        (Token.String.Double, u'"terminal"'),
        (Token.Operator, u'/'),
        (Token.Name.Class, u'nonterminal'),
        (Token.Operator, u'/'),
        (Token.Punctuation, u'['),
        (Token.String, u'cls'),
        (Token.Punctuation, u']'),
        (Token.Punctuation, u')'),
        (Token.Operator, u'*'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer_peg.get_tokens(fragment)) == tokens


def test_peg_operators(lexer_peg):
    # see for example:
    # - https://github.com/gvanrossum/pegen
    # - https://nim-lang.org/docs/pegs.html
    fragment = u"rule = 'a' | 'b'\n"
    tokens = [
        (Token.Name.Class, u'rule'),
        (Token.Text, u' '),
        (Token.Operator, u'='),
        (Token.Text, u' '),
        (Token.String.Single, u"'a'"),
        (Token.Text, u' '),
        (Token.Operator, u'|'),
        (Token.Text, u' '),
        (Token.String.Single, u"'b'"),
        (Token.Text, u'\n'),
    ]
    assert list(lexer_peg.get_tokens(fragment)) == tokens
    fragment = u"rule: 'a' ~ 'b'\n"
    tokens = [
        (Token.Name.Class, u'rule'),
        (Token.Operator, u':'),
        (Token.Text, u' '),
        (Token.String.Single, u"'a'"),
        (Token.Text, u' '),
        (Token.Operator, u'~'),
        (Token.Text, u' '),
        (Token.String.Single, u"'b'"),
        (Token.Text, u'\n'),
    ]
    assert list(lexer_peg.get_tokens(fragment)) == tokens


def test_peg_modified_strings(lexer_peg):
    # see for example:
    # - http://textx.github.io/Arpeggio/
    # - https://nim-lang.org/docs/pegs.html
    # - https://github.com/erikrose/parsimonious
    fragment = u'~"regex" i"insensitive" "multimod"ilx ("not modified")\n'
    tokens = [
        # can't handle parsimonious-style regex while ~ is a cut operator
        (Token.Operator, u'~'),
        (Token.String.Double, u'"regex"'),
        (Token.Text, u' '),
        (Token.String.Double, u'i"insensitive"'),
        (Token.Text, u' '),
        (Token.String.Double, u'"multimod"ilx'),
        (Token.Text, u' '),
        (Token.Punctuation, u'('),
        (Token.String.Double, u'"not modified"'),
        (Token.Punctuation, u')'),
        (Token.Text, u'\n'),
    ]
    assert list(lexer_peg.get_tokens(fragment)) == tokens
