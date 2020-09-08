# -*- coding: utf-8 -*-
"""
    Basic EzhilLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2015 Muthiah Annamalai <ezhillang@gmail.com>
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Operator, Number, Text, Token
from pygments.lexers import EzhilLexer


@pytest.fixture(scope='module')
def lexer():
    yield EzhilLexer()


def test_sum(lexer):
    fragment = '1+3\n'
    tokens = [
        (Number.Integer, '1'),
        (Operator, '+'),
        (Number.Integer, '3'),
        (Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_gcd_expr(lexer):
    fragment = '1^3+(5-5)*gcd(a,b)\n'
    tokens = [
        (Token.Number.Integer, '1'),
        (Token.Operator, '^'),
        (Token.Literal.Number.Integer, '3'),
        (Token.Operator, '+'),
        (Token.Punctuation, '('),
        (Token.Literal.Number.Integer, '5'),
        (Token.Operator, '-'),
        (Token.Literal.Number.Integer, '5'),
        (Token.Punctuation, ')'),
        (Token.Operator, '*'),
        (Token.Name, 'gcd'),
        (Token.Punctuation, '('),
        (Token.Name, 'a'),
        (Token.Operator, ','),
        (Token.Name, 'b'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_if_statement(lexer):
    fragment = """@( 0 > 3 ) ஆனால்
    பதிப்பி "wont print"
முடி"""
    tokens = [
        (Token.Operator, '@'),
        (Token.Punctuation, '('),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '0'),
        (Token.Text, ' '),
        (Token.Operator, '>'),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '3'),
        (Token.Text, ' '),
        (Token.Punctuation, ')'),
        (Token.Text, ' '),
        (Token.Keyword, 'ஆனால்'),
        (Token.Text, '\n'),
        (Token.Text, '    '),
        (Token.Keyword, 'பதிப்பி'),
        (Token.Text, ' '),
        (Token.Literal.String, '"wont print"'),
        (Token.Text, '\n'),
        (Token.Keyword, 'முடி'),
        (Token.Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function(lexer):
    fragment = """# (C) முத்தையா அண்ணாமலை 2013, 2015
நிரல்பாகம்  gcd ( x, y )
மு = max(x,y)
 q = min(x,y)

@( q == 0 ) ஆனால்
       பின்கொடு  மு
முடி
பின்கொடு  gcd( மு - q , q )
முடி\n"""
    tokens = [
        (Token.Comment.Single,
         '# (C) \u0bae\u0bc1\u0ba4\u0bcd\u0ba4\u0bc8\u0baf\u0bbe \u0b85'
         '\u0ba3\u0bcd\u0ba3\u0bbe\u0bae\u0bb2\u0bc8 2013, 2015\n'),
        (Token.Keyword, 'நிரல்பாகம்'),
        (Token.Text, '  '),
        (Token.Name, 'gcd'),
        (Token.Text, ' '),
        (Token.Punctuation, '('),
        (Token.Text, ' '),
        (Token.Name, 'x'),
        (Token.Operator, ','),
        (Token.Text, ' '),
        (Token.Name, 'y'),
        (Token.Text, ' '),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Name, '\u0bae\u0bc1'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Name.Builtin, 'max'),
        (Token.Punctuation, '('),
        (Token.Name, 'x'),
        (Token.Operator, ','),
        (Token.Name, 'y'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Text, ' '),
        (Token.Name, 'q'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Name.Builtin, 'min'),
        (Token.Punctuation, '('),
        (Token.Name, 'x'),
        (Token.Operator, ','),
        (Token.Name, 'y'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Text, '\n'),
        (Token.Operator, '@'),
        (Token.Punctuation, '('),
        (Token.Text, ' '),
        (Token.Name, 'q'),
        (Token.Text, ' '),
        (Token.Operator, '=='),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '0'),
        (Token.Text, ' '),
        (Token.Punctuation, ')'),
        (Token.Text, ' '),
        (Token.Keyword, 'ஆனால்'),
        (Token.Text, '\n'),
        (Token.Text, '       '),
        (Token.Keyword, 'பின்கொடு'),
        (Token.Text, '  '),
        (Token.Name, '\u0bae\u0bc1'),
        (Token.Text, '\n'),
        (Token.Keyword, 'முடி'),
        (Token.Text, '\n'),
        (Token.Keyword, '\u0baa\u0bbf\u0ba9\u0bcd\u0b95\u0bca\u0b9f\u0bc1'),
        (Token.Text, '  '),
        (Token.Name, 'gcd'),
        (Token.Punctuation, '('),
        (Token.Text, ' '),
        (Token.Name, '\u0bae\u0bc1'),
        (Token.Text, ' '),
        (Token.Operator, '-'),
        (Token.Text, ' '),
        (Token.Name, 'q'),
        (Token.Text, ' '),
        (Token.Operator, ','),
        (Token.Text, ' '),
        (Token.Name, 'q'),
        (Token.Text, ' '),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Keyword, 'முடி'),  # '\u0bae\u0bc1\u0b9f\u0bbf'),
        (Token.Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
