"""
    Tests for hexadecimal floating-point literals in C and C++.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.c_cpp import CLexer, CppLexer
from pygments.token import Number, Whitespace


@pytest.mark.parametrize('lexer_class', [CLexer, CppLexer])
@pytest.mark.parametrize('literal', [
    '0x1.p3', '0XAB.P-2', '0x1.p+3f', '0x1.p0F', '0x1.p3l', '0x1.p3L',
    '0x1.fp3', '0x.8p1', '0x1p3',
])
def test_hex_float_optional_fraction(lexer_class, literal):
    tokens = [(token, value) for token, value in lexer_class().get_tokens(literal)
              if token not in Whitespace]
    assert tokens == [(Number.Float, literal)]


def test_cpp_hex_float_digit_separators():
    literal = "0xA'B.p1'0"
    tokens = [(token, value) for token, value in CppLexer().get_tokens(literal)
              if token not in Whitespace]
    assert tokens == [(Number.Float, literal)]
