"""
    Hare lexer tests
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.hare import HareLexer
from pygments.token import Name, Number, Operator


@pytest.mark.parametrize('suffix', [
    '', 'i', 'u', 'z', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64',
])
def test_binary_integer_suffixes(suffix):
    literal = '0b1010' + suffix
    assert list(HareLexer().get_tokens_unprocessed(literal)) == [
        (0, Number.Bin, literal),
    ]


@pytest.mark.parametrize('literal', ['0b0', '0b1', '0b1_010', '0b10_10_01u16'])
def test_binary_integer_digits(literal):
    assert list(HareLexer().get_tokens_unprocessed(literal)) == [
        (0, Number.Bin, literal),
    ]


def test_binary_integer_adjacent_operators():
    assert list(HareLexer().get_tokens_unprocessed('-0b101u8+0b10')) == [
        (0, Operator, '-'),
        (1, Number.Bin, '0b101u8'),
        (8, Operator, '+'),
        (9, Number.Bin, '0b10'),
    ]


@pytest.mark.parametrize('name', ['binary', 'b1010u8', 'value0b101'])
def test_binary_looking_identifiers(name):
    assert list(HareLexer().get_tokens_unprocessed(name)) == [(0, Name, name)]
