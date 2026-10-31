"""
    Jsonnet lexer tests
    ~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.jsonnet import JsonnetLexer
from pygments.token import Number, Operator, Punctuation


@pytest.mark.parametrize('literal', [
    '0', '123', '12.345', '0.001', '1e10', '1E-3', '2.5e+12',
    '1_000_000', '0.000_001', '6.022_140_76e23', '1e1_000',
])
def test_number_literals(literal):
    assert list(JsonnetLexer().get_tokens_unprocessed(literal)) == [
        (0, Number.Float, literal),
    ]


def test_adjacent_array_elements():
    assert list(JsonnetLexer().get_tokens_unprocessed('[1,2]')) == [
        (0, Punctuation, '['),
        (1, Number.Float, '1'),
        (2, Punctuation, ','),
        (3, Number.Float, '2'),
        (4, Punctuation, ']'),
    ]


@pytest.mark.parametrize('operator', ['+', '-', '*', '/', '%'])
def test_adjacent_operators(operator):
    assert list(JsonnetLexer().get_tokens_unprocessed(f'1{operator}2')) == [
        (0, Number.Float, '1'),
        (1, Operator, operator),
        (2, Number.Float, '2'),
    ]


@pytest.mark.parametrize('sign', ['+', '-'])
def test_unary_sign(sign):
    assert list(JsonnetLexer().get_tokens_unprocessed(f'{sign}2.5')) == [
        (0, Operator, sign),
        (1, Number.Float, '2.5'),
    ]
