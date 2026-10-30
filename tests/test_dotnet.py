"""
    Tests for .NET language lexers.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.dotnet import CSharpLexer
from pygments.token import Name, Number


@pytest.mark.parametrize('level', ['none', 'basic', 'full'])
@pytest.mark.parametrize('literal, token', [
    ('1_000_000', Number.Integer),
    ('1__000UL', Number.Integer),
    ('0xFF_FF', Number.Hex),
    ('0X_FF__00uL', Number.Hex),
    ('0b_0010_1010', Number.Bin),
    ('0B11__00LU', Number.Bin),
    ('1_000.25_00', Number.Float),
    ('.12_34f', Number.Float),
    ('1_2e+1_0', Number.Float),
    ('1_2.3_4E-5_6M', Number.Float),
    ('1_000m', Number.Float),
])
def test_csharp_digit_separators(level, literal, token):
    assert list(CSharpLexer(unicodelevel=level).get_tokens_unprocessed(literal)) == [
        (0, token, literal),
    ]


def test_csharp_trailing_underscore_is_not_part_of_number():
    assert list(CSharpLexer().get_tokens_unprocessed('1_')) == [
        (0, Number.Integer, '1'),
        (1, Name, '_'),
    ]
