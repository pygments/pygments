"""
    Tests for the Visual Basic.NET lexer.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.dotnet import VbNetLexer
from pygments.token import Name, Number, Operator, Whitespace


@pytest.mark.parametrize('literal', [
    '&HFFFF', '&h8000S', '&H8000US', '&H_C305_F860',
    '&O77', '&o7_7UI', '&O_77UL',
    '&B01111100', '&b_0010_1010', '&B1100UL',
])
def test_prefixed_integer_literals(literal):
    assert list(VbNetLexer().get_tokens_unprocessed(literal)) == [
        (0, Number.Integer, literal),
    ]


def test_ampersand_is_still_concatenation():
    assert list(VbNetLexer().get_tokens_unprocessed('a & b')) == [
        (0, Name, 'a'), (1, Whitespace, ' '), (2, Operator, '&'),
        (3, Whitespace, ' '), (4, Name, 'b'),
    ]
