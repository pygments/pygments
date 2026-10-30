"""
    Tests for C# character literals.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.dotnet import CSharpLexer
from pygments.token import String


@pytest.mark.parametrize('level', ['none', 'basic', 'full'])
@pytest.mark.parametrize('literal', [
    r"'\x4'", r"'\x41'", r"'\x041'", r"'\x0041'",
    r"'\u0041'", r"'\U00000041'", r"'\n'", r"'\''", "'a'", "'é'",
])
def test_csharp_character_escapes(level, literal):
    assert list(CSharpLexer(unicodelevel=level).get_tokens_unprocessed(literal)) == [
        (0, String.Char, literal),
    ]
