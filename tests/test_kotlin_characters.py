"""
    Tests for Kotlin character literals.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.jvm import KotlinLexer
from pygments.token import String


@pytest.mark.parametrize('literal', [
    r"'\u0041'", r"'\u03A9'", r"'\u0000'", r"'\u00ff'",
    r"'\n'", r"'\''", r"'\\'", "'a'", "'Ω'",
])
def test_kotlin_character_literals(literal):
    assert list(KotlinLexer().get_tokens_unprocessed(literal)) == [(0, String.Char, literal)]
