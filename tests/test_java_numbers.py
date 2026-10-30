"""
    Tests for Java floating-point literals.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.jvm import AspectJLexer, JavaLexer
from pygments.token import Number


@pytest.mark.parametrize('lexer', [JavaLexer, AspectJLexer])
@pytest.mark.parametrize('literal', [
    '12e3', '123E-4', '12e+3F', '1_000e2', '12f', '123D', '1_000F',
    '12.5', '.5f', '0x1.fp2',
])
def test_java_float_literals(lexer, literal):
    assert list(lexer().get_tokens_unprocessed(literal)) == [(0, Number.Float, literal)]
