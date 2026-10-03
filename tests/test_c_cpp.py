"""
    Tests for the C and C++ lexers.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import CLexer
from pygments.lexers import CppLexer
from pygments.token import Number
from pygments.token import Operator
from pygments.token import Whitespace


@pytest.mark.parametrize('lexer_cls', [CLexer, CppLexer])
@pytest.mark.parametrize('number, token_type', [
    ('2', Number.Integer),
    ('0x2', Number.Hex),
    ('0b10', Number.Bin),
    ('02', Number.Oct),
    ('2.5', Number.Float),
    ('.5', Number.Float),
    ('2.', Number.Float),
    ('2e-3', Number.Float),
    ('2e+3', Number.Float),
    ('0x1p-2', Number.Float),
    ('0x1p+2', Number.Float),
    ("2'000u", Number.Integer),
])
@pytest.mark.parametrize('left', ['', '1', '1 '])
def test_minus_before_number(lexer_cls, number, token_type, left):
    # A leading sign is an operator; a sign inside an exponent stays in the number.
    tokens = [(token, value) for token, value in lexer_cls().get_tokens(f'{left}-{number}')
              if token is not Whitespace]
    expected = [(Operator, '-'), (token_type, number)]
    if left:
        expected.insert(0, (Number.Integer, '1'))
    assert tokens == expected
