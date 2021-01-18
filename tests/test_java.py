"""
    Basic JavaLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import time

import pytest

from pygments.token import String
from pygments.lexers import JavaLexer


@pytest.fixture(scope='module')
def lexer():
    yield JavaLexer()


@pytest.mark.parametrize(
    'text',
    (
        '""', '"abc"', '"ひらがな"', '"123"',
        '"\\\\"', '"\\t"' '"\\""',
    ),
)
def test_string_literals_positive_match(lexer, text):
    """Test positive matches for string literals."""
    tokens = list(lexer.get_tokens_unprocessed(text))
    assert all([token is String for _, token, _ in tokens])
    assert ''.join([value for _, _, value in tokens]) == text


def test_string_literals_backtracking(lexer):
    """Test catastrophic backtracking for string literals."""
    start_time = time.time()
    list(lexer.get_tokens_unprocessed('"' + '\\' * 100))
    assert time.time() - start_time < 1, 'possible backtracking bug'
