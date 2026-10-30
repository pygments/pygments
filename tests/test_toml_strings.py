"""
    Tests for TOML multiline string delimiters.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.configs import TOMLLexer
from pygments.token import Error, Name, Number


@pytest.mark.parametrize('quote', ['"', "'"])
@pytest.mark.parametrize('closing_quotes', [3, 4, 5])
@pytest.mark.parametrize('in_array', [False, True])
def test_toml_multiline_string_terminators(quote, closing_quotes, in_array):
    value = quote * 3 + 'content' + quote * closing_quotes
    if in_array:
        text = 'value = [' + value + ', 42]\nafter = 7\n'
    else:
        text = 'value = ' + value + '\nafter = 42\n'
    tokens = list(TOMLLexer().get_tokens_unprocessed(text))

    assert all(token is not Error for _, token, _ in tokens)
    assert (Name, 'after') in [(token, value) for _, token, value in tokens]
    assert (Number.Integer, '42') in [(token, value) for _, token, value in tokens]
