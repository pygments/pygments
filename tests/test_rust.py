"""
    Tests for Rust string literals.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.rust import RustLexer
from pygments.token import Name, String


@pytest.mark.parametrize('literal', [
    'c"hello"', r'c"\xC3\xA6"', r'c"\u{00E6}"', 'c"æ"',
    'cr"hello"', 'cr#""hello""#', 'cr##""hello"#"##',
])
def test_c_string_literals(literal):
    tokens = list(RustLexer().get_tokens_unprocessed(literal))
    assert ''.join(value for _, _, value in tokens) == literal
    assert all(token in String for _, token, _ in tokens)


@pytest.mark.parametrize('identifier', ['cstring', 'cr_string'])
def test_c_string_prefixes_in_identifiers(identifier):
    assert list(RustLexer().get_tokens_unprocessed(identifier)) == [(0, Name, identifier)]
