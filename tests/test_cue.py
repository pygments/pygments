"""
    Basic CueLexer Test
    ~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest
from pygments.token import (
    Token, Keyword, Name, Number, String, Comment,
    Operator, Punctuation, Whitespace, Error
)
from pygments.lexers.cue import CueLexer


@pytest.fixture(scope='module')
def lexer():
    yield CueLexer()


def test_package_declaration(lexer):
    """Test package declaration highlighting."""
    code = 'package example'
    tokens = list(lexer.get_tokens(code))
    assert (Keyword.Namespace, 'package') in tokens
    assert (Name.Namespace, 'example') in tokens


def test_import_statements(lexer):
    """Test various import statement forms."""
    # Simple import
    code = 'import "strings"'
    tokens = list(lexer.get_tokens(code))
    assert (Keyword.Namespace, 'import') in tokens
    assert (String, 'strings') in tokens

    # Import with alias
    code = 'import foo "strings"'
    tokens = list(lexer.get_tokens(code))
    assert (Keyword.Namespace, 'import') in tokens
    assert (Name.Namespace, 'foo') in tokens

    # Multi-line import block
    code = '''import (
    "strings"
    "encoding/json"
)'''
    tokens = list(lexer.get_tokens(code))
    assert (Keyword.Namespace, 'import') in tokens


def test_basic_types(lexer):
    """Test basic type highlighting."""
    code = 'name: string'
    tokens = list(lexer.get_tokens(code))
    assert (Keyword.Type, 'string') in tokens

    code = 'age: int'
    tokens = list(lexer.get_tokens(code))
    assert (Keyword.Type, 'int') in tokens


def test_numbers(lexer):
    """Test number format highlighting."""
    test_cases = [
        ('42', Number.Integer),
        ('3.14', Number.Float),
        ('1e10', Number.Float),
        ('0xFF', Number.Hex),
        ('0b1010', Number.Bin),
        ('0o755', Number.Oct),
        ('100Gi', Number.Integer),  # with units
    ]

    for code, expected_type in test_cases:
        tokens = list(lexer.get_tokens(code))
        assert any(token_type == expected_type for token_type, _ in tokens), \
               f"Expected {expected_type} for '{code}'"


def test_strings(lexer):
    """Test string highlighting."""
    test_cases = [
        '"hello world"',
        "'single quoted'",
        '`raw string`',
        '#"sharp string"#',
        '"""\nmultiline string\n"""',
    ]

    for code in test_cases:
        tokens = list(lexer.get_tokens(code))
        # Check that we have string tokens
        string_tokens = [t for t in tokens if t[0] in (String, String.Double, String.Single, String.Backtick)]
        assert len(string_tokens) > 0, f"No string tokens found for {code}"


def test_comments(lexer):
    """Test comment highlighting."""
    # Single line comment
    code = '// This is a comment'
    tokens = list(lexer.get_tokens(code))
    assert (Comment.Single, '// This is a comment') in tokens


def test_keywords(lexer):
    """Test keyword highlighting."""
    keywords = ['true', 'false', 'null', 'for', 'in', 'if', 'let']

    for keyword in keywords:
        tokens = list(lexer.get_tokens(keyword))
        assert any(token_type in (Keyword, Keyword.Constant) for token_type, _ in tokens), \
               f"Keyword '{keyword}' not highlighted correctly"


def test_operators(lexer):
    """Test operator highlighting."""
    operators = ['==', '!=', '<=', '>=', '<', '>', '&&', '||', '&', '|']

    for op in operators:
        code = f'a {op} b'
        tokens = list(lexer.get_tokens(code))
        assert (Operator, op) in tokens, f"Operator '{op}' not highlighted"


def test_definitions_and_fields(lexer):
    """Test definition and field highlighting."""
    # Regular field
    code = 'field: value'
    tokens = list(lexer.get_tokens(code))
    assert (Name.Property, 'field') in tokens

    # Definition (starts with #)
    code = '#Definition: {}'
    tokens = list(lexer.get_tokens(code))
    assert (Name.Entity, '#Definition') in tokens

    # Hidden field (starts with _)
    code = '_hidden: "secret"'
    tokens = list(lexer.get_tokens(code))
    assert (Name.Variable.Magic, '_hidden') in tokens


def test_interpolation(lexer):
    """Test string interpolation."""
    code = '"Hello \\(name)!"'
    tokens = list(lexer.get_tokens(code))
    # Should have punctuation for \( and )
    punctuation_tokens = [t for t in tokens if t[0] == Punctuation]
    assert len(punctuation_tokens) > 0


def test_complex_structure(lexer):
    """Test a more complex CUE structure."""
    code = '''
package config

#Service: {
    name: string
    port: int & >0 & <=65535
    replicas: *1 | int & >=1
}

service: #Service & {
    name: "web-server"
    port: 8080
    if env == "prod" {
        replicas: 3
    }
}
'''
    tokens = list(lexer.get_tokens(code))

    # Should not have any error tokens in well-formed code
    error_tokens = [t for t in tokens if t[0] == Error]
    assert len(error_tokens) == 0, f"Found error tokens: {error_tokens}"

    # Should have various expected token types
    token_types = [t[0] for t in tokens]
    assert Keyword.Namespace in token_types  # package
    assert Name.Entity in token_types        # #Service
    assert Keyword.Type in token_types       # string, int
    assert Name.Property in token_types      # name, port, etc.
    assert Number.Integer in token_types     # numbers
