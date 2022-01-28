"""
    Data Tests
    ~~~~~~~~~~

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import time

import pytest

from pygments.lexers.data import JsonLexer, JsonBareObjectLexer, JsonLdLexer
from pygments.token import Comment, Error, Token, Punctuation, Number, String, \
        Keyword, Name, Whitespace


@pytest.fixture(scope='module')
def lexer_json():
    yield JsonLexer()


@pytest.fixture(scope='module')
def lexer_bare():
    yield JsonBareObjectLexer()


@pytest.fixture(scope='module')
def lexer_json_ld():
    yield JsonLdLexer()


@pytest.mark.parametrize(
    'text, expected_token_types',
    (
        # Integers
        ('0', (Number.Integer,)),
        ('-1', (Number.Integer,)),
        ('1234567890', (Number.Integer,)),
        ('-1234567890', (Number.Integer,)),

        # Floats, including scientific notation
        ('123456789.0123456789', (Number.Float,)),
        ('-123456789.0123456789', (Number.Float,)),
        ('1e10', (Number.Float,)),
        ('-1E10', (Number.Float,)),
        ('1e-10', (Number.Float,)),
        ('-1E+10', (Number.Float,)),
        ('1.0e10', (Number.Float,)),
        ('-1.0E10', (Number.Float,)),
        ('1.0e-10', (Number.Float,)),
        ('-1.0E+10', (Number.Float,)),

        # Strings (escapes are tested elsewhere)
        ('""', (String.Double,)),
        ('"abc"', (String.Double,)),
        ('"ひらがな"', (String.Double,)),
        ('"123"', (String.Double,)),
        ('"[]"', (String.Double,)),
        ('"{}"', (String.Double,)),
        ('"true"', (String.Double,)),
        ('"false"', (String.Double,)),
        ('"null"', (String.Double,)),
        ('":,"', (String.Double,)),

        # Constants
        ('true', (Keyword.Constant, )),
        ('false', (Keyword.Constant, )),
        ('null', (Keyword.Constant, )),

        # Arrays
        ('[]', (Punctuation,)),
        ('["a", "b"]', (Punctuation, String.Double, Punctuation,
                        Whitespace, String.Double, Punctuation)),

        # Objects
        ('{}', (Punctuation,)),
        ('{"a": "b"}', (Punctuation, Name.Tag, Punctuation,
                        Whitespace, String.Double, Punctuation)),
    )
)
@pytest.mark.parametrize('end', ('', '\n'))
def test_json_literals_positive_match(lexer_json, text, expected_token_types, end):
    """Validate that syntactically-correct JSON literals are parsed correctly."""

    tokens = list(lexer_json.get_tokens_unprocessed(text + end))
    assert len(tokens) == len(expected_token_types) + bool(end)
    assert all(token[1] is expected_token
               for token, expected_token in zip(tokens, expected_token_types))
    assert ''.join(token[2] for token in tokens) == text + end


@pytest.mark.parametrize(
    'text, expected',
    (
        ('\u0020', Whitespace),  # space
        ('\u000a', Whitespace),  # newline
        ('\u000d', Whitespace),  # carriage return
        ('\u0009', Whitespace),  # tab
    )
)
def test_json_whitespace_positive_match(lexer_json, text, expected):
    """Validate that whitespace is parsed correctly."""

    tokens = list(lexer_json.get_tokens_unprocessed(text))
    assert tokens == [(0, expected, text)]

    # Expand the whitespace and verify parsing again.
    tokens = list(lexer_json.get_tokens_unprocessed(text * 2 + ' '))
    assert tokens == [(0, expected, text * 2 + ' ')]


@pytest.mark.parametrize(
    'text',
    (
        '"', '\\', '/', 'b', 'f', 'n', 'r', 't',
        'u0123', 'u4567', 'u89ab', 'ucdef', 'uABCD', 'uEF01',
    )
)
def test_json_object_key_escapes_positive_match(lexer_json, text):
    """Validate that escape sequences in JSON object keys are parsed correctly."""

    tokens = list(lexer_json.get_tokens_unprocessed('{"\\%s": 1}' % text))
    assert len(tokens) == 6
    assert tokens[1][1] is Name.Tag
    assert tokens[1][2] == '"\\%s"' % text


@pytest.mark.parametrize('text', ('uz', 'u1z', 'u12z', 'u123z'))
def test_json_string_unicode_escapes_negative_match(lexer_json, text):
    """Validate that if unicode escape sequences end abruptly there's no problem."""

    tokens = list(lexer_json.get_tokens_unprocessed('"\\%s"' % text))
    assert len(tokens) == 1
    assert tokens[0] == (0, String.Double, '"\\%s"' % text)


@pytest.mark.parametrize(
    'text',
    (
        '"', '\\', '/', 'b', 'f', 'n', 'r', 't',
        'u0123', 'u4567', 'u89ab', 'ucdef', 'uABCD', 'uEF01',
    )
)
def test_json_string_escapes_positive_match(lexer_json, text):
    """Validate that escape sequences in JSON string values are parsed correctly."""

    text = '"\\%s"' % text
    tokens = list(lexer_json.get_tokens_unprocessed(text))
    assert len(tokens) == 1
    assert tokens[0][1] is String.Double
    assert tokens[0][2] == text


@pytest.mark.parametrize('text', ('+\n', '0\n', '""0\n', 'a\nb\n', '""/-'))
def test_json_round_trip_errors(lexer_json, text):
    """Validate that past round-trip errors never crop up again."""

    tokens = list(lexer_json.get_tokens_unprocessed(text))
    assert ''.join(t[2] for t in tokens) == text


def test_json_comments_single_line_positive_matches(lexer_json):
    """Verify that single-line comments are tokenized correctly."""

    text = '{"a//b"//C1\n:123/////C2\n}\n// // C3'
    tokens = list(lexer_json.get_tokens_unprocessed(text))
    assert tokens[2] == (7, Comment.Single, "//C1")
    assert tokens[6] == (16, Comment.Single, "/////C2")
    assert tokens[10] == (26, Comment.Single, "// // C3")

    comment_count = sum(1 for token in tokens if token[1] == Comment or token[1].parent == Comment)
    assert comment_count == 3

    parsed_text = ''.join(token[2] for token in tokens)
    assert parsed_text == text, 'Input and output texts must match!'


def test_json_comments_multiline_positive_matches(lexer_json):
    """Verify that multiline comments are tokenized correctly."""

    text = '/** / **/{"a /**/ b"/* \n */:123}'
    tokens = list(lexer_json.get_tokens_unprocessed(text))
    assert tokens[0] == (0, Comment.Multiline, "/** / **/")
    assert tokens[3] == (20, Comment.Multiline, "/* \n */")

    comment_count = sum(1 for token in tokens if token[1] == Comment or token[1].parent == Comment)
    assert comment_count == 2

    parsed_text = ''.join(token[2] for token in tokens)
    assert parsed_text == text, 'Input and output texts must match!'


@pytest.mark.parametrize(
    "text, expected",
    (
        # Unfinished comment openers
        ('/', (0, Error, '/')),
        ('1/', (1, Error, '/')),
        ('/1', (0, Error, '/')),
        ('""/', (2, Error, '/')),
        # Unclosed multiline comments
        ('/*', (0, Error, '/*')),
        ('/**', (0, Error, '/**')),
        ('/*/', (0, Error, '/*/')),
        ('1/*', (1, Error, '/*')),
        ('""/*', (2, Error, '/*')),
        ('""/**', (2, Error, '/**')),
    )
)
def test_json_comments_negative_matches(lexer_json, text, expected):
    """Verify that the unfinished or unclosed comments are parsed as errors."""

    tokens = list(lexer_json.get_tokens_unprocessed(text))
    assert expected in tokens

    parsed_text = ''.join(token[2] for token in tokens)
    assert parsed_text == text, 'Input and output texts must match!'


def test_json_escape_backtracking(lexer_json):
    """Confirm that there is no catastrophic backtracking in the lexer.

    This no longer applies because the JSON lexer doesn't use regular expressions,
    but the test is included to ensure no loss of functionality now or in the future.
    """

    fragment = r'{"\u00D0000\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\63CD'
    start_time = time.time()
    list(lexer_json.get_tokens(fragment))
    assert time.time() - start_time < 1, 'The JSON lexer may have catastrophic backtracking'


@pytest.mark.parametrize(
    'keyword',
    (
        'base',
        'container',
        'context',
        'direction',
        'graph',
        'id',
        'import',
        'included',
        'index',
        'json',
        'language',
        'list',
        'nest',
        'none',
        'prefix',
        'propagate',
        'protected',
        'reverse',
        'set',
        'type',
        'value',
        'version',
        'vocab',
    )
)
def test_json_ld_keywords_positive_match(lexer_json_ld, keyword):
    """Validate that JSON-LD keywords are parsed correctly."""

    tokens = list(lexer_json_ld.get_tokens_unprocessed('{"@%s": ""}' % keyword))
    assert len(tokens) == 6
    assert tokens[1][1] is Token.Name.Decorator
    assert tokens[1][2] == '"@%s"' % keyword


@pytest.mark.parametrize(
    'keyword',
    (
        '@bogus',  # "@" does not guarantee a keyword match
        '@bases',  # Begins with the keyword "@base"
        'container',  # Matches "container" but has no leading "@"
    )
)
def test_json_ld_keywords_negative_match(lexer_json_ld, keyword):
    """Validate that JSON-LD non-keywords are parsed correctly."""

    tokens = list(lexer_json_ld.get_tokens_unprocessed('{"%s": ""}' % keyword))
    assert len(tokens) == 6
    assert tokens[1][1] is Token.Name.Tag
    assert tokens[1][2] == '"%s"' % keyword
