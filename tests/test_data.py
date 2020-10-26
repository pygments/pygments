# -*- coding: utf-8 -*-
"""
    Data Tests
    ~~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import time

import pytest

from pygments.lexers.data import JsonLexer, JsonBareObjectLexer, JsonLdLexer, YamlLexer
from pygments.token import Token, Punctuation, Text, Number, String, Keyword, Name


@pytest.fixture(scope='module')
def lexer_json():
    yield JsonLexer()


@pytest.fixture(scope='module')
def lexer_bare():
    yield JsonBareObjectLexer()


@pytest.fixture(scope='module')
def lexer_json_ld():
    yield JsonLdLexer()


@pytest.fixture(scope='module')
def lexer_yaml():
    yield YamlLexer()


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

            # Whitespace
            ('\u0020', (Text,)),  # space
            ('\u000a', (Text,)),  # newline
            ('\u000d', (Text,)),  # carriage return
            ('\u0009', (Text,)),  # tab

            # Arrays
            ('[]', (Punctuation,)),
            ('["a", "b"]', (Punctuation, String.Double, Punctuation, Text, String.Double, Punctuation)),

            # Objects
            ('{}', (Punctuation,)),
            ('{"a": "b"}', (Punctuation, Name.Tag, Punctuation, Text, String.Double, Punctuation)),
    )
)
def test_json_literals_positive_match(lexer_json, text, expected_token_types):
    """Validate that syntactically-correct JSON literals are parsed correctly."""

    tokens = list(lexer_json.get_tokens_unprocessed(text))
    assert len(tokens) == len(expected_token_types)
    assert all(token[1] is expected_token for token, expected_token in zip(tokens, expected_token_types))
    assert ''.join(token[2] for token in tokens) == text


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


@pytest.mark.parametrize('text', ('+\n', '0\n', '""0\n', 'a\nb\n',))
def test_json_round_trip_errors(lexer_json, text):
    """Validate that past round-trip errors never crop up again."""

    tokens = list(lexer_json.get_tokens_unprocessed(text))
    assert ''.join(t[2] for t in tokens) == text


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


def test_basic_json(lexer_json):
    fragment = '{"foo": "bar", "foo2": [1, 2, 3], "\\u0123": "\\u0123"}\n'
    tokens = [
        (Token.Punctuation, '{'),
        (Token.Name.Tag, '"foo"'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Literal.String.Double, '"bar"'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Name.Tag, '"foo2"'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Punctuation, '['),
        (Token.Literal.Number.Integer, '1'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '2'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '3'),
        (Token.Punctuation, '],'),
        (Token.Text, ' '),
        (Token.Name.Tag, '"\\u0123"'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Literal.String.Double, '"\\u0123"'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_json.get_tokens(fragment)) == tokens


def test_basic_bare(lexer_bare):
    # This is the same as testBasic for JsonLexer above, except the
    # enclosing curly braces are removed.
    fragment = '"foo": "bar", "foo2": [1, 2, 3]\n'
    tokens = [
        (Token.Name.Tag, '"foo"'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Literal.String.Double, '"bar"'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Name.Tag, '"foo2"'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Punctuation, '['),
        (Token.Literal.Number.Integer, '1'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '2'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '3'),
        (Token.Punctuation, ']'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_bare.get_tokens(fragment)) == tokens


def test_yaml(lexer_yaml):
    # Bug #1528: This previously parsed 'token # innocent' as a tag
    fragment = 'here: token # innocent: comment\n'
    tokens = [
        (Token.Name.Tag, 'here'),
        (Token.Punctuation, ':'),
        (Token.Text, ' '),
        (Token.Literal.Scalar.Plain, 'token'),
        (Token.Text, ' '),
        (Token.Comment.Single, '# innocent: comment'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_yaml.get_tokens(fragment)) == tokens
