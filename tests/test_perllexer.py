# -*- coding: utf-8 -*-
"""
    Pygments regex lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import time

import pytest

from pygments.token import Keyword, Name, String, Text
from pygments.lexers.perl import PerlLexer


@pytest.fixture(scope='module')
def lexer():
    yield PerlLexer()


# Test runaway regexes.
# A previous version of the Perl lexer would spend a great deal of
# time backtracking when given particular strings.  These tests show that
# the runaway backtracking doesn't happen any more (at least for the given
# cases).


# Test helpers.

def assert_single_token(lexer, s, token):
    """Show that a given string generates only one token."""
    tokens = list(lexer.get_tokens_unprocessed(s))
    assert len(tokens) == 1
    assert s == tokens[0][2]
    assert token == tokens[0][1]


def assert_tokens(lexer, strings, expected_tokens):
    """Show that a given string generates the expected tokens."""
    tokens = list(lexer.get_tokens_unprocessed(''.join(strings)))
    assert len(tokens) == len(expected_tokens)
    for index, s in enumerate(strings):
        assert s == tokens[index][2]
        assert expected_tokens[index] == tokens[index][1]


def assert_fast_tokenization(lexer, s):
    """Show that a given string is tokenized quickly."""
    start = time.time()
    tokens = list(lexer.get_tokens_unprocessed(s))
    end = time.time()
    # Isn't 10 seconds kind of a long time?  Yes, but we don't want false
    # positives when the tests are starved for CPU time.
    if end-start > 10:
        pytest.fail('tokenization took too long')
    return tokens


# Strings.

def test_single_quote_strings(lexer):
    assert_single_token(lexer, r"'foo\tbar\\\'baz'", String)
    assert_fast_tokenization(lexer, "'" + '\\'*999)


def test_double_quote_strings(lexer):
    assert_single_token(lexer, r'"foo\tbar\\\"baz"', String)
    assert_fast_tokenization(lexer, '"' + '\\'*999)


def test_backtick_strings(lexer):
    assert_single_token(lexer, r'`foo\tbar\\\`baz`', String.Backtick)
    assert_fast_tokenization(lexer, '`' + '\\'*999)


# Regex matches with various delimiters.

def test_match(lexer):
    assert_single_token(lexer, r'/aa\tbb/', String.Regex)
    assert_fast_tokenization(lexer, '/' + '\\'*999)


def test_match_with_slash(lexer):
    assert_tokens(lexer, ['m', '/\n\\t\\\\/'], [String.Regex, String.Regex])
    assert_fast_tokenization(lexer, 'm/xxx\n' + '\\'*999)


def test_match_with_bang(lexer):
    assert_tokens(lexer, ['m', r'!aa\t\!bb!'], [String.Regex, String.Regex])
    assert_fast_tokenization(lexer, 'm!' + '\\'*999)


def test_match_with_brace(lexer):
    assert_tokens(lexer, ['m', r'{aa\t\}bb}'], [String.Regex, String.Regex])
    assert_fast_tokenization(lexer, 'm{' + '\\'*999)


def test_match_with_angle_brackets(lexer):
    assert_tokens(lexer, ['m', r'<aa\t\>bb>'], [String.Regex, String.Regex])
    assert_fast_tokenization(lexer, 'm<' + '\\'*999)


def test_match_with_parenthesis(lexer):
    assert_tokens(lexer, ['m', r'(aa\t\)bb)'], [String.Regex, String.Regex])
    assert_fast_tokenization(lexer, 'm(' + '\\'*999)


def test_match_with_at_sign(lexer):
    assert_tokens(lexer, ['m', r'@aa\t\@bb@'], [String.Regex, String.Regex])
    assert_fast_tokenization(lexer, 'm@' + '\\'*999)


def test_match_with_percent_sign(lexer):
    assert_tokens(lexer, ['m', r'%aa\t\%bb%'], [String.Regex, String.Regex])
    assert_fast_tokenization(lexer, 'm%' + '\\'*999)


def test_match_with_dollar_sign(lexer):
    assert_tokens(lexer, ['m', r'$aa\t\$bb$'], [String.Regex, String.Regex])
    assert_fast_tokenization(lexer, 'm$' + '\\'*999)


# Regex substitutions with various delimeters.

def test_substitution_with_slash(lexer):
    assert_single_token(lexer, 's/aaa/bbb/g', String.Regex)
    assert_fast_tokenization(lexer, 's/foo/' + '\\'*999)


def test_substitution_with_at_sign(lexer):
    assert_single_token(lexer, r's@aaa@bbb@g', String.Regex)
    assert_fast_tokenization(lexer, 's@foo@' + '\\'*999)


def test_substitution_with_percent_sign(lexer):
    assert_single_token(lexer, r's%aaa%bbb%g', String.Regex)
    assert_fast_tokenization(lexer, 's%foo%' + '\\'*999)


def test_substitution_with_brace(lexer):
    assert_single_token(lexer, r's{aaa}', String.Regex)
    assert_fast_tokenization(lexer, 's{' + '\\'*999)


def test_substitution_with_angle_bracket(lexer):
    assert_single_token(lexer, r's<aaa>', String.Regex)
    assert_fast_tokenization(lexer, 's<' + '\\'*999)


def test_substitution_with_square_bracket(lexer):
    assert_single_token(lexer, r's[aaa]', String.Regex)
    assert_fast_tokenization(lexer, 's[' + '\\'*999)


def test_substitution_with_parenthesis(lexer):
    assert_single_token(lexer, r's(aaa)', String.Regex)
    assert_fast_tokenization(lexer, 's(' + '\\'*999)


# Namespaces/modules

def test_package_statement(lexer):
    assert_tokens(lexer, ['package', ' ', 'Foo'], [Keyword, Text, Name.Namespace])
    assert_tokens(lexer, ['package', '  ', 'Foo::Bar'], [Keyword, Text, Name.Namespace])


def test_use_statement(lexer):
    assert_tokens(lexer, ['use', ' ', 'Foo'], [Keyword, Text, Name.Namespace])
    assert_tokens(lexer, ['use', '  ', 'Foo::Bar'], [Keyword, Text, Name.Namespace])


def test_no_statement(lexer):
    assert_tokens(lexer, ['no', ' ', 'Foo'], [Keyword, Text, Name.Namespace])
    assert_tokens(lexer, ['no', '  ', 'Foo::Bar'], [Keyword, Text, Name.Namespace])


def test_require_statement(lexer):
    assert_tokens(lexer, ['require', ' ', 'Foo'], [Keyword, Text, Name.Namespace])
    assert_tokens(lexer, ['require', '  ', 'Foo::Bar'], [Keyword, Text, Name.Namespace])
    assert_tokens(lexer, ['require', ' ', '"Foo/Bar.pm"'], [Keyword, Text, String])
