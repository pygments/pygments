"""
    Pygments TADS 3 lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import time

import pytest

from pygments.lexers.int_fiction import Tads3Lexer
from pygments.token import Comment


@pytest.fixture(scope='module')
def lexer():
    return Tads3Lexer()


def assert_fast_tokenization(lexer, s):
    """Show that a given string is tokenized quickly."""
    start = time.time()
    tokens = list(lexer.get_tokens_unprocessed(s))
    end = time.time()
    # Isn't 10 seconds kind of a long time?  Yes, but we don't want false
    # positives when the tests are starved for CPU time.
    if end - start > 10:
        pytest.fail('tokenization took too long')
    return tokens


def test_comment_single_backtracking(lexer):
    # The single-line comment pattern used to backtrack catastrophically on
    # runs of backslashes (exponential in the number of backslashes).
    assert_fast_tokenization(lexer, '//' + '\\' * 60)


def test_comment_single_line_continuation(lexer):
    # A backslash directly before a newline continues the comment onto the
    # next line.
    tokens = list(lexer.get_tokens_unprocessed('// foo\\\nbar'))
    assert len(tokens) == 1
    assert tokens[0][1] is Comment.Single
    assert tokens[0][2] == '// foo\\\nbar'


def test_comment_single_ends_at_newline(lexer):
    # Without a backslash, the comment ends at the newline.
    tokens = list(lexer.get_tokens_unprocessed('// foo\nbar'))
    assert tokens[0][1] is Comment.Single
    assert tokens[0][2] == '// foo'


def test_comment_single_escaped_backslash_continuation(lexer):
    # An even run of backslashes before a newline still continues the
    # comment (the last backslash escapes the newline).
    tokens = list(lexer.get_tokens_unprocessed('// /* \\\\\n#define Room Unthing'))
    assert len(tokens) == 1
    assert tokens[0][1] is Comment.Single
    assert tokens[0][2] == '// /* \\\\\n#define Room Unthing'
