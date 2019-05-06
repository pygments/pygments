# -*- coding: utf-8 -*-
"""
    Pygments Markdown lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.markup import MarkdownLexer


@pytest.fixture(scope='module')
def lexer():
    yield MarkdownLexer()


def assert_same_text(lexer, text):
    """Show that lexed markdown does not remove any content. """
    tokens = list(lexer.get_tokens_unprocessed(text))
    output = ''.join(t[2] for t in tokens)
    assert text == output


def test_code_fence(lexer):
    assert_same_text(lexer, r'```\nfoo\n```\n')


def test_code_fence_gsm(lexer):
    assert_same_text(lexer, r'```markdown\nfoo\n```\n')


def test_code_fence_gsm_with_no_lexer(lexer):
    assert_same_text(lexer, r'```invalid-lexer\nfoo\n```\n')
