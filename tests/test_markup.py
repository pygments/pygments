"""
    Pygments markup lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.markup import (
    MarkdownLexer, RstLexer, TexLexer, TiddlyWiki5Lexer)
from pygments.token import Name, Operator, Text


def assert_token_offsets(lexer, text):
    """Every token index must point at its own value in the input text.

    ``get_tokens_unprocessed`` documents the first tuple element as the
    starting position of the token within the input text.
    """
    for index, _token, value in lexer.get_tokens_unprocessed(text):
        assert text[index:index + len(value)] == value, \
            f"token {value!r} has wrong index {index} " \
            f"(found {text[index:index + len(value)]!r} there)"


MARKDOWN_FENCED = 'intro\n```python\nx = 1\ny = 2\n```\nend\n'

RST_CODE_BLOCK = (
    'Intro paragraph.\n'
    '\n'
    '.. code-block:: python\n'
    '\n'
    '    x = 1\n'
    '    y = 2\n'
    '\n'
    'Outro.\n'
)

TIDDLYWIKI_CODE = 'Some text\n\n```python\nx = 1\ny = 2\n```\n\nmore\n'


def test_markdown_fenced_code_block_offsets():
    assert_token_offsets(MarkdownLexer(), MARKDOWN_FENCED)


def test_markdown_mentions_with_bot_suffix():
    lexer = MarkdownLexer()
    # A GitHub bot account name carries a literal `[bot]` suffix; the whole
    # mention should be a single Name.Entity token.
    assert list(lexer.get_tokens('@dependabot[bot]')) == [
        (Name.Entity, '@dependabot[bot]'), (Text.Whitespace, '\n')]
    # Plain mentions and topics keep working.
    assert list(lexer.get_tokens('@octocat')) == [
        (Name.Entity, '@octocat'), (Text.Whitespace, '\n')]
    tokens = list(lexer.get_tokens('see #open-source'))
    assert (Name.Entity, '#open-source') in tokens
    # Only the `[bot]` suffix is part of the mention; arbitrary brackets
    # following a mention still lex as a link.
    tokens = list(lexer.get_tokens('@user[foo](https://example.com)'))
    assert (Name.Entity, '@user') in tokens
    assert (Name.Tag, 'foo') in tokens
    assert (Name.Attribute, 'https://example.com') in tokens


def test_rst_code_block_offsets():
    assert_token_offsets(RstLexer(), RST_CODE_BLOCK)


def test_tiddlywiki_code_block_offsets():
    assert_token_offsets(TiddlyWiki5Lexer(), TIDDLYWIKI_CODE)


@pytest.mark.parametrize('environment', [
    'math', 'displaymath', 'equation', 'equation*', 'align', 'align*',
])
def test_tex_math_environments(environment):
    tokens = list(TexLexer().get_tokens(
        rf'\begin{{{environment}}}x+1\end{{{environment}}}'
    ))

    assert (Name.Builtin, 'x') in tokens
    assert (Operator, '+') in tokens
