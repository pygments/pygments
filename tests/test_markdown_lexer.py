# -*- coding: utf-8 -*-
"""
    Pygments Markdown lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest
from pygments.token import Generic, Token, String

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


def test_invalid_atx_heading(lexer):
    fragments = (
        '#',
        'a #',
        '*#',
    )

    for fragment in fragments:
        for token, _ in lexer.get_tokens(fragment):
            assert token != Generic.Heading


def test_atx_heading(lexer):
    fragments = (
        '#Heading',
        '# Heading',
        '# Another heading',
        '# Another # heading',
        '# Heading #',
    )

    for fragment in fragments:
        tokens = [
            (Generic.Heading, fragment),
            (Token.Text, '\n'),
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_invalid_atx_subheading(lexer):
    fragments = (
        '##',
        'a ##',
        '*##',
        '####### too many hashes'
    )

    for fragment in fragments:
        for token, _ in lexer.get_tokens(fragment):
            assert token != Generic.Subheading


def test_atx_subheading(lexer):
    fragments = (
        '##Subheading',
        '## Subheading',
        '### Subheading',
        '#### Subheading',
        '##### Subheading',
        '###### Subheading',
        '## Another subheading',
        '## Another ## subheading',
        '###### Subheading #',
        '###### Subheading ######',
    )

    for fragment in fragments:
        tokens = [
            (Generic.Subheading, fragment),
            (Token.Text, '\n'),
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_invalid_setext_heading(lexer):
    fragments = (
        'Heading\n',
        'Heading\n_',
        'Heading\n =====',
        'Heading\na=====',
        '=====',
        '\n=\n',
        'Heading\n=====Text'
    )

    for fragment in fragments:
        for token, _ in lexer.get_tokens(fragment):
            assert token != Generic.Heading


def test_setext_heading(lexer):
    fragments = (
        'Heading\n=',
        'Heading\n=======',
        'Heading\n==========',
    )

    for fragment in fragments:
        tokens = [
            (Generic.Heading, fragment.split('\n')[0]),
            (Token.Text, '\n'),
            (Generic.Heading, fragment.split('\n')[1]),
            (Token.Text, '\n'),
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_invalid_setext_subheading(lexer):
    fragments = (
        'Subheading\n',
        'Subheading\n_',
        'Subheading\n -----',
        'Subheading\na-----',
        '-----',
        '\n-\n',
        'Subheading\n-----Text'
    )

    for fragment in fragments:
        for token, _ in lexer.get_tokens(fragment):
            assert token != Generic.Subheading


def test_setext_subheading(lexer):
    fragments = (
        'Subheading\n-',
        'Subheading\n----------',
        'Subheading\n-----------',
    )

    for fragment in fragments:
        tokens = [
            (Generic.Subheading, fragment.split('\n')[0]),
            (Token.Text, '\n'),
            (Generic.Subheading, fragment.split('\n')[1]),
            (Token.Text, '\n'),
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_inline_code(lexer):
    fragment = '`code`'
    tokens = [
        (String.Backtick, '`code`'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '(`code`)'
    tokens = [
        (Token.Text, '('),
        (String.Backtick, '`code`'),
        (Token.Text, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = 'inline `code`!'
    tokens = [
        (Token.Text, 'inline '),
        (String.Backtick, '`code`'),
        (Token.Text, '!'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '`**code**`'
    tokens = [
        (String.Backtick, '`**code**`'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_invalid_code_block(lexer):
    fragments = (
        '```code```',
        'prefix not allowed before ```\ncode block\n```'
        '   code',
    )

    for fragment in fragments:
        for token, _ in lexer.get_tokens(fragment):
            assert token != String.Backtick


def test_code_block_fenced_by_backticks(lexer):
    fragments = (
        '```\ncode\n```',
        '```\nmulti\n`line`\ncode\n```',
    )
    for fragment in fragments:
        tokens = [
            (String.Backtick, fragment),
            (Token.Text, '\n'),
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_code_block_indented_by_spaces(lexer):
    fragment = '    code'
    tokens = [
        (Token.Text, '    '),
        (String.Backtick, 'code'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '      this is also **code**'
    tokens = [
        (Token.Text, '    '),
        (String.Backtick, '  this is also **code**'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '\tcode'
    tokens = [
        (Token.Text, '\t'),
        (String.Backtick, 'code'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
