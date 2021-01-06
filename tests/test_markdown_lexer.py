# -*- coding: utf-8 -*-
"""
    Pygments Markdown lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest
from pygments.token import Generic, Token, String, Keyword, Name

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


def test_task_list(lexer):
    fragment = '- [ ] sample task'
    tokens = [
        (Keyword, '- '),
        (Keyword, '[ ]'),
        (Token.Text, ' '),
        (Token.Text, 'sample'),
        (Token.Text, ' '),
        (Token.Text, 'task'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '* [ ] sample task'
    tokens = [
        (Keyword, '* '),
        (Keyword, '[ ]'),
        (Token.Text, ' '),
        (Token.Text, 'sample'),
        (Token.Text, ' '),
        (Token.Text, 'task'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '  * [ ] sample task'
    tokens = [
        (Token.Text, '  '),
        (Keyword, '* '),
        (Keyword, '[ ]'),
        (Token.Text, ' '),
        (Token.Text, 'sample'),
        (Token.Text, ' '),
        (Token.Text, 'task'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_bulleted_list(lexer):
    fragment = '* foo\n* bar'
    tokens = [
        (Keyword, '*'),
        (Token.Text, ' '),
        (Token.Text, 'foo'),
        (Token.Text, '\n'),
        (Keyword, '*'),
        (Token.Text, ' '),
        (Token.Text, 'bar'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '- foo\n- bar'
    tokens = [
        (Keyword, '-'),
        (Token.Text, ' '),
        (Token.Text, 'foo'),
        (Token.Text, '\n'),
        (Keyword, '-'),
        (Token.Text, ' '),
        (Token.Text, 'bar'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '* *foo*\n* bar'
    tokens = [
        (Keyword, '*'),
        (Token.Text, ' '),
        (Generic.Emph, '*foo*'),
        (Token.Text, '\n'),
        (Keyword, '*'),
        (Token.Text, ' '),
        (Token.Text, 'bar'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '```\ncode\n```\n* *foo*\n* bar'
    tokens = [
        (String.Backtick, '```\ncode\n```\n'),
        (Keyword, '*'),
        (Token.Text, ' '),
        (Generic.Emph, '*foo*'),
        (Token.Text, '\n'),
        (Keyword, '*'),
        (Token.Text, ' '),
        (Token.Text, 'bar'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_numbered_list(lexer):
    fragment = '1. foo\n2. bar'
    tokens = [
        (Keyword, '1.'),
        (Token.Text, ' '),
        (Token.Text, 'foo'),
        (Token.Text, '\n'),
        (Keyword, '2.'),
        (Token.Text, ' '),
        (Token.Text, 'bar'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_quote(lexer):
    fragment = '> a\n> quote'
    tokens = [
        (Keyword, '> '),
        (Generic.Emph, 'a\n'),
        (Keyword, '> '),
        (Generic.Emph, 'quote\n'),
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
        '```\ncode\n```\n',
        '```\nmulti\n`line`\ncode\n```\n',
    )
    for fragment in fragments:
        tokens = [
            (String.Backtick, fragment),
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_code_block_with_language(lexer):
    fragments = (
        '```python\nimport this\n```\n',
    )
    for fragment in fragments:
        tokens = [
            (String.Backtick, '```'),
            (String.Backtick, 'python'),
            (Token.Text, '\n'),
            (Token.Keyword.Namespace, 'import'),
            (Token.Text, ' '),
            (Token.Name.Namespace, 'this'),
            (Token.Text, '\n'),
            (String.Backtick, '```\n'),
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_inline_code(lexer):
    fragment = 'code: `code`'
    tokens = [
        (Token.Text, 'code:'),
        (Token.Text, ' '),
        (String.Backtick, '`code`'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = ' `**code**`'
    tokens = [
        (Token.Text, ' '),
        (String.Backtick, '`**code**`'),
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

    fragment = '* `code`'
    tokens = [
        (Token.Keyword, '*'),
        (Token.Text, ' '),
        (String.Backtick, '`code`'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '```\ncode\n```\n* nocode\n* `code`'
    tokens = [
        (String.Backtick, '```\ncode\n```\n'),
        (Token.Keyword, '*'),
        (Token.Text, ' '),
        (Token.Text, 'nocode'),
        (Token.Text, '\n'),
        (Token.Keyword, '*'),
        (Token.Text, ' '),
        (String.Backtick, '`code`'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '- `code`'
    tokens = [
        (Token.Keyword, '-'),
        (Token.Text, ' '),
        (String.Backtick, '`code`'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '1. `code`'
    tokens = [
        (Token.Keyword, '1.'),
        (Token.Text, ' '),
        (String.Backtick, '`code`'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = 'code (`in brackets`)'
    tokens = [
        (Token.Text, 'code'),
        (Token.Text, ' '),
        (Token.Text, '('),
        (String.Backtick, '`in brackets`'),
        (Token.Text, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_invalid_bold(lexer):
    fragments = (
        '**no bold__',
        '__no bold**',
        '*no bold*',
        '_no bold_',
    )

    for fragment in fragments:
        for token, _ in lexer.get_tokens(fragment):
            assert token != Generic.Strong


def test_bold_fenced_by_asterisk(lexer):
    fragment = '**bold**'
    tokens = [
        (Generic.Strong, '**bold**'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '(**bold**)'
    tokens = [
        (Token.Text, '('),
        (Generic.Strong, '**bold**'),
        (Token.Text, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_bold_fenced_by_underscore(lexer):
    fragment = '__bold__'
    tokens = [
        (Generic.Strong, '__bold__'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '(__bold__)'
    tokens = [
        (Token.Text, '('),
        (Generic.Strong, '__bold__'),
        (Token.Text, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_invalid_italics(lexer):
    fragments = (
        '*no italics_',
        '_no italics*',
        '**no italics**',
        '__no italics__',
    )

    for fragment in fragments:
        for token, _ in lexer.get_tokens(fragment):
            assert token != Generic.Emph


def test_italics_fenced_by_asterisk(lexer):
    fragment = '*italics*'
    tokens = [
        (Generic.Emph, '*italics*'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '(*italics*)'
    tokens = [
        (Token.Text, '('),
        (Generic.Emph, '*italics*'),
        (Token.Text, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_italics_fenced_by_underscore(lexer):
    fragment = '_italics_'
    tokens = [
        (Generic.Emph, '_italics_'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '(_italics_)'
    tokens = [
        (Token.Text, '('),
        (Generic.Emph, '_italics_'),
        (Token.Text, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_escape_italics(lexer):
    fragments = (
        r'\*no italics\*',
        r'\_ no italics \_',
    )

    for fragment in fragments:
        for token, _ in lexer.get_tokens(fragment):
            assert token != Generic.Emph


def test_italics_no_multiline(lexer):
    fragment = '*no\nitalics*'

    for token, _ in lexer.get_tokens(fragment):
        assert token != Generic.Emph


def test_italics_and_bold(lexer):
    fragment = '**bold** and *italics*'
    tokens = [
        (Generic.Strong, '**bold**'),
        (Token.Text, ' '),
        (Token.Text, 'and'),
        (Token.Text, ' '),
        (Generic.Emph, '*italics*'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '*italics* and **bold**'
    tokens = [
        (Generic.Emph, '*italics*'),
        (Token.Text, ' '),
        (Token.Text, 'and'),
        (Token.Text, ' '),
        (Generic.Strong, '**bold**'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_strikethrough(lexer):
    fragment = '~~striked~~not striked'
    tokens = [
        (Generic.Deleted, '~~striked~~'),
        (Token.Text, 'not'),
        (Token.Text, ' '),
        (Token.Text, 'striked'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_mentions(lexer):
    fragment = 'note for @me:'
    tokens = [
        (Token.Text, 'note'),
        (Token.Text, ' '),
        (Token.Text, 'for'),
        (Token.Text, ' '),
        (Name.Entity, '@me:'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_topics(lexer):
    fragment = 'message to #you:'
    tokens = [
        (Token.Text, 'message'),
        (Token.Text, ' '),
        (Token.Text, 'to'),
        (Token.Text, ' '),
        (Name.Entity, '#you:'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_links(lexer):
    fragment = '[text](link)'
    tokens = [
        (Token.Text, '['),
        (Token.Name.Tag, 'text'),
        (Token.Text, ']'),
        (Token.Text, '('),
        (Token.Name.Attribute, 'link'),
        (Token.Text, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '![Image of foo](https://bar.baz)'
    tokens = [
        (Token.Text, '!['),
        (Token.Name.Tag, 'Image of foo'),
        (Token.Text, ']'),
        (Token.Text, '('),
        (Token.Name.Attribute, 'https://bar.baz'),
        (Token.Text, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_reference_style_links(lexer):
    fragment = '[an example][id]'
    tokens = [
        (Token.Text, '['),
        (Token.Name.Tag, 'an example'),
        (Token.Text, ']'),
        (Token.Text, '['),
        (Token.Name.Label, 'id'),
        (Token.Text, ']'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

    fragment = '[id]: http://example.com'
    tokens = [
        (Token.Text, '['),
        (Token.Name.Label, 'id'),
        (Token.Text, ']: '),
        (Token.Name.Attribute, 'http://example.com'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
