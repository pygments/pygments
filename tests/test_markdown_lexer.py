"""
    Pygments Markdown lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
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


def test_invalid_code_block(lexer):
    fragments = (
        '```code```',
        'prefix not allowed before ```\ncode block\n```'
        '   code',
    )

    for fragment in fragments:
        for token, _ in lexer.get_tokens(fragment):
            assert token != String.Backtick


BOLD_ITA_MARKUP_TEST_CASES = (
    ('_ita_', [(Generic.Emph, '_ita_'), (Token.Text, '\n')]),
    ('*ita*', [(Generic.Emph, '*ita*'), (Token.Text, '\n')]),
    ('__bold__', [(Generic.Strong, '__bold__'), (Token.Text, '\n')]),
    ('**bold**', [(Generic.Strong, '**bold**'), (Token.Text, '\n')]),

    ('_italic\nit be_', [(Generic.Emph, '_italic\nit be_'),
                         (Token.Text, '\n')]),
    ('_text _', [(Token.Text, '_text'),
                 (Token.Text, ' '),
                 (Token.Text, '_'),
                 (Token.Text, '\n')]),
    ('_ text_', [(Token.Text, '_'),
                 (Token.Text, ' '),
                 (Token.Text, 'text_'),
                 (Token.Text, '\n')]),

    ('*italic\nit be*', [(Generic.Emph, '*italic\nit be*'),
                         (Token.Text, '\n')]),
    ('*text *', [(Token.Text, '*text'),
                 (Token.Text, ' '),
                 (Token.Text, '*'),
                 (Token.Text, '\n')]),
    ('* text*', [(Token.Keyword, '*'),
                 (Token.Text, ' '),
                 (Token.Text, 'text*'),
                 (Token.Text, '\n')]),

    ('__bold\nit be__', [(Generic.Strong, '__bold\nit be__'),
                         (Token.Text, '\n')]),
    ('__text __', [(Token.Text, '__text'),
                   (Token.Text, ' '),
                   (Token.Text, '__'),
                   (Token.Text, '\n')]),
    ('__ text__', [(Token.Text, '__'),
                   (Token.Text, ' '),
                   (Token.Text, 'text__'),
                   (Token.Text, '\n')]),

    ('**bold\nit be**', [(Generic.Strong, '**bold\nit be**'),
                         (Token.Text, '\n')]),
    ('**text **', [(Token.Text, '**text'),
                   (Token.Text, ' '),
                   (Token.Text, '**'),
                   (Token.Text, '\n')]),
    ('** text**', [(Token.Text, '**'),
                   (Token.Text, ' '),
                   (Token.Text, 'text**'),
                   (Token.Text, '\n')]),

    # something for the future: expressions can contain underscores
    # ('_text_text_', [(Generic.Emph, '_text_text_'),
    #                    (Token.Text, '\n')]),
    #
    # ('_text__text__', [(Generic.Emph, '_text__text__'),
    #                    (Token.Text, '\n')]),
)


@pytest.mark.parametrize("fragment,tokens", BOLD_ITA_MARKUP_TEST_CASES)
def test_bold_ita_markup(lexer, fragment, tokens):
    assert list(lexer.get_tokens(fragment)) == tokens


QUOTE_MARKUP_TEST_CASES = (
    ('> sometext', [(Token.Keyword, "> "),
                    (Generic.Emph, "sometext\n")]),
    ('>> sometext', [(Token.Keyword, ">> "),
                     (Generic.Emph, "sometext\n")]),
    ('> > sometext', [(Token.Keyword, "> > "),
                      (Generic.Emph, "sometext\n")]),
)


@pytest.mark.parametrize("fragment,tokens", QUOTE_MARKUP_TEST_CASES)
def test_quote(lexer, fragment, tokens):
    assert list(lexer.get_tokens(fragment)) == tokens
