"""
    Pygments markup lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexers.markup import MarkdownLexer, RstLexer, TiddlyWiki5Lexer


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


def test_rst_code_block_offsets():
    assert_token_offsets(RstLexer(), RST_CODE_BLOCK)


def test_tiddlywiki_code_block_offsets():
    assert_token_offsets(TiddlyWiki5Lexer(), TIDDLYWIKI_CODE)
