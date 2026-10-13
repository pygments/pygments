"""
    Tests for Fortran string literals with literal backslashes.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.fortran import FortranLexer, FortranFixedLexer
from pygments.token import Comment, Error, Keyword, String


@pytest.mark.parametrize('lexer_class', [FortranLexer, FortranFixedLexer])
@pytest.mark.parametrize('quote', ["'", '"'])
@pytest.mark.parametrize('content', ['C:\\path\\', '\\', "don't" + ' "quote" \\'])
def test_literal_backslashes(lexer_class, quote, content):
    literal = quote + content.replace(quote, quote * 2) + quote
    source = f'      path = {literal}\n      WRITE(*,*) path\n'
    tokens = list(lexer_class(backslash_escapes=False).get_tokens(source))
    assert [(token, value) for token, value in tokens if token in String] == [
        (String.Single if quote == "'" else String.Double, literal),
    ]
    assert (Keyword, 'WRITE') in tokens
    assert not any(token is Error for token, _ in tokens)


@pytest.mark.parametrize('lexer_class', [FortranLexer, FortranFixedLexer])
def test_backslash_option_accepts_command_line_value(lexer_class):
    literal = "'C:\\path\\'"
    tokens = list(lexer_class(backslash_escapes='false').get_tokens(
        f'      path = {literal}\n'
    ))
    assert (String.Single, literal) in tokens


@pytest.mark.parametrize('lexer_class', [FortranLexer, FortranFixedLexer])
def test_comments_after_literal_backslashes(lexer_class):
    tokens = list(lexer_class(backslash_escapes=False).get_tokens(
        "      path = '\\' ! a comment\n      WRITE(*,*) path\n"
    ))
    assert (String.Single, "'\\'") in tokens
    assert any(token is Comment and 'a comment' in value for token, value in tokens)
    assert (Keyword, 'WRITE') in tokens


@pytest.mark.parametrize('lexer_class', [FortranLexer, FortranFixedLexer])
def test_default_backslash_escaping_is_preserved(lexer_class):
    source = r'''      path = 'it\'s' // "a\"b"'''
    default_tokens = list(lexer_class().get_tokens(source))
    assert default_tokens == list(lexer_class(backslash_escapes=True).get_tokens(source))
    assert (String.Single, r"'it\'s'") in default_tokens
    assert (String.Double, r'"a\"b"') in default_tokens


def test_backslash_option_is_local_to_each_instance():
    source = "path = '\\'\nWRITE(*,*) path\n"
    escaped = FortranLexer()
    literal = FortranLexer(backslash_escapes=False)
    assert (String.Single, "'\\'") in list(literal.get_tokens(source))
    assert list(escaped.get_tokens(source)) == list(FortranLexer().get_tokens(source))
