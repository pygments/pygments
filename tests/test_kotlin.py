# -*- coding: utf-8 -*-
"""
    Basic JavaLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Text, Name, Keyword, Punctuation, String
from pygments.lexers import KotlinLexer


@pytest.fixture(scope='module')
def lexer():
    yield KotlinLexer()


def test_can_cope_with_backtick_names_in_functions(lexer):
    fragment = 'fun `wo bble`'
    tokens = [
        (Keyword, 'fun'),
        (Text, ' '),
        (Name.Function, '`wo bble`'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_can_cope_with_commas_and_dashes_in_backtick_Names(lexer):
    fragment = 'fun `wo,-bble`'
    tokens = [
        (Keyword, 'fun'),
        (Text, ' '),
        (Name.Function, '`wo,-bble`'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_can_cope_with_destructuring(lexer):
    fragment = 'val (a, b) = '
    tokens = [
        (Keyword, 'val'),
        (Text, ' '),
        (Punctuation, '('),
        (Name.Property, 'a'),
        (Punctuation, ','),
        (Text, ' '),
        (Name.Property, 'b'),
        (Punctuation, ')'),
        (Text, ' '),
        (Punctuation, '='),
        (Text, ' '),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_can_cope_generics_in_destructuring(lexer):
    fragment = 'val (a: List<Something>, b: Set<Wobble>) ='
    tokens = [
        (Keyword, 'val'),
        (Text, ' '),
        (Punctuation, '('),
        (Name.Property, 'a'),
        (Punctuation, ':'),
        (Text, ' '),
        (Name.Property, 'List'),
        (Punctuation, '<'),
        (Name, 'Something'),
        (Punctuation, '>'),
        (Punctuation, ','),
        (Text, ' '),
        (Name.Property, 'b'),
        (Punctuation, ':'),
        (Text, ' '),
        (Name.Property, 'Set'),
        (Punctuation, '<'),
        (Name, 'Wobble'),
        (Punctuation, '>'),
        (Punctuation, ')'),
        (Text, ' '),
        (Punctuation, '='),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_can_cope_with_generics(lexer):
    fragment = 'inline fun <reified T : ContractState> VaultService.queryBy(): Vault.Page<T> {'
    tokens = [
        (Keyword, 'inline fun'),
        (Text, ' '),
        (Punctuation, '<'),
        (Keyword, 'reified'),
        (Text, ' '),
        (Name, 'T'),
        (Text, ' '),
        (Punctuation, ':'),
        (Text, ' '),
        (Name, 'ContractState'),
        (Punctuation, '>'),
        (Text, ' '),
        (Name.Class, 'VaultService'),
        (Punctuation, '.'),
        (Name.Function, 'queryBy'),
        (Punctuation, '('),
        (Punctuation, ')'),
        (Punctuation, ':'),
        (Text, ' '),
        (Name, 'Vault'),
        (Punctuation, '.'),
        (Name, 'Page'),
        (Punctuation, '<'),
        (Name, 'T'),
        (Punctuation, '>'),
        (Text, ' '),
        (Punctuation, '{'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_should_cope_with_multiline_comments(lexer):
    fragment = '"""\nthis\nis\na\ncomment"""'
    tokens = [
        (String, '"""\nthis\nis\na\ncomment"""'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
