# -*- coding: utf-8 -*-
"""
    Basic JavaLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Text, Name, Keyword, Punctuation, String
from pygments.lexers import KotlinLexer


@pytest.fixture(scope='module')
def lexer():
    yield KotlinLexer()


def test_can_cope_with_backtick_names_in_functions(lexer):
    fragment = u'fun `wo bble`'
    tokens = [
        (Keyword, u'fun'),
        (Text, u' '),
        (Name.Function, u'`wo bble`'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_can_cope_with_commas_and_dashes_in_backtick_Names(lexer):
    fragment = u'fun `wo,-bble`'
    tokens = [
        (Keyword, u'fun'),
        (Text, u' '),
        (Name.Function, u'`wo,-bble`'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_can_cope_with_destructuring(lexer):
    fragment = u'val (a, b) = '
    tokens = [
        (Keyword, u'val'),
        (Text, u' '),
        (Punctuation, u'('),
        (Name.Property, u'a'),
        (Punctuation, u','),
        (Text, u' '),
        (Name.Property, u'b'),
        (Punctuation, u')'),
        (Text, u' '),
        (Punctuation, u'='),
        (Text, u' '),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_can_cope_generics_in_destructuring(lexer):
    fragment = u'val (a: List<Something>, b: Set<Wobble>) ='
    tokens = [
        (Keyword, u'val'),
        (Text, u' '),
        (Punctuation, u'('),
        (Name.Property, u'a'),
        (Punctuation, u':'),
        (Text, u' '),
        (Name.Property, u'List'),
        (Punctuation, u'<'),
        (Name, u'Something'),
        (Punctuation, u'>'),
        (Punctuation, u','),
        (Text, u' '),
        (Name.Property, u'b'),
        (Punctuation, u':'),
        (Text, u' '),
        (Name.Property, u'Set'),
        (Punctuation, u'<'),
        (Name, u'Wobble'),
        (Punctuation, u'>'),
        (Punctuation, u')'),
        (Text, u' '),
        (Punctuation, u'='),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_can_cope_with_generics(lexer):
    fragment = u'inline fun <reified T : ContractState> VaultService.queryBy(): Vault.Page<T> {'
    tokens = [
        (Keyword, u'inline fun'),
        (Text, u' '),
        (Punctuation, u'<'),
        (Keyword, u'reified'),
        (Text, u' '),
        (Name, u'T'),
        (Text, u' '),
        (Punctuation, u':'),
        (Text, u' '),
        (Name, u'ContractState'),
        (Punctuation, u'>'),
        (Text, u' '),
        (Name.Class, u'VaultService'),
        (Punctuation, u'.'),
        (Name.Function, u'queryBy'),
        (Punctuation, u'('),
        (Punctuation, u')'),
        (Punctuation, u':'),
        (Text, u' '),
        (Name, u'Vault'),
        (Punctuation, u'.'),
        (Name, u'Page'),
        (Punctuation, u'<'),
        (Name, u'T'),
        (Punctuation, u'>'),
        (Text, u' '),
        (Punctuation, u'{'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_should_cope_with_multiline_comments(lexer):
    fragment = u'"""\nthis\nis\na\ncomment"""'
    tokens = [
        (String, u'"""\nthis\nis\na\ncomment"""'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
