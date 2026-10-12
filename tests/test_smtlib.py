"""
    Tests for SMT-LIB highlighting.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import get_lexer_by_name, get_lexer_for_filename, guess_lexer
from pygments.token import Comment, Keyword, Name, Number, String, Whitespace


@pytest.mark.parametrize('text, token', [
    ('0', Number.Integer),
    ('123', Number.Integer),
    ('0.125', Number.Float),
    ('#b00101', Number.Bin),
    ('#x01aF', Number.Hex),
    ('forall', Keyword),
    ('lambda', Keyword),
    ('declare-sort-parameter', Keyword),
    ('define-funs-rec', Keyword),
    ('true', Keyword.Constant),
    ('Bool', Keyword.Type),
    (':named', Name.Attribute),
    (':solver-option', Name.Attribute),
    ('|a ; "quoted" name|', Name),
    ('|état\ninitial|', Name),
    ('||', Name),
    ('assertion', Name),
    ('let-value', Name),
    ('Int32', Name),
    ('-32', Name),
    ('+34', Name),
    ('a.b', Name),
])
def test_tokens(text, token):
    lexer = get_lexer_by_name('smtlib')
    assert list(lexer.get_tokens_unprocessed(text)) == [(0, token, text)]


def test_string_escaping():
    lexer = get_lexer_by_name('smtlib')
    text = '"one; \\n ""two""\nthree"'
    assert list(lexer.get_tokens_unprocessed(text)) == [
        (0, String, '"'),
        (1, String, 'one; \\n '),
        (9, String.Escape, '""'),
        (11, String, 'two'),
        (14, String.Escape, '""'),
        (16, String, '\nthree'),
        (22, String, '"'),
    ]


def test_comment_ends_at_carriage_return():
    lexer = get_lexer_by_name('smtlib')
    assert list(lexer.get_tokens_unprocessed('; comment\rcheck-sat')) == [
        (0, Comment.Single, '; comment'),
        (9, Whitespace, '\r'),
        (10, Keyword, 'check-sat'),
    ]


def test_unterminated_string():
    lexer = get_lexer_by_name('smtlib')
    content = '\\' * 10000
    assert list(lexer.get_tokens_unprocessed('"' + content)) == [
        (0, String, '"'),
        (1, String, content),
    ]


@pytest.mark.parametrize('alias', ['smtlib', 'smt-lib', 'smt2'])
def test_alias(alias):
    assert get_lexer_by_name(alias).name == 'SMT-LIB'


def test_filename():
    assert get_lexer_for_filename('problem.smt2').name == 'SMT-LIB'


def test_guess():
    assert guess_lexer('(set-logic QF_LIA)\n(check-sat)\n').name == 'SMT-LIB'


def test_guess_ignores_command_prefixes():
    lexer = get_lexer_by_name('smtlib')
    assert lexer.analyse_text('(check-sat-extra x)') == 0
