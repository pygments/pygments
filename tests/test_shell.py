# -*- coding: utf-8 -*-
"""
    Basic Shell Tests
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import BashLexer, BashSessionLexer, MSDOSSessionLexer, \
    PowerShellSessionLexer


@pytest.fixture(scope='module')
def lexer_bash():
    yield BashLexer()


@pytest.fixture(scope='module')
def lexer_session():
    yield BashSessionLexer()


@pytest.fixture(scope='module')
def lexer_msdos():
    yield MSDOSSessionLexer()


@pytest.fixture(scope='module')
def lexer_powershell_session():
    yield PowerShellSessionLexer()


def test_curly_no_escape_and_quotes(lexer_bash):
    fragment = 'echo "${a//["b"]/}"\n'
    tokens = [
        (Token.Name.Builtin, 'echo'),
        (Token.Text, ' '),
        (Token.Literal.String.Double, '"'),
        (Token.String.Interpol, '${'),
        (Token.Name.Variable, 'a'),
        (Token.Punctuation, '//['),
        (Token.Literal.String.Double, '"b"'),
        (Token.Punctuation, ']/'),
        (Token.String.Interpol, '}'),
        (Token.Literal.String.Double, '"'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_bash.get_tokens(fragment)) == tokens


def test_curly_with_escape(lexer_bash):
    fragment = 'echo ${a//[\\"]/}\n'
    tokens = [
        (Token.Name.Builtin, 'echo'),
        (Token.Text, ' '),
        (Token.String.Interpol, '${'),
        (Token.Name.Variable, 'a'),
        (Token.Punctuation, '//['),
        (Token.Literal.String.Escape, '\\"'),
        (Token.Punctuation, ']/'),
        (Token.String.Interpol, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_bash.get_tokens(fragment)) == tokens


def test_parsed_single(lexer_bash):
    fragment = "a=$'abc\\''\n"
    tokens = [
        (Token.Name.Variable, 'a'),
        (Token.Operator, '='),
        (Token.Literal.String.Single, "$'abc\\''"),
        (Token.Text, '\n'),
    ]
    assert list(lexer_bash.get_tokens(fragment)) == tokens


def test_short_variable_names(lexer_bash):
    fragment = 'x="$"\ny="$_"\nz="$abc"\n'
    tokens = [
        # single lone $
        (Token.Name.Variable, 'x'),
        (Token.Operator, '='),
        (Token.Literal.String.Double, '"'),
        (Token.Text, '$'),
        (Token.Literal.String.Double, '"'),
        (Token.Text, '\n'),
        # single letter shell var
        (Token.Name.Variable, 'y'),
        (Token.Operator, '='),
        (Token.Literal.String.Double, '"'),
        (Token.Name.Variable, '$_'),
        (Token.Literal.String.Double, '"'),
        (Token.Text, '\n'),
        # multi-letter user var
        (Token.Name.Variable, 'z'),
        (Token.Operator, '='),
        (Token.Literal.String.Double, '"'),
        (Token.Name.Variable, '$abc'),
        (Token.Literal.String.Double, '"'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_bash.get_tokens(fragment)) == tokens


def test_array_nums(lexer_bash):
    fragment = 'a=(1 2 3)\n'
    tokens = [
        (Token.Name.Variable, 'a'),
        (Token.Operator, '='),
        (Token.Operator, '('),
        (Token.Literal.Number, '1'),
        (Token.Text, ' '),
        (Token.Literal.Number, '2'),
        (Token.Text, ' '),
        (Token.Literal.Number, '3'),
        (Token.Operator, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_bash.get_tokens(fragment)) == tokens


def test_end_of_line_nums(lexer_bash):
    fragment = 'a=1\nb=2 # comment\n'
    tokens = [
        (Token.Name.Variable, 'a'),
        (Token.Operator, '='),
        (Token.Literal.Number, '1'),
        (Token.Text, '\n'),
        (Token.Name.Variable, 'b'),
        (Token.Operator, '='),
        (Token.Literal.Number, '2'),
        (Token.Text, ' '),
        (Token.Comment.Single, '# comment\n'),
    ]
    assert list(lexer_bash.get_tokens(fragment)) == tokens


def test_newline_in_echo(lexer_session):
    fragment = '$ echo \\\nhi\nhi\n'
    tokens = [
        (Token.Generic.Prompt, '$ '),
        (Token.Name.Builtin, 'echo'),
        (Token.Text, ' '),
        (Token.Literal.String.Escape, '\\\n'),
        (Token.Text, 'hi'),
        (Token.Text, '\n'),
        (Token.Generic.Output, 'hi\n'),
    ]
    assert list(lexer_session.get_tokens(fragment)) == tokens


def test_newline_in_ls(lexer_session):
    fragment = '$ ls \\\nhi\nhi\n'
    tokens = [
        (Token.Generic.Prompt, '$ '),
        (Token.Text, 'ls'),
        (Token.Text, ' '),
        (Token.Literal.String.Escape, '\\\n'),
        (Token.Text, 'hi'),
        (Token.Text, '\n'),
        (Token.Generic.Output, 'hi\n'),
    ]
    assert list(lexer_session.get_tokens(fragment)) == tokens


def test_comment_after_prompt(lexer_session):
    fragment = '$# comment'
    tokens = [
        (Token.Generic.Prompt, '$'),
        (Token.Comment.Single, '# comment\n'),
    ]
    assert list(lexer_session.get_tokens(fragment)) == tokens


def test_msdos_gt_only(lexer_msdos):
    fragment = '> py\nhi\n'
    tokens = [
        (Token.Generic.Prompt, '>'),
        (Token.Text, ' '),
        (Token.Text, 'py'),
        (Token.Text, '\n'),
        (Token.Generic.Output, 'hi\n'),
    ]
    assert list(lexer_msdos.get_tokens(fragment)) == tokens


def test_powershell_session(lexer_powershell_session):
    fragment = 'PS C:\\> Get-ChildItem\n'
    tokens = [
        (Token.Generic.Prompt, 'PS C:\\> '),
        (Token.Name.Builtin, 'Get-ChildItem'),
        (Token.Text, '\n')
    ]
    assert list(lexer_powershell_session.get_tokens(fragment)) == tokens

    fragment = 'PS> Get-ChildItem\n'
    tokens = [
        (Token.Generic.Prompt, 'PS> '),
        (Token.Name.Builtin, 'Get-ChildItem'),
        (Token.Text, '\n')
    ]
    assert list(lexer_powershell_session.get_tokens(fragment)) == tokens

    fragment = 'PS > Get-ChildItem\n'
    tokens = [
        (Token.Generic.Prompt, 'PS > '),
        (Token.Name.Builtin, 'Get-ChildItem'),
        (Token.Text, '\n')
    ]
    assert list(lexer_powershell_session.get_tokens(fragment)) == tokens


def test_powershell_remoting_session(lexer_powershell_session):
    fragment = '[Long-NetBIOS-Hostname]: PS C:\\> Get-ChildItem\n'
    tokens = [
        (Token.Generic.Prompt, '[Long-NetBIOS-Hostname]: PS C:\\> '),
        (Token.Name.Builtin, 'Get-ChildItem'),
        (Token.Text, '\n')
    ]
    assert list(lexer_powershell_session.get_tokens(fragment)) == tokens


def test_virtualenv(lexer_session):
    fragment = '(env) [~/project]$ foo -h\n'
    tokens = [
        (Token.Generic.Prompt.VirtualEnv, '(env)'),
        (Token.Text, ' '),
        (Token.Generic.Prompt, '[~/project]$ '),
        (Token.Text, 'foo'),
        (Token.Text, ' '),
        (Token.Text, '-h'),
        (Token.Text, '\n'),
    ]
    assert list(lexer_session.get_tokens(fragment)) == tokens
