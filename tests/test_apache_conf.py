# -*- coding: utf-8 -*-
"""
    Basic Apache Configuration Test
    ~~~~~~~~~~~~~~~~~--------------

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import textwrap

import pytest

from pygments.token import Text, Number, Token
from pygments.lexers import configs


@pytest.fixture(scope='module')
def lexer():
    yield configs.ApacheConfLexer()


def test_multiline_comment(lexer):
    fragment = '#SecAction \\\n  "id:\'900004\', \\\n  phase:1, \\\n  t:none, \\\n  setvar:tx.anomaly_score_blocking=on, \\\n  nolog, \\\n  pass"\n  \n'
    tokens = [
        (Token.Comment, '#SecAction \\\n  "id:\'900004\', \\\n  phase:1, \\\n  t:none, \\\n  setvar:tx.anomaly_score_blocking=on, \\\n  nolog, \\\n  pass"'),
        (Token.Text, '\n  \n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_multiline_argument(lexer):
        fragment = 'SecAction \\\n  "id:\'900001\', \\\n  phase:1, \\\n  t:none, \\\n  setvar:tx.critical_anomaly_score=5, \\\n  setvar:tx.error_anomaly_score=4, \\\n  setvar:tx.warning_anomaly_score=3, \\\n  setvar:tx.notice_anomaly_score=2, \\\n  nolog, \\\n  pass"\n'
        tokens = [
            (Token.Name.Builtin, 'SecAction'),
            (Token.Text, ' '),
            (Token.Text, '\\\n'),
            (Token.Text, '  '),
            (Token.Literal.String.Double, '"id:\'900001\', \\\n  phase:1, \\\n  t:none, \\\n  setvar:tx.critical_anomaly_score=5, \\\n  setvar:tx.error_anomaly_score=4, \\\n  setvar:tx.warning_anomaly_score=3, \\\n  setvar:tx.notice_anomaly_score=2, \\\n  nolog, \\\n  pass"'),
            (Token.Text, ''),
            (Token.Text, '\n'),
        ]
        assert list(lexer.get_tokens(fragment)) == tokens

def test_directive_no_args(lexer):
    fragment = 'Example\nServerName localhost'
    tokens = [
            (Token.Name.Builtin, 'Example'),
            (Token.Text, ''),
            (Token.Text, '\n'),
            (Token.Name.Builtin, 'ServerName'),
            (Token.Text, ' '),
            (Token.Text, 'localhost'),
            (Token.Text, ''),
            (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_include_globs(lexer):
    fragment = 'Include /etc/httpd/conf.d/*.conf'
    tokens = [
            (Token.Name.Builtin, 'Include'),
            (Token.Text, ' '),
            (Token.String.Other, '/etc/httpd/conf.d/*.conf'),
            (Token.Text, ''),
            (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_multi_include_globs(lexer):
    fragment = 'Include /etc/httpd/conf.d/*/*.conf'
    tokens = [
            (Token.Name.Builtin, 'Include'),
            (Token.Text, ' '),
            (Token.String.Other, '/etc/httpd/conf.d/*/*.conf'),
            (Token.Text, ''),
            (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_multi_include_globs_root(lexer):
    fragment = 'Include /*conf/*.conf'
    tokens = [
            (Token.Name.Builtin, 'Include'),
            (Token.Text, ' '),
            (Token.String.Other, '/*conf/*.conf'),
            (Token.Text, ''),
            (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_fix_lock_absolute_path(lexer):
    fragment = 'LockFile /var/lock/apache2/accept.lock'
    tokens = [
            (Token.Name.Builtin, 'LockFile'),
            (Token.Text, ' '),
            (Token.String.Other, '/var/lock/apache2/accept.lock'),
            (Token.Text, ''),
            (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_normal_scoped_directive(lexer):
    fragment = '<VirtualHost "test">\n</VirtualHost>'
    tokens = [
            (Token.Name.Tag, '<VirtualHost'),
            (Token.Text, ' '),
            (Token.Literal.String, '"test"'),
            (Token.Name.Tag, '>'),
            (Token.Text, '\n'),
            (Token.Name.Tag, '</VirtualHost'),
            (Token.Name.Tag, '>'),
            (Token.Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_malformed_scoped_directive_closing_tag(lexer):
    fragment = '<VirtualHost "test">\n</VirtualHost\n>'
    tokens = [
            (Token.Name.Tag, '<VirtualHost'),
            (Token.Text, ' '),
            (Token.Literal.String, '"test"'),
            (Token.Name.Tag, '>'),
            (Token.Text, '\n'),
            (Token.Error, '<'),
            (Token.Error, '/'),
            (Token.Name.Builtin, 'VirtualHost'),
            (Token.Text, ''),
            (Token.Text, '\n'),
            (Token.Error, '>'),
            (Token.Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

