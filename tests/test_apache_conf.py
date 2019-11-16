# -*- coding: utf-8 -*-
"""
    Basic Apache Configuration Test
    ~~~~~~~~~~~~~~~~~--------------

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
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