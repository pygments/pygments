"""
    Tests for the SAS lexer.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.sas import SASLexer
from pygments.token import Keyword


@pytest.mark.parametrize('prefix', ['', '\n', '  ', 'x=1;'])
@pytest.mark.parametrize(('statement', 'keyword'), [
    ('data demo;', 'data'),
    ('run;', 'run'),
    ('quit;', 'quit'),
    ('proc print;', 'proc print'),
    ('PROC\tPRINT;', 'PROC\tPRINT'),
])
def test_step_keywords_at_statement_boundaries(prefix, statement, keyword):
    source = prefix + statement
    tokens = list(SASLexer().get_tokens_unprocessed(source))
    assert [(token, value) for _, token, value in tokens
            if token is Keyword.Reserved] == [(Keyword.Reserved, keyword)]
    assert ''.join(value for _, _, value in tokens) == source


@pytest.mark.parametrize('source', [
    'metadata;', 'runner;', 'quitter;', 'procedure;',
    '"data;"', "'run;'", '/* quit; */', '* proc print;',
])
def test_step_keyword_boundaries_and_noncode(source):
    tokens = list(SASLexer().get_tokens_unprocessed(source))
    assert all(token is not Keyword.Reserved for _, token, _ in tokens)
    assert ''.join(value for _, _, value in tokens) == source
