"""
    Tests for Gherkin tables with compact cells.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.testing import GherkinLexer
from pygments.token import Keyword, Name, String


@pytest.mark.parametrize('row', ['|a|b|', '|a| b |', r'|a|b\|c|', '|a||'])
@pytest.mark.parametrize('following', [
    '    Then continue',
    '  Scenario: Next\n    When continue',
])
def test_keyword_after_data_table(row, following):
    source = ('Feature: Tables\n  Scenario: First\n    Given a table\n'
              f'      {row}\n\n{following}\n')
    tokens = list(GherkinLexer().get_tokens(source))
    keyword_text = ''.join(value for token, value in tokens if token is Keyword)
    assert ('Then' if 'Then' in following else 'Scenario') in keyword_text
    assert 'continue' not in ''.join(value for token, value in tokens if token in String)


@pytest.mark.parametrize('header', ['|value|', '| value |', r'|a\|b|'])
def test_examples_header_does_not_consume_data_or_next_scenario(header):
    source = ('Feature: Tables\n  Scenario Outline: First\n    Given <value>\n'
              f'    Examples:\n      {header}\n      |one|\n\n'
              '  Scenario: Next\n    Then continue\n')
    tokens = list(GherkinLexer().get_tokens(source))
    assert 'one' in ''.join(value for token, value in tokens if token in String)
    assert 'one' not in ''.join(value for token, value in tokens if token is Name.Variable)
    assert 'Scenario' in ''.join(value for token, value in tokens if token is Keyword)
    assert 'continue' not in ''.join(value for token, value in tokens if token in String)
