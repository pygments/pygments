"""
    Tests for the Gherkin lexer.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.testing import GherkinLexer
from pygments.token import Comment, Name, String


@pytest.mark.parametrize('header', [
    'Feature: Tagged feature',
    'Scenario: Empty scenario',
    'Feature: Tagged feature\n  Scenario: Empty scenario',
])
def test_tags_before_first_step(header):
    source = f'{header}\n  # keep the comment\n  @first @second\n  Scenario: Tagged\n'
    tokens = list(GherkinLexer().get_tokens(source))
    assert [value for token, value in tokens if token is Name.Tag] == [
        '@first', '@second',
    ]
    assert any(token is Comment and '# keep the comment' in value
               for token, value in tokens)


def test_tags_inside_quoted_step_text_are_not_tags():
    tokens = list(GherkinLexer().get_tokens(
        'Feature: Tagged feature\n  @scenario\n  Scenario: Quoted text\n'
        '    Given "@literal"\n'
    ))
    assert [value for token, value in tokens if token is Name.Tag] == ['@scenario']
    assert '@literal' in ''.join(value for token, value in tokens if token in String)
