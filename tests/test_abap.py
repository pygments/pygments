"""ABAP keyword regressions."""

import pytest

from pygments.lexers.business import ABAPLexer
from pygments.token import Comment, Keyword, String


@pytest.mark.parametrize('source,keyword', [
    ('CONDENSE word NO-GAPS.', 'NO-GAPS'),
    ('condense word no-gaps.', 'no-gaps'),
    ('FINAL(value) = 1.', 'FINAL'),
    ('CLASS zcl_test DEFINITION PUBLIC FINAL CREATE PUBLIC.', 'FINAL'),
    ('class zcl_test definition public final create public.', 'final'),
])
def test_missing_keywords(source, keyword):
    assert (Keyword, keyword) in list(ABAPLexer().get_tokens(source))


@pytest.mark.parametrize('identifier', ['final_value', 'no-gaps_extra'])
def test_keyword_prefix_remains_identifier(identifier):
    tokens = list(ABAPLexer().get_tokens(identifier + '.'))
    assert not any(kind is Keyword and value.lower() in ('final', 'no-gaps')
                   for kind, value in tokens)


def test_keywords_inside_strings_and_comments():
    tokens = list(ABAPLexer().get_tokens("text = 'FINAL NO-GAPS'. \" FINAL NO-GAPS"))
    assert (String.Single, "'FINAL NO-GAPS'") in tokens
    assert (Comment.Single, '\" FINAL NO-GAPS\n') in tokens
