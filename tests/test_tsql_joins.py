"""
    Tests for Transact-SQL LEFT / RIGHT JOIN keyword categorization (issue #1540).
"""

import pytest

from pygments.lexers.sql import TransactSqlLexer
from pygments.token import Comment, Keyword, Literal, Name


@pytest.fixture(scope="module")
def lexer():
    return TransactSqlLexer()


def test_tsql_reported_query_left_join(lexer):
    sql = (
        "SELECT LEFT(N'AString', 5) FROM dbo.SomeTable AS A "
        "LEFT JOIN dbo.AnotherTable AS B ON A.ID = B.ID\n"
    )
    tokens = list(lexer.get_tokens(sql))

    # Exact source reconstruction
    assert "".join(val for _, val in tokens) == sql

    # First LEFT is a function call
    # Second LEFT is a Keyword before JOIN
    left_tokens = [(tok, val) for tok, val in tokens if val == "LEFT"]
    assert len(left_tokens) == 2
    assert left_tokens[0] == (Name.Function, "LEFT")
    assert left_tokens[1] == (Keyword, "LEFT")

    # JOIN is Keyword
    assert (Keyword, "JOIN") in tokens


def test_tsql_right_outer_join(lexer):
    sql = "SELECT RIGHT('abc', 1) FROM a RIGHT OUTER JOIN b ON a.id=b.id\n"
    tokens = list(lexer.get_tokens(sql))

    assert "".join(val for _, val in tokens) == sql

    right_tokens = [(tok, val) for tok, val in tokens if val == "RIGHT"]
    assert len(right_tokens) == 2
    assert right_tokens[0] == (Name.Function, "RIGHT")
    assert right_tokens[1] == (Keyword, "RIGHT")

    assert (Keyword, "OUTER") in tokens
    assert (Keyword, "JOIN") in tokens


def test_tsql_mixed_case_and_spacing_joins(lexer):
    sql = "SELECT * FROM a lEfT\tOuTeR\nJoIn b ON a.id=b.id RIGHT\nJOIN c ON b.id=c.id\n"
    tokens = list(lexer.get_tokens(sql))

    assert "".join(val for _, val in tokens) == sql

    # lEfT and RIGHT in join positions are Keyword
    join_keywords = [
        (tok, val) for tok, val in tokens
        if val in ("lEfT", "OuTeR", "JoIn", "RIGHT", "JOIN")
    ]
    assert join_keywords == [
        (Keyword, "lEfT"), (Keyword, "OuTeR"), (Keyword, "JoIn"),
        (Keyword, "RIGHT"), (Keyword, "JOIN"),
    ]


def test_tsql_function_calls_with_spacing_and_comments(lexer):
    sql = "SELECT LEFT ('word', 2), RIGHT\n('word', 2), LEFT/*comment*/('word', 2);\n"
    tokens = list(lexer.get_tokens(sql))

    assert "".join(val for _, val in tokens) == sql

    call_tokens = [tok for tok, val in tokens if val in ("LEFT", "RIGHT")]
    assert len(call_tokens) == 3
    for tok in call_tokens:
        assert tok == Name.Function


def test_tsql_identifiers_and_bracketed_names(lexer):
    sql = "SELECT [LEFT], [RIGHT], left_join, right_side, LEFTovers, RIGHTmost FROM t\n"
    tokens = list(lexer.get_tokens(sql))

    assert "".join(val for _, val in tokens) == sql

    bracket_names = [val for tok, val in tokens if tok == Name and val in ("LEFT", "RIGHT")]
    assert bracket_names == ["LEFT", "RIGHT"]

    ident_names = [
        val for tok, val in tokens
        if tok == Name and val in ("left_join", "right_side", "LEFTovers", "RIGHTmost")
    ]
    assert len(ident_names) == 4


def test_tsql_comments_and_strings_unchanged(lexer):
    sql = (
        "-- LEFT JOIN and RIGHT OUTER JOIN\n"
        "SELECT 'LEFT JOIN', \"RIGHT JOIN\" /* LEFT JOIN */\n"
    )
    tokens = list(lexer.get_tokens(sql))

    assert "".join(val for _, val in tokens) == sql

    # Single comment
    assert (Comment.Single, "-- LEFT JOIN and RIGHT OUTER JOIN\n") in tokens
    # Strings
    assert (Literal.String.Single, "'LEFT JOIN'") in tokens
    assert (Literal.String.Symbol, '"RIGHT JOIN"') in tokens
    # Multiline comment
    comment_text = [val for tok, val in tokens if tok == Comment.Multiline]
    assert " LEFT JOIN " in "".join(comment_text)


def test_tsql_join_word_boundaries(lexer):
    # JOINED and JOINER are not JOIN, so LEFT / RIGHT before them must not be treated as join keywords
    sql = "SELECT LEFT JOINED, RIGHT OUTER JOINER FROM t\n"
    tokens = list(lexer.get_tokens(sql))

    assert "".join(val for _, val in tokens) == sql

    left_tok = [tok for tok, val in tokens if val == "LEFT"][0]
    right_tok = [tok for tok, val in tokens if val == "RIGHT"][0]

    assert left_tok == Name.Function
    assert right_tok == Name.Function

    joined_tok = [tok for tok, val in tokens if val == "JOINED"][0]
    joiner_tok = [tok for tok, val in tokens if val == "JOINER"][0]
    assert joined_tok == Name
    assert joiner_tok == Name
