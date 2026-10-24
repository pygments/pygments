import pytest

from pygments.lexers.int_fiction import Tads3Lexer
from pygments.token import Comment, Keyword, Name, Text


@pytest.mark.parametrize('backslashes', range(1, 5))
def test_single_line_comment_continues_after_backslash_before_newline(backslashes):
    backslashes = '\\' * backslashes
    source = f'// comment{backslashes}\ncontinued\nnext\n'

    tokens = [(token, value) for token, value in Tads3Lexer().get_tokens(source)
              if value]

    assert tokens == [
        (Comment.Single, f'// comment{backslashes}\ncontinued'),
        (Text, '\n'),
        (Name.Class, 'next'),
        (Text, '\n'),
    ]


@pytest.mark.parametrize(('suffix', 'comment'), [
    ('\\ x', '// comment\\ x'),
    ('\\\\ x', '// comment\\\\ x'),
    ('\\x', '// comment\\x'),
    ('\\  ', '// comment\\  '),
])
def test_single_line_comment_does_not_continue_when_backslash_is_not_last(
        suffix, comment):
    source = f'// comment{suffix}\nnext\n'

    tokens = [(token, value) for token, value in Tads3Lexer().get_tokens(source)
              if value]

    assert tokens == [
        (Comment.Single, comment),
        (Text, '\n'),
        (Name.Class, 'next'),
        (Text, '\n'),
    ]


@pytest.mark.parametrize('backslash_count', range(1, 5))
def test_single_line_comment_continuation_works_in_whitespace_rule(backslash_count):
    backslashes = '\\' * backslash_count
    source = f'extern // comment{backslashes}\nobject Foo;\n'

    tokens = [(token, value) for token, value in Tads3Lexer().get_tokens(source)
              if value]

    assert tokens[:4] == [
        (Keyword.Reserved, 'extern'),
        (Text, ' '),
        (Comment.Single, f'// comment{backslashes}\nobject Foo;'),
        (Text, '\n'),
    ]


def test_single_line_comment_continuation_works_in_preprocessor_whitespace():
    source = '#if 1 // comment\\\\\ncontinued\n#endif\n'

    tokens = [(token, value) for token, value in Tads3Lexer().get_tokens(source)
              if value]

    assert tokens == [
        (Comment.Preproc, '#if 1 // comment\\\\\ncontinued\n'),
        (Comment.Preproc, '#endif\n'),
    ]


def test_truncated_single_line_comment_keeps_trailing_backslashes():
    source = '// truncated ' + '\\' * 4

    tokens = list(Tads3Lexer(stripnl=False, ensurenl=False).get_tokens(source))

    assert tokens == [(Comment.Single, source)]


def test_single_line_comment_continuation_stops_before_a_blank_line():
    source = '// comment\\\\\n\nnext\n'

    tokens = [(token, value) for token, value in Tads3Lexer().get_tokens(source)
              if value]

    assert tokens == [
        (Comment.Single, '// comment\\\\\n'),
        (Text, '\n'),
        (Name.Class, 'next'),
        (Text, '\n'),
    ]
