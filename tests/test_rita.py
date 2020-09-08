import pytest

from pygments.lexers import RitaLexer
from pygments.token import Keyword, Literal, Punctuation, Text, Operator


@pytest.fixture(scope='session')
def lexer():
    yield RitaLexer()


def test_2_macros(lexer):
    text = '{WORD("Hello"), WORD("world")}->MARK("GREETING")'
    expected = [
        (Punctuation, '{'),
        (Keyword, 'WORD'),
        (Punctuation, '('),
        (Literal, '"Hello"'),
        (Punctuation, ')'),
        (Punctuation, ','),
        (Text, ' '),
        (Keyword, 'WORD'),
        (Punctuation, '('),
        (Literal, '"world"'),
        (Punctuation, ')'),
        (Punctuation, '}'),
        (Operator, '->'),
        (Keyword, 'MARK'),
        (Punctuation, '('),
        (Literal, '"GREETING"'),
        (Punctuation, ')'),
        (Text, '\n')

    ]
    assert expected == list(lexer.get_tokens(text))

