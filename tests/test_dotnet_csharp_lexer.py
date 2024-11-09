"""
Includes `pytest` tests for :class:`pygments.lexers.dotnet.CSharpLexer`.
"""
import pytest

from pygments import lex
from pygments.lexers.dotnet import CSharpLexer
from pygments.token import Token

KEYWORD_FILE_TESTS = [
    ("file record X(int file);", False,
        [Token.Keyword, Token.Name]),
    ('int file(Func<int> file) => file();', False,
        [Token.Name.Function, Token.Name, Token.Name]),
    ('int file = Program.file(file => 42);', False,
        [Token.Name, Token.Name, Token.Name]),
    ('await file()', False,
        [Token.Name]),
    ('await someObject.file()', False,
        [Token.Name]),
]


@pytest.mark.parametrize("code, passing, target", KEYWORD_FILE_TESTS)
def test_contextual_keyword_file(code, passing, target):
    """Checks 'file' assigned token types matching *expected* in *code*.

    :param code: the code to be tokenized.
    :param passing: ``true`` iff lexer's assignments are expected to match
        *target*
    :param target: a reasonable assignment based on intuition and examples
        involving identifiers that are not contextual keywords.
    """
    lexer = CSharpLexer(unicodelevel='none')
    lexer_tokens = [
        token for token, text in lex(code, lexer) if text == 'file'
    ]
    if passing:
        assert target == lexer_tokens
    elif target != lexer_tokens:
        pytest.xfail(f"Unfixed; {lexer_tokens!r} != {target!r}")
