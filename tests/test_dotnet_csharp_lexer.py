"""
Includes `pytest` tests for :class:`pygments.lexers.dotnet.CSharpLexer`.
"""
import pytest

from pygments import lex
from pygments.lexers.dotnet import CSharpLexer
from pygments.token import Name, Keyword

KEYWORD_FILE_TESTS = [
    ("file record X(int file);", True,
        [Keyword, Name]),
    ('int file(Func<int> file) => file();', True,
        [Name.Function, Name, Name]),
    ('int file = Program.file(file => 42);', True,
        [Name, Name, Name]),
    ('await file()', False,
        [Name]),
    ('await someObject.file()', True,
        [Name]),
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
