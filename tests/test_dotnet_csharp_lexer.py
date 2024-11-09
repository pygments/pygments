"""
Includes `pytest` tests for :class:`pygments.lexers.dotnet.CSharpLexer`.
"""
import pytest

from pygments import lex
from pygments.lexers.dotnet import CSharpLexer
from pygments.token import Token

FULL_SAMPLE_1 = """
    file record X(int file);
    file record Y(int blah);
    
    public static class Program {
        static int file(Func<int,int> file) => file(13);
        static int blah(Func<int,int> blah) => blah(13);
    
        public static void Main(string[] args) {
            int file = Program.file(file => 42);
            int blah = Program.blah(blah => 12);
            X x = new(9);
            Console.WriteLine($"{file}{blah}{x}{args}");
        }
    }
"""

KEYWORD_FILE_TESTS = [
    ("file record X(int file);", False, [
        # keyword: before 'record'
        Token.Keyword,
        # (record parameter) name: between (type) name and ')'
        Token.Name]),
    ('int file(Func<int> file) => file();', False, [
        # function name: between (type) name 'int' and '('
        Token.Name.Function,
        # (parameter) name: between '>' and ')'
        Token.Name,
        # name (in method part of call): between '=>' and '('
        Token.Name]),
    ('int file = Program.file(file => 42);', False, [
        # name (variable declaration): between (type) name and '='
        Token.Name,
        # name (in method part of call): between '.' and '('
        Token.Name,
        # name (untyped parameter in lambda): between '(' and '=>'
        Token.Name]),
    ('await file()', False, [
        # name (in method part of call): between 'await' and '('
        # note: more general issue with await whatever()
        Token.Name]),
    ('await someObject.file()', False, [
        # name (in method part of call): between '.' and '('
        Token.Name]),
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
