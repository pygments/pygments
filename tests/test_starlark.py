import pytest
from typing import List, Tuple, Any
from pygments.token import Token, Name, Punctuation, String, Keyword
from pygments.lexers.python import StarlarkLexer

@pytest.fixture
def lexer() -> StarlarkLexer:
    return StarlarkLexer()

def get_tokens(lexer: StarlarkLexer, text: str) -> List[Tuple[Any, str]]:
    return [(tokentype, value) for tokentype, value in lexer.get_tokens(text) if tokentype != Token.Text.Whitespace]

def test_starlark_builtins(lexer: StarlarkLexer) -> None:
    text: str = "rule(name = 'my_target')"
    tokens: List[Tuple[Any, str]] = get_tokens(lexer, text)
    
    assert tokens[0] == (Name.Builtin, "rule")
    assert tokens[1] == (Punctuation, "(")
    assert tokens[2] == (Name, "name")
    assert tokens[3] == (Token.Operator, "=")
    assert tokens[4] == (String.Single, "'my_target'")
    assert tokens[5] == (Punctuation, ")")

def test_starlark_python_inheritance(lexer: StarlarkLexer) -> None:
    text: str = "def my_macro(ctx): pass"
    tokens: List[Tuple[Any, str]] = get_tokens(lexer, text)
    
    assert tokens[0] == (Keyword, "def")
    assert tokens[1] == (Name.Function, "my_macro")
    assert tokens[2] == (Punctuation, "(")
    assert tokens[3] == (Name, "ctx")
    assert tokens[4] == (Punctuation, ")")
    assert tokens[5] == (Punctuation, ":")
    assert tokens[6] == (Keyword, "pass")

def test_starlark_complex_builtins(lexer: StarlarkLexer) -> None:
    text: str = "select({'//conditions:default': []})"
    tokens: List[Tuple[Any, str]] = get_tokens(lexer, text)
    
    assert tokens[0] == (Name.Builtin, "select")
    assert tokens[2] == (Punctuation, "{")
    assert tokens[3] == (String.Single, "'//conditions:default'")
