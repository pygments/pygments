import pytest
from typing import List, Tuple, Any
from pygments.token import Token, Name, Punctuation, Keyword, Operator
from pygments.lexers.python import CoconutLexer

@pytest.fixture
def lexer() -> CoconutLexer:
    return CoconutLexer()

def get_tokens(lexer: CoconutLexer, text: str) -> List[Tuple[Any, str]]:
    return [(tokentype, value) for tokentype, value in lexer.get_tokens(text) if tokentype != Token.Text.Whitespace]

def test_coconut_keywords(lexer: CoconutLexer) -> None:
    text: str = "match data Vector2(x, y):"
    tokens: List[Tuple[Any, str]] = get_tokens(lexer, text)
    
    assert tokens[0] == (Keyword, "match")
    assert tokens[1] == (Keyword, "data")
    assert tokens[2] == (Name.Function, "Vector2")

def test_coconut_operator_keyword(lexer: CoconutLexer) -> None:
    text: str = "operator + (a, b) = a + b"
    tokens: List[Tuple[Any, str]] = get_tokens(lexer, text)
    
    assert tokens[0] == (Keyword, "operator")
    assert tokens[1] == (Operator, "+")
    assert tokens[2] == (Punctuation, "(")
    assert tokens[3] == (Name, "a")
    assert tokens[4] == (Punctuation, ",")
    assert tokens[5] == (Name, "b")
    assert tokens[6] == (Punctuation, ")")
    assert tokens[7] == (Operator, "=")

def test_coconut_python_fallback(lexer: CoconutLexer) -> None:
    text: str = "class MyClass(object):"
    tokens: List[Tuple[Any, str]] = get_tokens(lexer, text)
    
    assert tokens[0] == (Keyword, "class")
    assert tokens[1] == (Name.Class, "MyClass")
    assert tokens[2] == (Punctuation, "(")
    assert tokens[3] == (Name.Builtin, "object")
    assert tokens[4] == (Punctuation, ")")
