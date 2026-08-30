import pytest
from typing import List, Tuple, Any
from pygments.token import Token, Name, Generic, Operator
from pygments.lexers.python import IPythonConsoleLexer

@pytest.fixture
def lexer() -> IPythonConsoleLexer:
    return IPythonConsoleLexer()

def get_tokens(lexer: IPythonConsoleLexer, text: str) -> List[Tuple[Any, str]]:
    return [(tokentype, value) for tokentype, value in lexer.get_tokens(text) if tokentype != Token.Text.Whitespace]

def test_ipython_prompts(lexer: IPythonConsoleLexer) -> None:
    text: str = "In [1]: x = 5\nOut[1]: 5"
    tokens: List[Tuple[Any, str]] = get_tokens(lexer, text)
    
    assert tokens[0] == (Generic.Prompt, "In [")
    assert tokens[1] == (Generic.Prompt.LineNo, "1")
    assert tokens[2] == (Generic.Prompt, "]: ")
    assert tokens[3] == (Name, "x")
    assert tokens[4] == (Operator, "=")
    
    out_idx: int = next(i for i, t in enumerate(tokens) if t[0] == Generic.Output and t[1] == "Out[")
    assert tokens[out_idx] == (Generic.Output, "Out[")
    assert tokens[out_idx + 1] == (Generic.Output, "1")
    assert tokens[out_idx + 2] == (Generic.Output, "]: ")

def test_ipython_line_magic(lexer: IPythonConsoleLexer) -> None:
    text: str = "%timeit list(range(1000))"
    tokens: List[Tuple[Any, str]] = get_tokens(lexer, text)
    
    assert tokens[0] == (Name.Decorator, "%timeit")
    assert tokens[1] == (Name.Builtin, "list")
    assert tokens[2] == (Token.Punctuation, "(")
    assert tokens[3] == (Name.Builtin, "range")

def test_ipython_cell_magic(lexer: IPythonConsoleLexer) -> None:
    text: str = "%%bash\necho 'hello'"
    tokens: List[Tuple[Any, str]] = [(t, v) for t, v in lexer.get_tokens(text) if t != Token.Text.Whitespace]
    
    assert tokens[0] == (Name.Decorator, "%%bash")
    assert tokens[1] == (Name, "echo") 

def test_ipython_shell_command(lexer: IPythonConsoleLexer) -> None:
    text: str = "!ls -la"
    tokens: List[Tuple[Any, str]] = get_tokens(lexer, text)
    
    assert tokens[0] == (Operator, "!")
    assert tokens[1] == (Name.Builtin, "ls")

def test_ipython_help_introspection(lexer: IPythonConsoleLexer) -> None:
    text_postfix: str = "my_var??"
    tokens_postfix: List[Tuple[Any, str]] = get_tokens(lexer, text_postfix)
    
    assert tokens_postfix[0] == (Name, "my_var")
    assert tokens_postfix[1] == (Operator, "??")

    text_prefix: str = "?my_var"
    tokens_prefix: List[Tuple[Any, str]] = get_tokens(lexer, text_prefix)
    
    assert tokens_prefix[0] == (Operator, "?")
    assert tokens_prefix[1] == (Name, "my_var")
