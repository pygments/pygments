import pytest
from pygments.lexers.func import FuncLexer
from pygments.token import Token, Name

@pytest.fixture(scope='module')
def lexer_func():
    yield FuncLexer()


@pytest.mark.parametrize('text', (
    'take(first)Entry', '"not_a_string', 'msg.sender', 'send_message,then_terminate', '_'))
def test_func_not_identifier(lexer_func, text):
    """Test text that should **not** be tokenized as identifier."""
    assert list(lexer_func.get_tokens(text))[0] != (Name.Variable, text)


@pytest.mark.parametrize('text', (
    '`test identifier`', 'simple_identifier', 'query\'\'', 
    '_internal_value', 'get_pubkeys&signatures',
    'dict::udict_set_builder', '2+2=2*2', '-alsovalidname', '{hehehe}'))
def test_func_identifier(lexer_func, text):
    """Test text that should be tokenized as identifier."""
    assert list(lexer_func.get_tokens(text))[0] == (Name.Variable, text)


@pytest.mark.parametrize('text', (
'`test identifier`(', 'simple_identifier(', 'query\'\'(', 
'_internal_value(', 'get_pubkeys&signatures(',
'dict::udict_set_builder(', '2+2=2*2(', '-alsovalidname(', '{hehehe}('))
def test_func_function(lexer_func, text):
    """Test text that should be tokenized as identifier."""
    assert list(lexer_func.get_tokens(text))[0] == (Name.Function, text[:-1])


@pytest.mark.parametrize('text', ('0x0f', '0x1_2', '123', '0b10', '0xffff_fff', '1'))
def test_func_number(lexer_func, text):
    """Test text that should be tokenized as number."""
    assert list(lexer_func.get_tokens(text))[0] == (Token.Literal.Number, text)


@pytest.mark.parametrize('text', ('0x0f_m', '0X1_2', '12d3', '0b1_0f', '0bff_fff', '0b'))
def test_func_not_number(lexer_func, text):
    """Test text that should *not* be tokenized as number."""
    assert list(lexer_func.get_tokens(text))[0] != (Token.Literal.Number, text)
