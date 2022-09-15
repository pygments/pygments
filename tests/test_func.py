import pytest
from pygments.lexers.func import FuncLexer
from pygments.token import Token, Name

@pytest.fixture(scope='module')
def lexer_func():
    yield FuncLexer()


def test_simple_func_contract(lexer_func):
    """valid, even all contract is parsed ok"""

    fragment = '''
#include "../";
#pragma version >=1.0.0;

global int k;
const int k = 1;

() recv_internal(int my_balance, int msg_value, cell in_msg_full, slice in_msg_body) impure {
    slice cs = in_msg_full.begin_parse();
    int flags = cs~load_uint(0x4_1_0);

    if ((flags & 1) == true) { ;; ignore all bounced messages
        return ();
    }

    {-
    {-
     Test comment
    -}

    slice sender_address = cs~load_msg_addr();

    ;; Send message
    var message = begin_cell()
        .store_uint(0x18, 6)
        .store_slice(sender_address)
        .store_coins(0)
        .store_uint(0, 1 + 4 + 4 + 64 + 32 + 1 + 1)
        .store_slice("Hello, world!"s)
        .end_cell();

    send_raw_message(message, 64);

    ;; Update counter
    var cs = get_data().begin_parse();
    var counter = data~load_uint(32);

    store_data(
        begin_cell()
            .store_uint(counter + 1, 32)
        .end_cell()
    );
}

() recv_external(slice in_msg) impure {
    throw(0xffff);
}

int counter() method_id {
    var data = get_data().begin_parse();
    return data~load_uint(32);
}
    '''
    
    tokens = list(lexer_func.get_tokens(fragment))
    assert all(x[1] != Token.Error for x in tokens)


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
