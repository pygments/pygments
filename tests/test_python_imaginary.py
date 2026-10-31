import ast

import pytest

from pygments.lexers.python import PythonLexer
from pygments.token import Name, Number


@pytest.mark.parametrize('literal', [
    '1j', '1J', '0j', '01j', '1_000j', '1.5j', '1.5J',
    '.5j', '.5J', '1.j', '1.J', '1e3j', '1e3J', '1.5e-3j',
    '1_2.3_4e+5_6J',
])
def test_imaginary_literal(literal):
    assert isinstance(ast.literal_eval(literal), complex)
    assert list(PythonLexer().get_tokens_unprocessed(literal)) == [
        (0, Number.Float, literal),
    ]


@pytest.mark.parametrize('literal, token', [
    ('123', Number.Integer), ('1.5', Number.Float),
    ('1e3', Number.Float), ('0x1', Number.Hex), ('j', Name), ('J', Name),
])
def test_other_numbers_and_identifiers(literal, token):
    assert list(PythonLexer().get_tokens_unprocessed(literal)) == [
        (0, token, literal),
    ]
