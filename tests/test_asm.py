# -*- coding: utf-8 -*-
"""
    Basic GasLexer/NasmLexer Test
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Token
from pygments.lexers import NasmLexer, GasLexer


@pytest.fixture(scope='module')
def lexer_gas():
    yield GasLexer()

@pytest.fixture(scope='module')
def lexer_nasm():
    yield NasmLexer()


def test_comments(lexer_gas):
    fragment = '''
    lock addq $0, /* comments */ (%rsp) /*
    // comments
    */ xorq %rax, %rax // comments
    '''
    tokens = [
        (Token.Text, '    '),
        (Token.Name.Attribute, 'lock'),
        (Token.Text, ' '),
        (Token.Name.Function, 'addq'),
        (Token.Text, ' '),
        (Token.Name.Constant, '$0'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Comment.Multiline, '/* comments */'),
        (Token.Text, ' '),
        (Token.Punctuation, '('),
        (Token.Name.Variable, '%rsp'),
        (Token.Punctuation, ')'),
        (Token.Text, ' '),
        (Token.Comment.Multiline, '/*\n    // comments\n    */'),
        (Token.Text, ' '),
        (Token.Name.Function, 'xorq'),
        (Token.Text, ' '),
        (Token.Name.Variable, '%rax'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Name.Variable, '%rax'),
        (Token.Text, ' '),
        (Token.Comment.Single, '// comments\n'),
        (Token.Text, '    \n')
    ]
    assert list(lexer_gas.get_tokens(fragment)) == tokens

def test_cpuid(lexer_nasm):
    # CPU is a valid directive, and we don't want to parse this as
    # cpu id, but as a single token. See bug #1517
    fragment = 'cpuid'
    expected = [
        (Token.Name.Function, 'cpuid'),
        (Token.Text, '\n'),
    ]
    assert expected == list(lexer_nasm.get_tokens(fragment))
