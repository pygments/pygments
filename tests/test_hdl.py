# -*- coding: utf-8 -*-
"""
    HDL Tests
    ~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import SystemVerilogLexer
from pygments.token import (Comment, Keyword, Name, Number, Operator,
                            Punctuation, Text)


@pytest.fixture(scope='module')
def lexer():
    yield SystemVerilogLexer()


SYSTEMVERILOG_BASIC_FRAGMENT = """\
// Adder flops the sum of its inputs
module Adder #(
    parameter int N = 42
) (
    output logic [N-1:0] y,
    output logic         co,

    input  logic [N-1:0] a,
    input  logic [N-1:0] b,
    input  logic         ci,

    input  logic clk
);
    always_ff @(posedge clk) begin
        {co, y} <= a + b + ci;
    end
endmodule : Adder
"""

def test_systemverilog_basic(lexer):
    """A basic SystemVerilog test.

    Examine tokens emitted by the SV lexer for a trivial, but complete code
    fragment. Not intended to stress any particular corner of the language.
    """
    tokens = [
        (Comment.Single, '// Adder flops the sum of its inputs\n'),
        (Keyword, 'module'),
        (Text, ' '),
        (Name, 'Adder'),
        (Text, ' '),
        (Punctuation, '#'),
        (Punctuation, '('),
        (Text, '\n'),

        (Text, '    '),
        (Keyword, 'parameter'),
        (Text, ' '),
        (Keyword, 'int'),
        (Text, ' '),
        (Name, 'N'),
        (Text, ' '),
        (Operator, '='),
        (Text, ' '),
        # Note: This should be Number.Integer
        (Number.Hex, '42'),
        (Text, '\n'),

        (Punctuation, ')'),
        (Text, ' '),
        (Punctuation, '('),
        (Text, '\n'),

        (Text, '    '),
        (Keyword, 'output'),
        (Text, ' '),
        (Keyword, 'logic'),
        (Text, ' '),
        (Punctuation, '['),
        (Name, 'N'),
        (Operator, '-'),
        (Number.Hex, '1'),
        (Operator, ':'),
        (Number.Hex, '0'),
        (Punctuation, ']'),
        (Text, ' '),
        (Name, 'y'),
        (Punctuation, ','),
        (Text, '\n'),

        (Text, '    '),
        (Keyword, 'output'),
        (Text, ' '),
        (Keyword, 'logic'),
        (Text, '         '),
        (Name, 'co'),
        (Punctuation, ','),
        (Text, '\n'),
        (Text, '\n'),

        (Text, '    '),
        (Keyword, 'input'),
        (Text, '  '),
        (Keyword, 'logic'),
        (Text, ' '),
        (Punctuation, '['),
        (Name, 'N'),
        (Operator, '-'),
        (Number.Hex, '1'),
        # Note: This ':' should be Punctuation
        (Operator, ':'),
        (Number.Hex, '0'),
        (Punctuation, ']'),
        (Text, ' '),
        (Name, 'a'),
        (Punctuation, ','),
        (Text, '\n'),

        (Text, '    '),
        (Keyword, 'input'),
        (Text, '  '),
        (Keyword, 'logic'),
        (Text, ' '),
        (Punctuation, '['),
        (Name, 'N'),
        (Operator, '-'),
        (Number.Hex, '1'),
        (Operator, ':'),
        (Number.Hex, '0'),
        (Punctuation, ']'),
        (Text, ' '),
        (Name, 'b'),
        (Punctuation, ','),
        (Text, '\n'),

        (Text, '    '),
        (Keyword, 'input'),
        (Text, '  '),
        (Keyword, 'logic'),
        (Text, '         '),
        (Name, 'ci'),
        (Punctuation, ','),
        (Text, '\n'),
        (Text, '\n'),

        (Text, '    '),
        (Keyword, 'input'),
        (Text, '  '),
        (Keyword, 'logic'),
        (Text, ' '),
        (Name, 'clk'),
        (Text, '\n'),

        (Punctuation, ')'),
        (Punctuation, ';'),
        (Text, '\n'),

        (Text, '    '),
        (Keyword, 'always_ff'),
        (Text, ' '),
        (Punctuation, '@'),
        (Punctuation, '('),
        (Keyword, 'posedge'),
        (Text, ' '),
        (Name, 'clk'),
        (Punctuation, ')'),
        (Text, ' '),
        (Keyword, 'begin'),
        (Text, '\n'),

        (Text, '        '),
        (Punctuation, '{'),
        (Name, 'co'),
        (Punctuation, ','),
        (Text, ' '),
        (Name, 'y'),
        (Punctuation, '}'),
        (Text, ' '),
        (Operator, '<'),
        (Operator, '='),
        (Text, ' '),
        (Name, 'a'),
        (Text, ' '),
        (Operator, '+'),
        (Text, ' '),
        (Name, 'b'),
        (Text, ' '),
        (Operator, '+'),
        (Text, ' '),
        (Name, 'ci'),
        (Punctuation, ';'),
        (Text, '\n'),

        (Text, '    '),
        (Keyword, 'end'),
        (Text, '\n'),

        (Keyword, 'endmodule'),
        (Text, ' '),
        (Operator, ':'),
        (Text, ' '),
        (Name, 'Adder'),
        (Text, '\n'),
    ]
    assert list(lexer.get_tokens(SYSTEMVERILOG_BASIC_FRAGMENT)) == tokens
