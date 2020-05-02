# -*- coding: utf-8 -*-
"""
    Basic HDL Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Keyword, Name, Number, Operator, Punctuation, Text
from pygments.lexers import SystemVerilogLexer


@pytest.fixture(scope='module')
def lexer():
    yield SystemVerilogLexer()


# Note there are a few minor issues, listed below
def test_systemverilog_basic(lexer):
    """A basic SystemVerilog test."""

    fragment = """
    module Foo #(
        parameter int N = 42
    ) (
        input  logic [N-1:0] x0,
        input  logic [N-1:0] x1,
        output logic [N-1:0] y,
        input  logic clk
    );
        always_ff @(posedge clk) y <= x0 + x1;
    endmodule : Foo
    """

    tokens = [
        (Text, '    '),
        (Keyword.Declaration, 'module'),
        (Text, ' '),
        (Name.Class, 'Foo'),
        (Text, ' '),
        # This should probably be one token '#('
        (Punctuation, '#'),
        (Punctuation, '('),
        (Text, '\n        '),
        (Keyword.Declaration, 'parameter'),
        (Text, ' '),
        (Keyword.Type, 'int'),
        (Text, ' '),
        (Name, 'N'),
        (Text, ' '),
        (Operator, '='),
        (Text, ' '),
        # This should be Number.Integer
        (Number.Hex, '42'),
        (Text, '\n    '),
        (Punctuation, ')'),
        (Text, ' '),
        (Punctuation, '('),
        (Text, '\n        '),
        (Keyword, 'input'),
        (Text, '  '),
        (Keyword.Type, 'logic'),
        (Text, ' '),
        (Punctuation, '['),
        (Name, 'N'),
        (Operator, '-'),
        (Number.Hex, '1'),
        # Is this really an operator, not punctuation?
        (Operator, ':'),
        (Number.Hex, '0'),
        (Punctuation, ']'),
        (Text, ' '),
        (Name, 'x0'),
        (Punctuation, ','),
        (Text, '\n        '),
        (Keyword, 'input'),
        (Text, '  '),
        (Keyword.Type, 'logic'),
        (Text, ' '),
        (Punctuation, '['),
        (Name, 'N'),
        (Operator, '-'),
        (Number.Hex, '1'),
        (Operator, ':'),
        (Number.Hex, '0'),
        (Punctuation, ']'),
        (Text, ' '),
        (Name, 'x1'),
        (Punctuation, ','),
        (Text, '\n        '),
        (Keyword, 'output'),
        (Text, ' '),
        (Keyword.Type, 'logic'),
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
        (Text, '\n        '),
        (Keyword, 'input'),
        (Text, '  '),
        (Keyword.Type, 'logic'),
        (Text, ' '),
        (Name, 'clk'),
        (Text, '\n    '),
        (Punctuation, ')'),
        (Punctuation, ';'),
        (Text, '\n        '),
        (Keyword, 'always_ff'),
        (Text, ' '),
        (Punctuation, '@'),
        (Punctuation, '('),
        (Keyword, 'posedge'),
        (Text, ' '),
        (Name, 'clk'),
        (Punctuation, ')'),
        (Text, ' '),
        (Name, 'y'),
        (Text, ' '),
        # This should be a single operator: '<='
        (Operator, '<'),
        (Operator, '='),
        (Text, ' '),
        (Name, 'x0'),
        (Text, ' '),
        (Operator, '+'),
        (Text, ' '),
        (Name, 'x1'),
        (Punctuation, ';'),
        (Text, '\n    '),
        (Keyword.Declaration, 'endmodule'),
        (Text, ' '),
        (Punctuation, ':'),
        (Text, ' '),
        (Name.Class, 'Foo'),
        (Text, '\n    \n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
