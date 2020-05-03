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
        (Number.Integer, '42'),
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
        (Number.Integer, '1'),
        # Is this really an operator, not punctuation?
        (Operator, ':'),
        (Number.Integer, '0'),
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
        (Number.Integer, '1'),
        (Operator, ':'),
        (Number.Integer, '0'),
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
        (Number.Integer, '1'),
        (Operator, ':'),
        (Number.Integer, '0'),
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


# Believe it or not, SystemVerilog supports spaces before and after the base
# specifier (ie 'b, 'd, 'h). See IEEE 1800-2017 Section 5.7.1 for examples.
SVNUMS = """
8'b10101010
8 'b10101010
8'b 10101010
8'sb10101010
8'Sb10101010
8'B10101010
8'b1010_1010
8'b10xXzZ?10

24'o01234567
24 'o01234567
24'o 01234567
24'so01234567
24'So01234567
24'O01234567
24'o0123_4567
24'o01xXzZ?7

32'd27182818
32 'd27182818
32'd 27182818
32'sd27182818
32'Sd27182818
32'D27182818
32'd2718_2818
32'd27xXzZ?8

32'hdeadbeef
32 'hdeadbeef
32'h deadbeef
32'shdeadbeef
32'Shdeadbeef
32'Hdeadbeef
32'hdead_beef
32'hdexXzZ?f

'0 '1 'x 'X 'z 'Z

42 1234_5678
"""

def test_systemverilog_numbers(lexer):
    """Test most types of numbers"""

    tokens = [
        (Number.Bin, "8'b10101010"),
        (Text, '\n'),
        (Number.Bin, "8 'b10101010"),
        (Text, '\n'),
        (Number.Bin, "8'b 10101010"),
        (Text, '\n'),
        (Number.Bin, "8'sb10101010"),
        (Text, '\n'),
        (Number.Bin, "8'Sb10101010"),
        (Text, '\n'),
        (Number.Bin, "8'B10101010"),
        (Text, '\n'),
        (Number.Bin, "8'b1010_1010"),
        (Text, '\n'),
        (Number.Bin, "8'b10xXzZ?10"),
        (Text, '\n\n'),
        (Number.Oct, "24'o01234567"),
        (Text, '\n'),
        (Number.Oct, "24 'o01234567"),
        (Text, '\n'),
        (Number.Oct, "24'o 01234567"),
        (Text, '\n'),
        (Number.Oct, "24'so01234567"),
        (Text, '\n'),
        (Number.Oct, "24'So01234567"),
        (Text, '\n'),
        (Number.Oct, "24'O01234567"),
        (Text, '\n'),
        (Number.Oct, "24'o0123_4567"),
        (Text, '\n'),
        (Number.Oct, "24'o01xXzZ?7"),
        (Text, '\n\n'),
        (Number.Integer, "32'd27182818"),
        (Text, '\n'),
        (Number.Integer, "32 'd27182818"),
        (Text, '\n'),
        (Number.Integer, "32'd 27182818"),
        (Text, '\n'),
        (Number.Integer, "32'sd27182818"),
        (Text, '\n'),
        (Number.Integer, "32'Sd27182818"),
        (Text, '\n'),
        (Number.Integer, "32'D27182818"),
        (Text, '\n'),
        (Number.Integer, "32'd2718_2818"),
        (Text, '\n'),
        (Number.Integer, "32'd27xXzZ?8"),
        (Text, '\n\n'),
        (Number.Hex, "32'hdeadbeef"),
        (Text, '\n'),
        (Number.Hex, "32 'hdeadbeef"),
        (Text, '\n'),
        (Number.Hex, "32'h deadbeef"),
        (Text, '\n'),
        (Number.Hex, "32'shdeadbeef"),
        (Text, '\n'),
        (Number.Hex, "32'Shdeadbeef"),
        (Text, '\n'),
        (Number.Hex, "32'Hdeadbeef"),
        (Text, '\n'),
        (Number.Hex, "32'hdead_beef"),
        (Text, '\n'),
        (Number.Hex, "32'hdexXzZ?f"),
        (Text, '\n\n'),
        (Number, "'0"),
        (Text, ' '),
        (Number, "'1"),
        (Text, ' '),
        (Number, "'x"),
        (Text, ' '),
        (Number, "'X"),
        (Text, ' '),
        (Number, "'z"),
        (Text, ' '),
        (Number, "'Z"),
        (Text, '\n\n'),
        (Number.Integer, '42'),
        (Text, ' '),
        (Number.Integer, '1234_5678'),
        (Text, '\n'),
    ]
    assert list(lexer.get_tokens(SVNUMS)) == tokens
