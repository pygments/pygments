"""
    Basic CrystalLexer Test
    ~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Text, String, Number, Punctuation, Error, Whitespace
from pygments.lexers import CrystalLexer


@pytest.fixture(scope='module')
def lexer():
    yield CrystalLexer()


def test_numbers(lexer):
    for kind, testset in [
        (Number.Integer, '0  1  1_000_000  1u8  11231231231121312i64'),
        (Number.Float, '0.0  1.0_f32  1_f32  0f64  1e+4  1e111  1_234.567_890'),
        (Number.Bin, '0b1001_0110  0b0u8'),
        (Number.Oct, '0o17  0o7_i32'),
        (Number.Hex, '0xdeadBEEF'),
    ]:
        for fragment in testset.split():
            assert list(lexer.get_tokens(fragment + '\n')) == \
                [(kind, fragment), (Whitespace, '\n')]

    for fragment in '01  0b2  0x129g2  0o12358'.split():
        assert next(lexer.get_tokens(fragment + '\n'))[0] == Error


def test_symbols(lexer):
    for fragment in [':sym_bol', ':\u3042', ':question?']:
        assert list(lexer.get_tokens(fragment + '\n')) == \
            [(String.Symbol, fragment), (Whitespace, '\n')]

    fragment = ':"sym bol"\n'
    tokens = [
        (String.Symbol, ':"'),
        (String.Symbol, 'sym bol'),
        (String.Symbol, '"'),
        (Whitespace, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_chars(lexer):
    for fragment in ["'a'", "'я'", "'\\u{1234}'", "'\n'"]:
        assert list(lexer.get_tokens(fragment + '\n')) == \
            [(String.Char, fragment), (Whitespace, '\n')]
    assert next(lexer.get_tokens("'abc'"))[0] == Error


def test_string_escapes(lexer):
    for body in ['\\n', '\\a', '\\xff', '\\u1234', '\\000', '\\u{0}', '\\u{10AfF9}']:
        fragment = '"a' + body + 'z"\n'
        assert list(lexer.get_tokens(fragment)) == [
            (String.Double, '"'),
            (String.Double, 'a'),
            (String.Escape, body),
            (String.Double, 'z'),
            (String.Double, '"'),
            (Whitespace, '\n'),
        ]


def test_empty_percent_strings(lexer):
    for body in ['%()', '%[]', '%{}', '%<>', '%||']:
        fragment = '(' + body + ')\n'
        assert list(lexer.get_tokens(fragment)) == [
            (Punctuation, '('),
            (String.Other, body[:-1]),
            (String.Other, body[-1]),
            (Punctuation, ')'),
            (Whitespace, '\n'),
        ]
