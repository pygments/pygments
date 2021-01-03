# -*- coding: utf-8 -*-
"""
    Basic JavaLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import time

import pytest

from pygments.token import Keyword, Name, Number, Punctuation, String, Text
from pygments.lexers import JavaLexer


@pytest.fixture(scope='module')
def lexer():
    yield JavaLexer()


def test_enhanced_for(lexer):
    fragment = 'label:\nfor(String var2: var1) {}\n'
    tokens = [
        (Name.Label, 'label:'),
        (Text, '\n'),
        (Keyword, 'for'),
        (Punctuation, '('),
        (Name, 'String'),
        (Text, ' '),
        (Name, 'var2'),
        (Punctuation, ':'),
        (Text, ' '),
        (Name, 'var1'),
        (Punctuation, ')'),
        (Text, ' '),
        (Punctuation, '{'),
        (Punctuation, '}'),
        (Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_numeric_literals(lexer):
    fragment = '0 5L 9__542_72l 0xbEEf 0X9_A 0_35 01 0b0___101_0'
    fragment += ' 0. .7_17F 3e-1_3d 1f 6_01.9e+3 0x.1Fp3 0XEP8D\n'
    tokens = [
        (Number.Integer, '0'),
        (Text, ' '),
        (Number.Integer, '5L'),
        (Text, ' '),
        (Number.Integer, '9__542_72l'),
        (Text, ' '),
        (Number.Hex, '0xbEEf'),
        (Text, ' '),
        (Number.Hex, '0X9_A'),
        (Text, ' '),
        (Number.Oct, '0_35'),
        (Text, ' '),
        (Number.Oct, '01'),
        (Text, ' '),
        (Number.Bin, '0b0___101_0'),
        (Text, ' '),
        (Number.Float, '0.'),
        (Text, ' '),
        (Number.Float, '.7_17F'),
        (Text, ' '),
        (Number.Float, '3e-1_3d'),
        (Text, ' '),
        (Number.Float, '1f'),
        (Text, ' '),
        (Number.Float, '6_01.9e+3'),
        (Text, ' '),
        (Number.Float, '0x.1Fp3'),
        (Text, ' '),
        (Number.Float, '0XEP8D'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


@pytest.mark.parametrize(
    'text',
    (
        '""', '"abc"', '"ひらがな"', '"123"',
        '"\\\\"', '"\\t"' '"\\""',
    ),
)
def test_string_literals_positive_match(lexer, text):
    """Test positive matches for string literals."""
    tokens = list(lexer.get_tokens_unprocessed(text))
    assert all([token is String for _, token, _ in tokens])
    assert ''.join([value for _, _, value in tokens]) == text


def test_string_literals_backtracking(lexer):
    """Test catastrophic backtracking for string literals."""
    start_time = time.time()
    list(lexer.get_tokens_unprocessed('"' + '\\' * 100))
    assert time.time() - start_time < 1, 'possible backtracking bug'
