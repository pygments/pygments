# -*- coding: utf-8 -*-
"""
    CPP Tests
    ~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import CppLexer, CLexer
from pygments.token import Token

from pygments.lexers import guess_lexer


@pytest.fixture(scope='module')
def lexer():
    yield CppLexer()


def test_good_comment(lexer):
    fragment = '/* foo */\n'
    tokens = [
        (Token.Comment.Multiline, '/* foo */'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_open_comment(lexer):
    fragment = '/* foo\n'
    tokens = [
        (Token.Comment.Multiline, '/* foo\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_guess_c_lexer():
    code = '''
    #include <stdio.h>
    #include <stdlib.h>

    int main(void);

    int main(void) {
        uint8_t x = 42;
        uint8_t y = x + 1;

        /* exit 1 for success! */
        return 1;
    }
    '''
    lexer = guess_lexer(code)
    assert isinstance(lexer, CLexer)
