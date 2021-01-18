"""
    CPP Tests
    ~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexers import CppLexer, CLexer

from pygments.lexers import guess_lexer


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
