"""
    CoffeeScript tests
    ~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import CoffeeScriptLexer
from pygments.token import Token

COFFEE_SLASH_GOLDEN = [
    # input_str, slashes_are_regex_here
    (r'/\\/', True),
    (r'/\\/i', True),
    (r'/\//', True),
    (r'/(\s)/', True),
    ('/a{2,8}/', True),
    ('/b*c?d+/', True),
    ('/(capture-match)/', True),
    ('/(?:do-not-capture-match)/', True),
    ('/this|or|that/', True),
    ('/[char-set]/', True),
    ('/[^neg-char_st]/', True),
    ('/^.*$/', True),
    (r'/\n(\f)\0\1\d\b\cm\u1234/', True),
    (r'/^.?([^/\\\n\w]*)a\1+$/.something(or_other) # something more complex', True),
    ("foo = (str) ->\n  /'|\"/.test str", True),
    ('a = a / b / c', False),
    ('a = a/b/c', False),
    ('a = a/b/ c', False),
    ('a = a /b/c', False),
    ('a = 1 + /d/.test(a)', True),
]


@pytest.fixture(scope='module')
def lexer():
    yield CoffeeScriptLexer()


@pytest.mark.parametrize('golden', COFFEE_SLASH_GOLDEN)
def test_coffee_slashes(lexer, golden):
    input_str, slashes_are_regex_here = golden
    output = list(lexer.get_tokens(input_str))
    print(output)
    for t, s in output:
        if '/' in s:
            is_regex = t is Token.String.Regex
            assert is_regex == slashes_are_regex_here, (t, s)
