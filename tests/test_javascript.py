# -*- coding: utf-8 -*-
"""
    Javascript tests
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest

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

def test_coffee_slashes():
    for input_str, slashes_are_regex_here in COFFEE_SLASH_GOLDEN:
        yield coffee_runner, input_str, slashes_are_regex_here

def coffee_runner(input_str, slashes_are_regex_here):
    lex = CoffeeScriptLexer()
    output = list(lex.get_tokens(input_str))
    print(output)
    for t, s in output:
        if '/' in s:
            is_regex = t is Token.String.Regex
            assert is_regex == slashes_are_regex_here, (t, s)

class CoffeeTest(unittest.TestCase):
    def setUp(self):
        self.lexer = CoffeeScriptLexer()

    def testMixedSlashes(self):
        fragment = u'a?/foo/:1/2;\n'
        tokens = [
            (Token.Name.Other, u'a'),
            (Token.Operator, u'?'),
            (Token.Literal.String.Regex, u'/foo/'),
            (Token.Operator, u':'),
            (Token.Literal.Number.Integer, u'1'),
            (Token.Operator, u'/'),
            (Token.Literal.Number.Integer, u'2'),
            (Token.Punctuation, u';'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testBewareInfiniteLoop(self):
        # This demonstrates the case that "This isn't really guarding" comment
        # refers to.
        fragment = '/a/x;\n'
        tokens = [
            (Token.Text, ''),
            (Token.Operator, '/'),
            (Token.Name.Other, 'a'),
            (Token.Operator, '/'),
            (Token.Name.Other, 'x'),
            (Token.Punctuation, ';'),
            (Token.Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
