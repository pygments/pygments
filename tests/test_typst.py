"""
    Typst tests
    ~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import TypstLexer
from pygments.token import Token


def match_typst_synatax(lexer, match):
    input_str, expected = match
    output = list(lexer.get_tokens(input_str))
    print("{:40} {:40}".format("Output", "Expected"))
    for i in range(max(len(output), len(expected))):
        print("{:40} {:40}".format(str(output[i]), str(expected[i])))
        assert output[i] == expected[i]


@pytest.fixture(scope='module')
def lexer():
    yield TypstLexer()


NESTING = [
    (r'#rect()[]', [
        (Token.Name.Function, '#rect'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Punctuation, '['),
        (Token.Punctuation, ']'),
        (Token.Text.Whitespace, '\n'),
    ]),
    (r'#rect()[]#rect()[]', [
        (Token.Name.Function, '#rect'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Punctuation, '['),
        (Token.Punctuation, ']'),
        (Token.Name.Function, '#rect'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Punctuation, '['),
        (Token.Punctuation, ']'),
        (Token.Text.Whitespace, '\n'),
    ]),
    ("\n".join(["#rect()[", "= Heading", "]"]), [
        (Token.Name.Function, '#rect'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Punctuation, '['),
            (Token.Text.Whitespace, '\n'),
            (Token.Generic.Heading, '= Heading'),
            (Token.Text.Whitespace, '\n'),
        (Token.Punctuation, ']'),
        (Token.Text.Whitespace, '\n'),
    ]),
    ('#rect()[= Heading]', [
        (Token.Name.Function, '#rect'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Punctuation, '['),
            (Token.Generic.Heading, '= Heading'),
        (Token.Punctuation, ']'),
        (Token.Text.Whitespace, '\n'),
    ]),
    (r'\#', [(Token.Text, r'\#'), (Token.Text.Whitespace, '\n')]),
    (r'[', [(Token.Punctuation, '['), (Token.Text.Whitespace, '\n')]),
    (r'#show', [
        (Token.Keyword.Declaration, '#show'),
        (Token.Text.Whitespace, '\n'),
    ]),
    (r'#let a = 0', [
        (Token.Keyword.Declaration, '#let'),
        (Token.Text.Whitespace, ' '),
        (Token.Text, 'a'),
        (Token.Text.Whitespace, ' '),
        (Token.Operator, '='),
        (Token.Text.Whitespace, ' '),
        (Token.Literal, '0'),
        (Token.Text.Whitespace, '\n'),
    ]),
]

LITERALS = [
    # Literals can only be given in code and not in markup mode.
    # Thus we initialize a code field with '#{..}'
    (r'#{1}', [(Token.Punctuation, '#{'), (Token.Literal, '1'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{11}', [(Token.Punctuation, '#{'), (Token.Literal, '11'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{111}', [(Token.Punctuation, '#{'), (Token.Literal, '111'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{1111}', [(Token.Punctuation, '#{'), (Token.Literal, '1111'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{11111}', [(Token.Punctuation, '#{'), (Token.Literal, '11111'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{111111}', [(Token.Punctuation, '#{'), (Token.Literal, '111111'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{1111111}', [(Token.Punctuation, '#{'), (Token.Literal, '1111111'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{11111111}', [(Token.Punctuation, '#{'), (Token.Literal, '11111111'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{111111111}', [(Token.Punctuation, '#{'), (Token.Literal, '111111111'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{1111111111}', [(Token.Punctuation, '#{'), (Token.Literal, '1111111111'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{11111111111}', [(Token.Punctuation, '#{'), (Token.Literal, '11111111111'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    (r'#{111111111111}', [(Token.Punctuation, '#{'), (Token.Literal, '111111111111'), (Token.Punctuation, '}'), (Token.Text.Whitespace, '\n')]),
    # Test combination
    (r'#{let a = 0}', [
        (Token.Punctuation, '#{'),
        (Token.Keyword.Declaration, 'let'),
        (Token.Text.Whitespace, ' '),
        (Token.Name, 'a'),
        (Token.Text.Whitespace, ' '),
        (Token.Operator, '='),
        (Token.Text.Whitespace, ' '),
        (Token.Literal, '0'),
        (Token.Text.Whitespace, '\n')
    ]),
]

NUMBERED_LIST = [
    # Numbered List
    ('\n'.join(["1. Hello", "2. Mama"]), [
        (Token.Punctuation, '1.'),
        (Token.Text.Whitespace, ' '),
        (Token.Text, 'Hello'),
        (Token.Text.Whitespace, '\n'),
        (Token.Punctuation, '2.'),
        (Token.Text.Whitespace, ' '),
        (Token.Text, 'Mama'),
        (Token.Text.Whitespace, '\n'),
    ]),
]

HEADINGS = [
    (r'', [(Token.Text.Whitespace, '\n')]),
    (r'= Heading', [(Token.Generic.Heading, '= Heading'), (Token.Text.Whitespace, '\n')]),
    (r'== Heading', [(Token.Generic.Heading, '== Heading'), (Token.Text.Whitespace, '\n')]),
    (r'=== Heading', [(Token.Generic.Heading, '=== Heading'), (Token.Text.Whitespace, '\n')]),
    (r'==== Heading', [(Token.Generic.Heading, '==== Heading'), (Token.Text.Whitespace, '\n')]),
    (r'===== Heading', [(Token.Generic.Heading, '===== Heading'), (Token.Text.Whitespace, '\n')]),
    (r'====== Heading', [(Token.Generic.Heading, '====== Heading'), (Token.Text.Whitespace, '\n')]),
]

@pytest.mark.parametrize('matches', LITERALS)
def test_literals(lexer, matches):
    match_typst_synatax(lexer, matches)

@pytest.mark.parametrize('matches', NUMBERED_LIST)
def test_numbered_list(lexer, matches):
    match_typst_synatax(lexer, matches)

@pytest.mark.parametrize('matches', HEADINGS)
def test_headings(lexer, matches):
    match_typst_synatax(lexer, matches)

