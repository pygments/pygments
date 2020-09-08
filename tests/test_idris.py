# -*- coding: utf-8 -*-
"""
    Basic IdrisLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Keyword, Text, Name, Operator, Literal
from pygments.lexers import IdrisLexer


@pytest.fixture(scope='module')
def lexer():
    yield IdrisLexer()

def test_reserved_word(lexer):
    fragment = 'namespace Foobar\n  links : String\n  links = "abc"'
    tokens = [
        (Keyword.Reserved, 'namespace'),
        (Text, ' '),
        (Keyword.Type, 'Foobar'),
        (Text, '\n'),
        (Text, '  '),
        (Name.Function, 'links'),
        (Text, ' '),
        (Operator.Word, ':'),
        (Text, ' '),
        (Keyword.Type, 'String'),
        (Text, '\n'),
        (Text, ' '),
        (Text, ' '),
        (Text, 'links'),
        (Text, ' '),
        (Operator.Word, '='),
        (Text, ' '),
        (Literal.String, '"'),
        (Literal.String, 'abc'),
        (Literal.String, '"'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_compiler_directive(lexer):
    fragment = '%link C "object.o"\n%name Vect xs'
    tokens = [
        (Keyword.Reserved, '%link'),
        (Text, ' '),
        (Keyword.Type, 'C'),
        (Text, ' '),
        (Literal.String, '"'),
        (Literal.String, 'object.o'),
        (Literal.String, '"'),
        (Text, '\n'),
        (Keyword.Reserved, '%name'),
        (Text, ' '),
        (Keyword.Type, 'Vect'),
        (Text, ' '),
        (Text, 'xs'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
