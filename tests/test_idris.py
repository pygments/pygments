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
    fragment = u'namespace Foobar\n  links : String\n  links = "abc"'
    tokens = [
        (Keyword.Reserved, u'namespace'),
        (Text, u' '),
        (Keyword.Type, u'Foobar'),
        (Text, u'\n'),
        (Text, u'  '),
        (Name.Function, u'links'),
        (Text, u' '),
        (Operator.Word, u':'),
        (Text, u' '),
        (Keyword.Type, u'String'),
        (Text, u'\n'),
        (Text, u' '),
        (Text, u' '),
        (Text, u'links'),
        (Text, u' '),
        (Operator.Word, u'='),
        (Text, u' '),
        (Literal.String, u'"'),
        (Literal.String, u'abc'),
        (Literal.String, u'"'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_compiler_directive(lexer):
    fragment = u'%link C "object.o"\n%name Vect xs'
    tokens = [
        (Keyword.Reserved, u'%link'),
        (Text, u' '),
        (Keyword.Type, u'C'),
        (Text, u' '),
        (Literal.String, u'"'),
        (Literal.String, u'object.o'),
        (Literal.String, u'"'),
        (Text, u'\n'),
        (Keyword.Reserved, u'%name'),
        (Text, u' '),
        (Keyword.Type, u'Vect'),
        (Text, u' '),
        (Text, u'xs'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
