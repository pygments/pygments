# -*- coding: utf-8 -*-
"""
    Basic Yang Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Operator, Number, Text, Token
from pygments.lexers import YangLexer

@pytest.fixture(scope='module')
def lexer():
    yield YangLexer()

def test_namespace_1(lexer):
    """
    Namespace `urn:test:std:yang` should not be explicitly highlighted
    """
    fragment = u'namespace urn:test:std:yang;\n'
    tokens = [
        (Token.Keyword, u'namespace'),
        (Token.Text, u' '),
        (Token.Name.Variable, u'urn:test:std:yang'),
        (Token.Punctuation, u';'),
        (Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_namespace_2(lexer):
    """
    Namespace `yang` should be explicitly highlighted
    """
    fragment = u'type yang:counter64;\n'
    tokens = [
        (Token.Keyword, u'type'),
        (Token.Text, u' '),
        (Token.Name.Namespace, u'yang'),
        (Token.Punctuation, u':'),
        (Token.Name.Variable, u'counter64'),
        (Token.Punctuation, u';'),
        (Text, u'\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
