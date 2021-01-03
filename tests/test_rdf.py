# -*- coding: utf-8 -*-
"""
    Basic RubyLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Name, Punctuation, Text
from pygments.lexers import TurtleLexer, ShExCLexer


@pytest.fixture(scope='module')
def turtle_lexer():
    yield TurtleLexer()

@pytest.fixture(scope='module')
def shexc_lexer():
    yield ShExCLexer()

def test_turtle_prefixed_name_starting_with_number(turtle_lexer):
    fragment = 'alice:6f6e4241-75a2-4780-9b2a-40da53082e54\n'
    tokens = [
        (Name.Namespace, 'alice'),
        (Punctuation, ':'),
        (Name.Tag, '6f6e4241-75a2-4780-9b2a-40da53082e54'),
        (Text, '\n'),
    ]
    assert list(turtle_lexer.get_tokens(fragment)) == tokens

def test_shexc_prefixed_name_starting_with_number(shexc_lexer):
    fragment = 'alice:6f6e4241-75a2-4780-9b2a-40da53082e54\n'
    tokens = [
        (Name.Namespace, 'alice'),
        (Punctuation, ':'),
        (Name.Tag, '6f6e4241-75a2-4780-9b2a-40da53082e54'),
        (Text, '\n'),
    ]
    assert list(shexc_lexer.get_tokens(fragment)) == tokens
