# -*- coding: utf-8 -*-
"""
    Basic MumpsLexer Test
    ~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2020 by the Pygments team, see AUTHORS
    :license: BSD, see LICENSE for details
"""

import pytest

from pygments.token import Whitespace, Keyword
from pygments.lexers import MumpsLexer

@pytest.fixture(scope='module')
def lexer():
    yield MumpsLexer()

def test_quit_line(lexer):
    fragment = ' q'
    tokens = [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Whitespace, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

