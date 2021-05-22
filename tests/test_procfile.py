# -*- coding: utf-8 -*-
"""
    Basic ProcfileLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.token import Name, Punctuation, Text
from pygments.lexers.procfile import ProcfileLexer


@pytest.fixture(scope='module')
def lexer():
    yield ProcfileLexer()


def test_basic_line(lexer):
    text = 'task: executable --options'

    tokens = lexer.get_tokens(text)

    for index, token in enumerate(tokens):
        if index == 0:
            assert token == (Name.Label, 'task')
        elif index == 1:
            assert token == (Punctuation, ':')
        else:
            assert token[0] in (Text, Text.Whitespace)


def test_environment_variable(lexer):
    text = '$XDG_SESSION_PATH'

    token = list(lexer.get_tokens(text))[0]

    assert token == (Name.Variable, text)
