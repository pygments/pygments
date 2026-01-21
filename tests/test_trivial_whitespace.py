"""
    Pygments whitespace test
    ~~~~~~~~~~~~~~~~~~~~~~~~

    This test checks whether a single whitespace is lexed with the
    Whitespace token.

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments import lexers
from pygments.token import Whitespace

@pytest.mark.parametrize('cls', lexers._iter_lexerclasses(plugins=True))
def test_trivial_whitespace_token(cls):
    inst = cls()

    try:
        token = list(inst.get_tokens(' '))
    except KeyboardInterrupt:
        raise KeyboardInterrupt(
            'interupted %s.get_tokens(): test_content=%r' %
            (cls.__name__, test_content))

    # idially, only one Token of type Whitespace occurs
    # but for now allow the Newline to be a separate token, but of type Whitespace
    for token_type, token_value in token:
        assert token_type == Whitespace, "%s lexer trivial whitespace test failed: %s of value %r is not of type %s" %  \
            (cls.name, token_type, token_value, Whitespace)

