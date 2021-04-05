"""
    Devicetree Lexer Tests
    ~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.devicetree import DevicetreeLexer
from pygments.token import Token


@pytest.fixture(scope="module")
def lexer():
    yield DevicetreeLexer()


@pytest.mark.parametrize(
    "fragment",
    (
        'nodelabel: node@0 { foo = "bar"; };',
        'nodelabel: node { foo = "bar"; };',
        'nodelabel0: nodelabel1: node@0 { foo = "bar"; };',
    ),
)
def test_fragment_out_of_root_node(lexer, fragment):
    """Validate that a devicetree fragment out of a root node is parsed correctly."""

    tokens = list(lexer.get_tokens(fragment))
    assert all(x[0] != Token.Error for x in tokens)
