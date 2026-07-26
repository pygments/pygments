"""
Makefile lexer tests
~~~~~~~~~~~~~~~~~~~~

:copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
:license: BSD, see LICENSE for details.
"""

import pytest

from pygments import lex
from pygments.lexers import MakefileLexer
from pygments.token import Comment, Error, Text


@pytest.mark.parametrize(
    "source",
    [
        "define two-lines\necho foo\necho $(bar)\nendef\nall:\n",
        "override define two-lines =\nfoo\n$(bar)\nendef\nall:\n",
    ],
)
def test_define_directive(source):
    tokens = list(lex(source, MakefileLexer()))

    assert "".join(value for _, value in tokens) == source
    assert all(token is not Error for token, _ in tokens)
    assert tokens[0][0] is Comment.Preproc
    assert (Comment.Preproc, "endef\n") in tokens
    assert any(token is Text for token, _ in tokens[1:-1])
