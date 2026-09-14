"""
    Basic CrystalLexer Test
    ~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.crystal import CrystalLexer
from pygments.token import Error


@pytest.fixture(scope='module')
def lexer():
    yield CrystalLexer()


def test_unterminated_heredoc_not_duplicated(lexer):
    """An unterminated heredoc must not re-lex (and thus duplicate) its body.

    Same defect as #2998 in the Ruby lexer, in the copy of
    ``heredoc_callback`` that lives here.
    """
    text = 'x = <<-EOF\nalpha\nbeta\n'
    tokens = list(lexer.get_tokens(text))
    output = ''.join(value for _, value in tokens)

    assert output == text
    assert output.count('alpha') == 1

    error_values = [value for token, value in tokens if token is Error]
    assert error_values == ['alpha\n', 'beta\n']


def test_terminated_heredoc_is_unaffected(lexer):
    """The closed-heredoc path must keep round-tripping."""
    text = 'x = <<-EOF\nalpha\nbeta\nEOF\n'
    output = ''.join(value for _, value in lexer.get_tokens(text))

    assert output == text
