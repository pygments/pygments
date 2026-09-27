"""
    Basic TerraformLexer Test
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.configs import TerraformLexer
from pygments.token import Error, Whitespace


@pytest.fixture(scope='module')
def lexer():
    yield TerraformLexer()


def test_unterminated_heredoc_not_duplicated(lexer):
    """An unterminated heredoc must not re-lex (and thus duplicate) its body.

    Same defect as #2998 in the Ruby lexer, in the copy of
    ``heredoc_callback`` that lives here: the "end of heredoc not found"
    branch emitted the remaining lines as ``Error`` but left ``ctx.pos``
    on the opening line, so resetting ``ctx.end`` made the main loop lex
    the same text a second time.
    """
    text = 'x = <<EOF\nalpha\nbeta\n'
    tokens = list(lexer.get_tokens(text))
    output = ''.join(value for _, value in tokens)

    assert output == text
    assert output.count('alpha') == 1

    error_values = [value for token, value in tokens if token is Error]
    assert error_values == ['alpha\n', 'beta\n']


def test_terminated_heredoc_is_unaffected(lexer):
    """The closed-heredoc path must keep round-tripping."""
    text = 'x = <<EOF\nalpha\nbeta\nEOF\n'
    output = ''.join(value for _, value in lexer.get_tokens(text))

    assert output == text


def test_whitespace_after_heredoc_operator_is_kept(lexer):
    """The space between ``<<`` and the delimiter must not be dropped.

    The rule matched it with a non-capturing ``\\s*`` that the callback
    never yielded, so those characters disappeared from the output.
    """
    text = 'x = << EOF\nalpha\nEOF\n'
    tokens = list(lexer.get_tokens(text))
    output = ''.join(value for _, value in tokens)

    assert output == text

    # the whitespace is yielded in its own token, right after the operator
    values = [(token, value) for token, value in tokens]
    operator_index = next(i for i, (_, value) in enumerate(values) if value == '<<')
    assert values[operator_index + 1] == (Whitespace, ' ')
