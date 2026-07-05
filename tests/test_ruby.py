"""
    Basic RubyLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest
from pygments.token import Error, Name
from pygments.lexers.ruby import RubyLexer


@pytest.fixture(scope='module')
def lexer():
    yield RubyLexer()


@pytest.mark.parametrize(
    'method_name',
    (
        # Bare, un-scoped method names
        'a', 'A', 'z', 'Z', 'は', '\u0080', '\uffff',
        'aは0_', 'はA__9', '\u0080はa0_', '\uffff__99Z',

        # Method names with trailing characters
        'aは!', 'はz?', 'はa=',

        # Scoped method names
        'self.a', 'String.は_', 'example.AZ09_!',

        # Operator overrides
        '+', '+@', '-', '-@', '!', '!@', '~', '~@',
        '*', '**', '/', '%', '&', '^', '`',
        '<=>', '<', '<<', '<=', '>', '>>', '>=',
        '==', '!=', '===', '=~', '!~',
        '[]', '[]=',
    )
)
def test_positive_method_names(lexer, method_name):
    """Validate positive method name parsing."""

    text = 'def ' + method_name
    assert list(lexer.get_tokens(text))[-2] == (Name.Function, method_name.rpartition('.')[2])


@pytest.mark.parametrize('method_name', ('1', '_', '<>', '<<=', '>>=', '&&', '||', '==?', '==!', '===='))
def test_negative_method_names(lexer, method_name):
    """Validate negative method name parsing."""

    text = 'def ' + method_name
    assert list(lexer.get_tokens(text))[-2] != (Name.Function, method_name)


def test_unterminated_heredoc_not_duplicated(lexer):
    """An unterminated heredoc must not re-lex (and thus duplicate) its body.

    Regression test for issue #2998: when a heredoc's terminator is never
    found (e.g. a truncated snippet), the callback emitted the remaining
    lines as ``Error`` tokens but did not advance ``ctx.pos``.  Resetting
    ``ctx.end`` to the full length then caused the main loop to re-lex the
    same text, so the heredoc body appeared twice in the output.
    """
    text = (
        'exec_query(<<-end_sql)\n'
        '  SELECT a.attname\n'
        '  FROM pg_attribute a'
    )
    tokens = list(lexer.get_tokens(text))
    output = ''.join(value for _, value in tokens)

    # Every input character is preserved exactly once (get_tokens appends a
    # trailing newline), i.e. nothing is duplicated.
    assert output == text + '\n'
    assert output.count('SELECT a.attname') == 1

    # The unterminated body is reported as Error, not silently re-lexed.
    error_values = [value for token, value in tokens if token is Error]
    assert error_values == ['  SELECT a.attname\n', '  FROM pg_attribute a\n']
