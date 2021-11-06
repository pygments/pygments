"""
    Pygments Robot Framework lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.robotframework import RobotFrameworkLexer


@pytest.fixture(scope='module')
def lexer():
    yield RobotFrameworkLexer()


def assert_same_text(lexer, text):
    """Show that lexed text does not remove any content. """
    tokens = list(lexer.get_tokens_unprocessed(text))
    output = ''.join(t[2] for t in tokens)
    assert text == output


def test_empty_brackets_after_scalar_variable(lexer):
    assert_same_text(lexer, '*** Variables ***\n'
                            '${test}[]\n')


def test_empty_brackets_after_list_variable(lexer):
    assert_same_text(lexer, '*** Variables ***\n'
                            '@{test}[]\n')


def test_empty_brackets_after_dict_variable(lexer):
    assert_same_text(lexer, '*** Variables ***\n'
                            '&{test}[]\n')
