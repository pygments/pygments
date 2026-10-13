"""
    Tests for reusing the Groff formatter.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments import format
from pygments.formatters import GroffFormatter
from pygments.style import Style
from pygments.token import Text


class PlainStyle(Style):
    styles = {}


@pytest.mark.parametrize('linenos', [False, True])
@pytest.mark.parametrize('wrap', [0, 5])
@pytest.mark.parametrize('text', ['abcd', 'ab\ncd'])
def test_reuse_starts_a_new_document(linenos, wrap, text):
    options = {'linenos': linenos, 'wrap': wrap, 'style': PlainStyle}
    formatter = GroffFormatter(**options)
    tokens = [(Text, text)]
    expected = format(tokens, GroffFormatter(**options))
    assert format(tokens, formatter) == expected
    assert format(tokens, formatter) == expected


def test_line_numbering_within_one_document():
    formatter = GroffFormatter(linenos=True, style=PlainStyle)
    assert format([(Text, 'one\ntwo')], formatter) == (
        '.nf\n\\f[CR]\n   1 one\n   2 two\n.fi'
    )
