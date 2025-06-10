"""
    Pygments tests for using()
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import typing
from pytest import raises

from pygments.lexer import using, bygroups, this, RegexLexer
from pygments.token import String, Text, Keyword


class MyLexer(RegexLexer):
    tokens: typing.ClassVar = {
        'root': [
            (r'#.*',
             using(this, state='invalid')),
            (r'(")(.+?)(")',
             bygroups(String, using(this, state='string'), String)),
            (r'[^"]+', Text),
        ],
        'string': [
            (r'.+', Keyword),
        ],
    }


def test_basic():
    expected = [(Text, 'a'), (String, '"'), (Keyword, 'bcd'),
                (String, '"'), (Text, 'e\n')]
    assert list(MyLexer().get_tokens('a"bcd"e')) == expected


def test_error():
    def gen():
        return list(MyLexer().get_tokens('#a'))
    assert raises(KeyError, gen)
