"""
    Pygments Java properties lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.configs import PropertiesLexer
from pygments.token import Comment, Name, Operator, String, Whitespace


@pytest.mark.parametrize('prefix', ['', ' ', '\t', '\f'])
@pytest.mark.parametrize('key', ['//server/path', '///', '//'])
def test_slashes_start_a_property_key(prefix, key):
    source = prefix + key + '=value\n'
    expected = [(Name.Attribute, key), (Operator, '='), (String, 'value'),
                (Whitespace, '\n')]
    if prefix:
        expected.insert(0, (Whitespace, prefix))
    assert list(PropertiesLexer().get_tokens(source)) == expected


@pytest.mark.parametrize('marker', ['#', '!'])
def test_property_comments(marker):
    source = ' \t' + marker + ' // comment\n'
    assert list(PropertiesLexer().get_tokens(source)) == [
        (Whitespace, ' \t'), (Comment.Single, marker + ' // comment'),
        (Whitespace, '\n'),
    ]


def test_slashes_in_property_values():
    assert list(PropertiesLexer().get_tokens('key=//server/path\n')) == [
        (Name.Attribute, 'key'), (Operator, '='), (String, '//server/path'),
        (Whitespace, '\n'),
    ]
