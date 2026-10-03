"""
    Pygments SVG formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from xml.etree import ElementTree

import pytest

from pygments import format, highlight
from pygments.formatters import SvgFormatter
from pygments.lexers import PythonLexer
from pygments.token import Keyword, Text


def svg_text(output):
    root = ElementTree.fromstring(output)
    lines = root.findall('.//{http://www.w3.org/2000/svg}text')
    return '\n'.join(''.join(line.itertext()) for line in lines)


@pytest.mark.parametrize('text', [
    'a\tb', 'abc\tdef', '<>&\tvalue', 'a\nb\tc', '\ta\tb', 'a\t\nb\tc',
])
@pytest.mark.parametrize('split_tokens', [False, True])
@pytest.mark.parametrize('token', [Text, Keyword])
@pytest.mark.parametrize('spacehack', [False, True])
def test_tab_stops_follow_source_columns(text, split_tokens, token, spacehack):
    values = list(text) if split_tokens else [text]
    output = format(((token, value) for value in values), SvgFormatter(spacehack=spacehack))
    expected = text.expandtabs().replace(' ', '\N{NO-BREAK SPACE}') if spacehack else text
    assert svg_text(output) == expected


def test_tab_stops_with_python_lexer():
    text = 'print(\t1)'
    output = highlight(text, PythonLexer(ensurenl=False), SvgFormatter())
    assert svg_text(output) == text.expandtabs().replace(' ', '\N{NO-BREAK SPACE}')
