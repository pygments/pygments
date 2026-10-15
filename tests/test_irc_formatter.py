"""
    Pygments IRC formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from io import StringIO

import pytest

from pygments import format
from pygments.lexers import PythonLexer
from pygments.formatters import IRCFormatter
from pygments.token import Token

tokensource = list(PythonLexer().get_tokens("lambda x: 123"))
newlinetokensource = list(PythonLexer().get_tokens("from \\\n\\\n    os import  path\n"))

def test_correct_output():
    hfmt = IRCFormatter()
    houtfile = StringIO()
    hfmt.format(tokensource, houtfile)

    assert '\x0302lambda\x03 x: \x0302123\x03\n' == houtfile.getvalue()

def test_linecount_output():
    hfmt = IRCFormatter(linenos = True)
    houtfile = StringIO()
    hfmt.format(newlinetokensource, houtfile)

    expected_out = '0001: \x0302from\x03\x0315 \\\x03\n0002: \x0315\\\x03\n0003: \x0315    \x03\x1d\x0310os\x03\x1d\x0315 \x03\x0302import\x03  path\n0004: '
    assert expected_out == houtfile.getvalue()


@pytest.mark.parametrize('linenos', [False, True])
@pytest.mark.parametrize('first', ['', 'old', 'old\ntext\n'])
def test_reuse_formatter_restarts_line_numbers(linenos, first):
    formatter = IRCFormatter(linenos=linenos)
    format([(Token.Keyword, first)], formatter)
    tokens = [(Token.Keyword, 'new\ntext')]
    expected = '\x0302new\x03\n\x0302text\x03'
    if linenos:
        expected = '0001: \x0302new\x03\n0002: \x0302text\x03'

    assert format(tokens, formatter) == expected
    assert format(tokens, formatter) == expected
