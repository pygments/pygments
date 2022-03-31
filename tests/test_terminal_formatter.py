"""
    Pygments terminal formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
from io import StringIO

from pygments.lexers.sql import PlPgsqlLexer
from pygments.formatters import TerminalFormatter, Terminal256Formatter, \
    HtmlFormatter, LatexFormatter

from pygments.style import Style
from pygments.token import Token
from pygments.lexers import Python3Lexer
from pygments import highlight

DEMO_TEXT = '''\
-- comment
select
* from bar;
'''
DEMO_LEXER = PlPgsqlLexer
DEMO_TOKENS = list(DEMO_LEXER().get_tokens(DEMO_TEXT))

ANSI_RE = re.compile(r'\x1b[\w\W]*?m')


def strip_ansi(x):
    return ANSI_RE.sub('', x)


def test_reasonable_output():
    out = StringIO()
    TerminalFormatter().format(DEMO_TOKENS, out)
    plain = strip_ansi(out.getvalue())
    assert DEMO_TEXT.count('\n') == plain.count('\n')
    print(repr(plain))

    for a, b in zip(DEMO_TEXT.splitlines(), plain.splitlines()):
        assert a == b


def test_reasonable_output_lineno():
    out = StringIO()
    TerminalFormatter(linenos=True).format(DEMO_TOKENS, out)
    plain = strip_ansi(out.getvalue())
    assert DEMO_TEXT.count('\n') + 1 == plain.count('\n')
    print(repr(plain))

    for a, b in zip(DEMO_TEXT.splitlines(), plain.splitlines()):
        assert a in b


class MyStyle(Style):
    styles = {
        Token.Comment:    'ansibrightblack',
        Token.String:     'ansibrightblue bg:ansired',
        Token.Number:     'ansibrightgreen bg:ansigreen',
        Token.Number.Hex: 'ansigreen bg:ansibrightred',
    }


CODE = '''
# this should be a comment
print("Hello World")
async def function(a,b,c, *d, **kwarg:Bool)->Bool:
    pass
    return 123, 0xb3e3

'''


def test_style_html():
    style = HtmlFormatter(style=MyStyle).get_style_defs()
    assert '#555555' in style, "ansigray for comment not html css style"


def test_others_work():
    """Check other formatters don't crash."""
    highlight(CODE, Python3Lexer(), LatexFormatter(style=MyStyle))
    highlight(CODE, Python3Lexer(), HtmlFormatter(style=MyStyle))


def test_256esc_seq():
    """
    Test that a few escape sequences are actually used when using ansi<> color
    codes.
    """
    def termtest(x):
        return highlight(x, Python3Lexer(),
                         Terminal256Formatter(style=MyStyle))

    assert '32;101' in termtest('0x123')
    assert '92;42' in termtest('123')
    assert '90' in termtest('#comment')
    assert '94;41' in termtest('"String"')
