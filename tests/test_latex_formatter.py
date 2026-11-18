"""
    Pygments LaTeX formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import os
import tempfile
from os import path
from io import StringIO
from textwrap import dedent

import pytest

from pygments.formatters import LatexFormatter
from pygments.formatters.latex import LatexEmbeddedLexer
from pygments.lexers import PythonLexer, PythonConsoleLexer
from pygments.style import Style
from pygments.token import Token

TESTDIR = path.dirname(path.abspath(__file__))
TESTFILE = path.join(TESTDIR, 'test_latex_formatter.py')


@pytest.mark.parametrize('token, name', [
    (Token.String.Char, 'sc'),
    (Token.String.Doc, 'sd'),
    (Token.String.Symbol, 'ss'),
])
def test_colorful_transparent_background(token, name):
    formatter = LatexFormatter(style='colorful', nowrap=True)
    output = StringIO()
    formatter.format([(token, 'text')], output)
    assert output.getvalue() == rf'\PY{{l+s+{name}}}{{text}}'

    definitions = formatter.get_style_defs().splitlines()
    parent = next(line for line in definitions
                  if line.startswith(r'\@namedef{PY@tok@s}'))
    child = next(line for line in definitions
                 if line.startswith(rf'\@namedef{{PY@tok@{name}}}'))
    assert r'\colorbox' in parent
    assert r'\let\PY@bc=\relax' in child
    inherited = next(line for line in definitions
                     if line.startswith(r'\@namedef{PY@tok@s2}'))
    assert r'\colorbox' in inherited
    assert r'\let\PY@bc=\relax' not in inherited


@pytest.mark.parametrize('child_style', ['bg:', 'noinherit'])
@pytest.mark.parametrize('commandprefix', ['PY', 'Custom'])
def test_clear_background_without_other_styles(child_style, commandprefix):
    class TransparentStyle(Style):
        styles = {
            Token.String: 'bg:#ffffff',
            Token.String.Doc: child_style,
        }

    formatter = LatexFormatter(style=TransparentStyle, commandprefix=commandprefix)
    definitions = formatter.get_style_defs().splitlines()
    assert rf'\@namedef{{{commandprefix}@tok@sd}}{{\let\{commandprefix}@bc=\relax}}' in definitions


def test_correct_output():
    with open(TESTFILE, encoding='utf-8') as fp:
        tokensource = list(PythonLexer().get_tokens(fp.read()))
    hfmt = LatexFormatter(nowrap=True)
    houtfile = StringIO()
    hfmt.format(tokensource, houtfile)

    assert r'\begin{Verbatim}' not in houtfile.getvalue()
    assert r'\end{Verbatim}' not in houtfile.getvalue()


def test_valid_output():
    with open(TESTFILE, encoding='utf-8') as fp:
        tokensource = list(PythonLexer().get_tokens(fp.read()))
    fmt = LatexFormatter(full=True, encoding='latin1')

    handle, pathname = tempfile.mkstemp('.tex')
    # place all output files in /tmp too
    old_wd = os.getcwd()
    os.chdir(os.path.dirname(pathname))
    tfile = os.fdopen(handle, 'wb')
    fmt.format(tokensource, tfile)
    tfile.close()
    try:
        import subprocess
        po = subprocess.Popen(['latex', '-interaction=nonstopmode',
                               pathname], stdout=subprocess.PIPE)
        ret = po.wait()
        output = po.stdout.read()
        po.stdout.close()
    except OSError as e:
        # latex not available
        pytest.skip(str(e))
    else:
        if ret:
            print(output)
        assert not ret, 'latex run reported errors'

    os.unlink(pathname)
    os.chdir(old_wd)


def test_embedded_lexer():
    # Latex surrounded by '|' should be Escaped
    lexer = LatexEmbeddedLexer('|', '|', PythonConsoleLexer())

    # similar to gh-1516
    src = dedent("""\
    >>> x = 1
    >>> y = mul(x, |$z^2$|)  # these |pipes| are untouched
    >>> y
    |$1 + z^2$|""")

    assert list(lexer.get_tokens(src)) == [
        (Token.Generic.Prompt, '>>> '),
        (Token.Name, 'x'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '1'),
        (Token.Text.Whitespace, '\n'),
        (Token.Generic.Prompt, '>>> '),
        (Token.Name, 'y'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Name, 'mul'),
        (Token.Punctuation, '('),
        (Token.Name, 'x'),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Escape, '$z^2$'),
        (Token.Punctuation, ')'),
        (Token.Text, '  '),
        (Token.Comment.Single, '# these |pipes| are untouched'),  # note: not Token.Escape
        (Token.Text.Whitespace, '\n'),
        (Token.Generic.Prompt, '>>> '),
        (Token.Name, 'y'),
        (Token.Text.Whitespace, '\n'),
        (Token.Escape, '$1 + z^2$'),
        (Token.Generic.Output, '\n'),
    ]


def test_embedded_lexer_inherits_options():
    # gh-2975: wrapping a lexer in LatexEmbeddedLexer (as the command line
    # does when `escapeinside` is set) must not override the wrapped lexer's
    # input-preprocessing options such as `stripnl`. Previously the wrapper
    # was built with default options, so `stripnl=False` was ignored and
    # leading/trailing blank lines were stripped anyway.
    from pygments.lexers.special import TextLexer

    inner = TextLexer(stripnl=False)
    wrapped = LatexEmbeddedLexer('|', '|', inner)
    assert wrapped.stripnl is False

    src = '\nLINE\n'
    # The blank leading line survives, exactly as with the unwrapped lexer.
    assert list(wrapped.get_tokens(src)) == list(inner.get_tokens(src))
    assert list(wrapped.get_tokens(src))[0][1].startswith('\n')

    # An option passed explicitly to the wrapper still takes precedence.
    assert LatexEmbeddedLexer('|', '|', inner, stripnl=True).stripnl is True
