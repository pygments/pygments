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
from pygments.lexers import PythonLexer, PythonConsoleLexer, get_lexer_by_name
from pygments.token import Token

TESTDIR = path.dirname(path.abspath(__file__))
TESTFILE = path.join(TESTDIR, 'test_latex_formatter.py')


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


def test_escape_token_from_a_lexer_is_escaped():
    # Token.Escape marks the verbatim LaTeX that LatexEmbeddedLexer produces,
    # but a couple of lexers also use it for their own escape syntax. Without
    # `escapeinside` it comes from the highlighted file, so it gets escaped.
    src = '%\\immediate\\write18{id}%\n'
    tokensource = list(get_lexer_by_name('ansys').get_tokens(src))
    assert (Token.Escape, '%\\immediate\\write18{id}%') in tokensource

    outfile = StringIO()
    LatexFormatter(nowrap=True).format(tokensource, outfile)
    assert '\\write18' not in outfile.getvalue()
    assert r'\PYZbs{}write18' in outfile.getvalue()


def test_escape_token_stays_verbatim_with_escapeinside():
    outfile = StringIO()
    LatexFormatter(nowrap=True, escapeinside='||').format(
        [(Token.Escape, '$z^2$')], outfile)
    assert '$z^2$' in outfile.getvalue()


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
