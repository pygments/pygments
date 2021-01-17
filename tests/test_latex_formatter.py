# -*- coding: utf-8 -*-
"""
    Pygments LaTeX formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import os
import tempfile
from os import path
from textwrap import dedent

import pytest

from pygments.formatters import LatexFormatter
from pygments.formatters.latex import LatexEmbeddedLexer
from pygments.lexers import PythonLexer, PythonConsoleLexer
from pygments.token import Token

TESTDIR = path.dirname(path.abspath(__file__))
TESTFILE = path.join(TESTDIR, 'test_latex_formatter.py')


def test_valid_output():
    with open(TESTFILE) as fp:
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
        (Token.Text, '\n'),
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
        (Token.Text, '\n'),
        (Token.Generic.Prompt, '>>> '),
        (Token.Name, 'y'),
        (Token.Text, '\n'),
        (Token.Escape, '$1 + z^2$'),
        (Token.Generic.Output, '\n'),
    ]
