# -*- coding: utf-8 -*-
"""
    Pygments LaTeX formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import os
import tempfile
from os import path
from io import StringIO

import pytest

from pygments.formatters import LatexFormatter
from pygments.lexers import PythonLexer

TESTDIR = path.dirname(path.abspath(__file__))
TESTFILE = path.join(TESTDIR, 'test_latex_formatter.py')

with open(TESTFILE) as fp:
    tokensource = list(PythonLexer().get_tokens(fp.read()))


def test_correct_output():
    hfmt = LatexFormatter(nowrap=True)
    houtfile = StringIO()
    hfmt.format(tokensource, houtfile)

    assert r'\begin{Verbatim}' not in houtfile.getvalue()
    assert r'\end{Verbatim}' not in houtfile.getvalue()


def test_valid_output():
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
