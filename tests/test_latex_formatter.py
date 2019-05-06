# -*- coding: utf-8 -*-
"""
    Pygments LaTeX formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import os
import tempfile
from os import path

import pytest

from pygments.formatters import LatexFormatter
from pygments.lexers import PythonLexer

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
