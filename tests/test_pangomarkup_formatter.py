"""
    Pygments Pango Markup formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import os
import re
import tempfile
from io import StringIO
from os import path

import pytest

from pygments.formatters import PangoMarkupFormatter, NullFormatter
from pygments.lexers import PythonLexer

TESTDIR = path.dirname(path.abspath(__file__))
TESTFILE = path.join(TESTDIR, 'test_pangomarkup_formatter.py')

with open(TESTFILE, encoding='utf-8') as fp:
    tokensource = list(PythonLexer().get_tokens(fp.read()))


def test_correct_output():
    pfmt = PangoMarkupFormatter(nowrap=True)
    poutfile = StringIO()
    pfmt.format(tokensource, poutfile)

    nfmt = NullFormatter()
    noutfile = StringIO()
    nfmt.format(tokensource, noutfile)

    stripped_markup = re.sub('</?(b|u|i)>', '', poutfile.getvalue())
    stripped_markup = re.sub('<span[^>]+>', '', stripped_markup)
    stripped_markup = re.sub('</span>', '', stripped_markup)

    text = noutfile.getvalue()
    text = text.replace('&', '&amp;')
    text = text.replace('<', '&lt;')
    text = '<tt>' + text + '</tt>'

    assert stripped_markup == text
