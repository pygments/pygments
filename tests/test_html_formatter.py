# -*- coding: utf-8 -*-
"""
    Pygments HTML formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import io
import os
import re
import tempfile
from io import StringIO
from os import path

import pytest

from pygments.formatters import HtmlFormatter, NullFormatter
from pygments.formatters.html import escape_html
from pygments.lexers import PythonLexer
from pygments.style import Style

TESTDIR = path.dirname(path.abspath(__file__))
TESTFILE = path.join(TESTDIR, 'test_html_formatter.py')

with io.open(TESTFILE, encoding='utf-8') as fp:
    tokensource = list(PythonLexer().get_tokens(fp.read()))


def test_correct_output():
    hfmt = HtmlFormatter(nowrap=True)
    houtfile = StringIO()
    hfmt.format(tokensource, houtfile)

    nfmt = NullFormatter()
    noutfile = StringIO()
    nfmt.format(tokensource, noutfile)

    stripped_html = re.sub('<.*?>', '', houtfile.getvalue())
    escaped_text = escape_html(noutfile.getvalue())
    assert stripped_html == escaped_text


def test_external_css():
    # test correct behavior
    # CSS should be in /tmp directory
    fmt1 = HtmlFormatter(full=True, cssfile='fmt1.css', outencoding='utf-8')
    # CSS should be in TESTDIR (TESTDIR is absolute)
    fmt2 = HtmlFormatter(full=True, cssfile=path.join(TESTDIR, 'fmt2.css'),
                         outencoding='utf-8')
    tfile = tempfile.NamedTemporaryFile(suffix='.html')
    fmt1.format(tokensource, tfile)
    try:
        fmt2.format(tokensource, tfile)
        assert path.isfile(path.join(TESTDIR, 'fmt2.css'))
    except IOError:
        # test directory not writable
        pass
    tfile.close()

    assert path.isfile(path.join(path.dirname(tfile.name), 'fmt1.css'))
    os.unlink(path.join(path.dirname(tfile.name), 'fmt1.css'))
    try:
        os.unlink(path.join(TESTDIR, 'fmt2.css'))
    except OSError:
        pass


def test_all_options():
    def check(optdict):
        outfile = StringIO()
        fmt = HtmlFormatter(**optdict)
        fmt.format(tokensource, outfile)

    for optdict in [
        dict(nowrap=True),
        dict(linenos=True, full=True),
        dict(linenos=True, linespans='L'),
        dict(hl_lines=[1, 5, 10, 'xxx']),
        dict(hl_lines=[1, 5, 10], noclasses=True),
    ]:
        check(optdict)

    for linenos in [False, 'table', 'inline']:
        for noclasses in [False, True]:
            for linenospecial in [0, 5]:
                for anchorlinenos in [False, True]:
                    optdict = dict(
                        linenos=linenos,
                        noclasses=noclasses,
                        linenospecial=linenospecial,
                        anchorlinenos=anchorlinenos,
                    )
                    check(optdict)


def test_lineanchors():
    optdict = dict(lineanchors="foo")
    outfile = StringIO()
    fmt = HtmlFormatter(**optdict)
    fmt.format(tokensource, outfile)
    html = outfile.getvalue()
    assert re.search("<pre><span></span><a name=\"foo-1\">", html)


def test_lineanchors_with_startnum():
    optdict = dict(lineanchors="foo", linenostart=5)
    outfile = StringIO()
    fmt = HtmlFormatter(**optdict)
    fmt.format(tokensource, outfile)
    html = outfile.getvalue()
    assert re.search("<pre><span></span><a name=\"foo-5\">", html)


def test_valid_output():
    # test all available wrappers
    fmt = HtmlFormatter(full=True, linenos=True, noclasses=True,
                        outencoding='utf-8')

    handle, pathname = tempfile.mkstemp('.html')
    with os.fdopen(handle, 'w+b') as tfile:
        fmt.format(tokensource, tfile)
    catname = os.path.join(TESTDIR, 'dtds', 'HTML4.soc')
    try:
        import subprocess
        po = subprocess.Popen(['nsgmls', '-s', '-c', catname, pathname],
                              stdout=subprocess.PIPE)
        ret = po.wait()
        output = po.stdout.read()
        po.stdout.close()
    except OSError:
        # nsgmls not available
        pass
    else:
        if ret:
            print(output)
        assert not ret, 'nsgmls run reported errors'

    os.unlink(pathname)


def test_get_style_defs_contains_pre_style():
    style_defs = HtmlFormatter().get_style_defs().splitlines()
    assert style_defs[0] == 'pre { line-height: 125%; }'


def test_get_style_defs_contains_default_line_numbers_styles():
    style_defs = HtmlFormatter().get_style_defs().splitlines()

    assert style_defs[1] == (
        'td.linenos pre '
        '{ color: #000000; background-color: #f0f0f0; padding-left: 5px; padding-right: 5px; }'
    )
    assert style_defs[2] == (
        'span.linenos '
        '{ color: #000000; background-color: #f0f0f0; padding-left: 5px; padding-right: 5px; }'
    )


def test_get_style_defs_contains_style_specific_line_numbers_styles():
    class TestStyle(Style):
        line_number_color = '#ff0000'
        line_number_background_color = '#0000ff'
        line_number_special_color = '#00ff00'
        line_number_special_background_color = '#ffffff'

    style_defs = HtmlFormatter(style=TestStyle).get_style_defs().splitlines()

    assert style_defs[1] == (
        'td.linenos pre '
        '{ color: #ff0000; background-color: #0000ff; padding-left: 5px; padding-right: 5px; }'
    )
    assert style_defs[2] == (
        'span.linenos '
        '{ color: #ff0000; background-color: #0000ff; padding-left: 5px; padding-right: 5px; }'
    )
    assert style_defs[3] == (
        'td.linenos pre.special '
        '{ color: #00ff00; background-color: #ffffff; padding-left: 5px; padding-right: 5px; }'
    )
    assert style_defs[4] == (
        'span.linenos.special '
        '{ color: #00ff00; background-color: #ffffff; padding-left: 5px; padding-right: 5px; }'
    )


@pytest.mark.parametrize(
    "formatter_kwargs, style_defs_args, assert_starts_with, assert_contains",
    [
        [{}, [], ".", []],
        [{"cssclass": "foo"}, [], ".foo .", []],
        [{"cssclass": "foo"}, [".bar"], ".bar .", []],
        [{"cssclass": "foo"}, [[".bar", ".baz"]], ".ba", [".bar .", ".baz ."]],
    ]
)
def test_get_token_style_defs_uses_css_prefix(
    formatter_kwargs, style_defs_args, assert_starts_with, assert_contains
):
    formatter = HtmlFormatter(**formatter_kwargs)

    for line in formatter.get_token_style_defs(*style_defs_args):
        assert line.startswith(assert_starts_with)
        for s in assert_contains:
            assert s in line


def test_get_background_style_defs_uses_multiple_css_prefixes():
    formatter = HtmlFormatter()

    lines = formatter.get_background_style_defs([".foo", ".bar"])
    assert lines[0].startswith(".foo .hll, .bar .hll {")
    assert lines[1].startswith(".foo , .bar {")


def test_unicode_options():
    fmt = HtmlFormatter(title='Föö',
                        cssclass='bär',
                        cssstyles='div:before { content: \'bäz\' }',
                        encoding='utf-8')
    handle, pathname = tempfile.mkstemp('.html')
    with os.fdopen(handle, 'w+b') as tfile:
        fmt.format(tokensource, tfile)


def test_ctags():
    try:
        import ctags
    except ImportError:
        # we can't check without the ctags module, but at least check the exception
        assert pytest.raises(
            RuntimeError, HtmlFormatter, tagsfile='support/tags'
        )
    else:
        # this tagfile says that test_ctags() is on line 165, even if it isn't
        # anymore in the actual source
        fmt = HtmlFormatter(tagsfile='support/tags', lineanchors='L',
                            tagurlformat='%(fname)s%(fext)s')
        outfile = StringIO()
        fmt.format(tokensource, outfile)
        assert '<a href="test_html_formatter.py#L-165">test_ctags</a>' \
            in outfile.getvalue()


def test_filename():
    optdict = dict(filename="test.py")
    outfile = StringIO()
    fmt = HtmlFormatter(**optdict)
    fmt.format(tokensource, outfile)
    html = outfile.getvalue()
    assert re.search("<span class=\"filename\">test.py</span><pre>", html)
