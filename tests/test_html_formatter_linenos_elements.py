import os
from io import StringIO

import pytest

from pygments.formatters import HtmlFormatter
from pygments.lexers import PythonLexer

from .support import structural_diff

TESTDIR = os.path.dirname(os.path.abspath(__file__))
EXPECTED_OUTPUT_DIR = os.path.join(TESTDIR, "html_linenos_expected_output")
CODE = list(PythonLexer().get_tokens("# a\n# b\n# c"))


def single_line(text):
    return "".join(l.strip() for l in text.splitlines())


# Note: option `anchorlinenos` is currently ignored for `linenos=inline`
@pytest.mark.parametrize("linenos", ["inline", "table"])
@pytest.mark.parametrize("noclasses", ["False", "True"])
@pytest.mark.parametrize("linenostep", ["1", "2"])
@pytest.mark.parametrize("linenostart", ["1", "8"])
@pytest.mark.parametrize("linenospecial", ["0", "3"])
@pytest.mark.parametrize("anchorlinenos", ["False", "True"])
@pytest.mark.parametrize("filename", ["", "testfilename"])
def test_linenos_elements(
    linenos, noclasses, linenostep, linenostart, linenospecial, anchorlinenos, filename
):
    options = dict(
        linenos=linenos,
        noclasses=noclasses,
        linenostep=linenostep,
        linenostart=linenostart,
        linenospecial=linenospecial,
        anchorlinenos=anchorlinenos,
        filename=filename,
    )

    output = StringIO()
    fmt = HtmlFormatter(**options)
    fmt.format(CODE, output)
    html = output.getvalue()

    filename_parts = []
    filename_parts.append(linenos)
    filename_parts.append("nocls" if noclasses == "True" else "cls")
    filename_parts.append("step_" + linenostep)
    filename_parts.append("start_" + linenostart)
    filename_parts.append("special_" + linenospecial)
    filename_parts.append("anchor" if anchorlinenos == "True" else "noanchor")
    filename_parts.append("filename" if filename else "nofilename")
    expected_html_filename = "_".join(filename_parts) + ".html"

    # with open(os.path.join(EXPECTED_OUTPUT_DIR, expected_html_filename), 'w') as f:
    #     import bs4 as BeautifulSoup
    #     f.write(str(BeautifulSoup.BeautifulSoup(html, 'html.parser')))

    with open(os.path.join(EXPECTED_OUTPUT_DIR, expected_html_filename),
              encoding="utf-8") as f:
        expected_html = f.read()

    structural_diff.structural_diff(html, expected_html)
