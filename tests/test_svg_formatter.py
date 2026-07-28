"""
    Pygments SVG formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from io import StringIO

from pygments.formatters import SvgFormatter
from pygments.token import Token


def test_font_options_are_escaped():
    outfile = StringIO()
    fmt = SvgFormatter(fontfamily='"><g x="', fontsize='14px"><g x="')
    fmt.format([(Token.Text, 'x\n')], outfile)
    svg = outfile.getvalue()
    assert '<g font-family="&quot;&gt;&lt;g x=&quot;" ' \
           'font-size="14px&quot;&gt;&lt;g x=&quot;">' in svg
    assert svg.count('<g ') == 1
