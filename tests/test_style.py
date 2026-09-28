"""
    Pygments Style tests
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import _ansimap
from pygments.token import Token
from pygments.style import Style


ANSI_DARK_COLORS = {
    'ansiblack': '000000',
    'ansired': '7f0000',
    'ansigreen': '007f00',
    'ansiyellow': '7f7f00',
    'ansiblue': '00007f',
    'ansimagenta': '7f007f',
    'ansicyan': '007f7f',
    'ansigray': 'e5e5e5',
}


def test_ansimap_dark_colors():
    """The dark ANSI colors should use the standard 8-color palette."""
    for name, value in ANSI_DARK_COLORS.items():
        assert _ansimap[name] == value


def test_ansiyellow_resolves_to_dark_yellow():
    """Regression test: ansiyellow used to map to blue-purple ``7f7fe0``."""
    class MyStyle(Style):
        styles = {
            Token.Comment: 'ansiyellow',
        }

    assert MyStyle.style_for_token(Token.Comment)['color'] == '7f7f00'