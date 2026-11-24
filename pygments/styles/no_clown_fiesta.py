"""
    pygments.styles.no_clown_fiesta
    ~~~~~~~~~~~~~~~~~~~~~~~

    pygments version of the "no-clown-fiesta" nvim theme.
    https://github.com/aktersnurra/no-clown-fiesta.nvim

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import (
    Comment,
    Error,
    Generic,
    Keyword,
    Literal,
    Name,
    Number,
    Operator,
    Other,
    Punctuation,
    String,
    Text,
    Token,
    Whitespace,
)

__all__ = ['NoClownFiestaStyle']

FG = "#E1E1E1"
BG = "#151515"
ALT_BG = "#171717"
ACCENT = "#202020"
WHITE = "#E1E1E1"
GRAY = "#373737"
MEDIUM_GRAY = "#727272"
LIGHT_GRAY = "#AFAFAF"
BLUE = "#BAD7FF"
GRAY_BLUE = "#7E97AB"
MEDIUM_GRAY_BLUE = "#A2B5C1"
CYAN = "#88afa2"
RED = "#b46958"
GREEN = "#90A959"
YELLOW = "#F4BF75"
ORANGEE = "#FFA557"
CURSOR_FG = "#151515"
CURSOR_BG = "#D0D0D0"
ERROR = "#984936"
ACCENT_LIGHTER_BLUE = "#1e222a"


class NoClownFiestaStyle(Style):
    name = "no_clown_fiesta"
    default_style = ""

    background_color = BG
    highlight_color = ACCENT_LIGHTER_BLUE
    line_number_color = MEDIUM_GRAY
    line_number_background_color = ALT_BG
    line_number_special_color = WHITE
    line_number_special_background_color = ACCENT

    styles = {
        Text: FG,
        Token: FG,
        Whitespace: LIGHT_GRAY,
        Error: f"bold {ERROR}",
        Other: FG,

        Comment: MEDIUM_GRAY,

        Keyword: GRAY_BLUE,
        Keyword.Namespace: RED,

        Name: WHITE,
        Name.Builtin: CYAN,
        Name.Decorator: CYAN,
        Name.Exception: RED,
        Name.Function: CYAN,
        Name.Tag: BLUE,
        Name.Class: CYAN,
        Name.Attribute: CYAN,

        Literal: WHITE,

        String: MEDIUM_GRAY_BLUE,
        String.Char: GREEN,

        Number: RED,

        Operator: WHITE,
        Operator.Word: GRAY_BLUE,
        Punctuation: WHITE,

        Generic.Deleted: ERROR,
        Generic.Inserted: CYAN,
        Generic.Error: f"bold {ERROR}",
        Generic.Output: LIGHT_GRAY,
        Generic.Prompt: BLUE,
        Generic.Subheading: BLUE,
        Generic.Traceback: ERROR,
    }
