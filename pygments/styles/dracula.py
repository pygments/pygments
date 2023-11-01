"""
    pygments.styles.dracula
    ~~~~~~~~~~~~~~~~~~~~~~~

    Pygments version of `Dracula` from https://github.com/dracula/dracula-theme.

    Based on the Dracula Theme for pygments by Chris Bracco.
    See https://github.com/dracula/pygments/tree/fee9ed5613d1086bc01b9d0a5a0e9867a009f571

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, Literal, \
    Number, Operator, Other, Punctuation, Text, Generic, Whitespace


__all__ = ['DraculaStyle']


class DraculaStyle(Style):
    name = 'dracula'

    background = "#282A36"
    foreground = "#F8F8F2"
    selection = "#44475A"
    comment = "#6272A4"
    cyan = "#8BE9FD"
    green = "#50FA7B"
    orange = "#FFB86C"
    pink = "#FF79C6"
    purple = "#BD93F9"
    red = "#FF5555"
    yellow = "#F1FA8C"

    deletion = "#8B080B"

    styles = {
        Whitespace: foreground,

        Comment: comment,
        Comment.Preproc: pink,

        Generic: foreground,
        Generic.Deleted: deletion,
        Generic.Emph: "underline",
        Generic.Heading: "bold",
        Generic.Inserted: "bold",
        Generic.Output: selection,
        Generic.EmphStrong: "underline",
        Generic.Subheading: "bold",

        Error: foreground,

        Keyword: pink,
        Keyword.Constant: pink,
        Keyword.Declaration: cyan + " italic",
        Keyword.Namespace: pink + " italic",
        Keyword.Type: cyan,

        Literal: foreground,
        Literal.Date: foreground,

        Name: foreground,
        Name.Attribute: green,
        Name.Builtin: cyan,
        Name.Class: green,
        Name.Function: green,
        Name.Label: cyan + " italic",
        Name.Tag: pink,
        Name.Variable: cyan + " italic",
        Name.Variable.Class: cyan + " italic",
        Name.Variable.Global: cyan + " italic",
        Name.Variable.Instance: cyan + " italic",

        Number: orange,

        Operator: pink,

        Other: foreground,

        Punctuation: foreground,

        String: purple,

        Text: foreground,
    }
