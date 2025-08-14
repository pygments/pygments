"""
    pygments.styles.emacs_dark
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    A highlighting style for Pygments, inspired by Emacs (dark background).

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Token, Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic, Whitespace


__all__ = ["EmacsDarkStyle"]


class EmacsDarkStyle(Style):
    """
    The default Emacs style with dark background (emacs -r).
    """
    name = "emacs-dark"

    background_color = "#000000"

    styles = {
        Token:                     "#ccc",

        Whitespace:                "#444",
        Comment:                   "italic #ff7f24",
        Comment.Preproc:           "noitalic",
        Comment.Special:           "noitalic bold",

        Keyword:                   "#0ff",
        Keyword.Type:              "#98fb98",

        Operator:                  "#ccc",
        Operator.Word:             "#b0c4de",

        Name.Builtin:              "#b0c4de",
        Name.Function:             "#87cefa",
        Name.Class:                "#98fb98",
        Name.Namespace:            "#98fb98",
        Name.Exception:            "bold #D2413A",
        Name.Variable:             "#eedd82",
        Name.Constant:             "#7fffd4",
        Name.Label:                "#7fffd4",
        Name.Entity:               "bold #999999",
        Name.Attribute:            "#eedd82",
        Name.Tag:                  "#87cefa",
        Name.Decorator:            "#98fb98",

        String:                    "#ffa07a",
        String.Doc:                "italic",
        String.Interpol:           "bold #BB6688",
        String.Escape:             "bold #BB6622",
        String.Regex:              "#BB6688",
        String.Symbol:             "#B8860B",
        Number:                    "#ccc",

        Generic.Heading:           "bold #87cefa",
        Generic.Subheading:        "bold #eedd82",
        Generic.Deleted:           "#ee2222",
        Generic.Inserted:          "#00A000",
        Generic.Error:             "#FF0000",
        Generic.Emph:              "italic",
        Generic.Strong:            "bold",
        Generic.EmphStrong:        "bold italic",
        Generic.Prompt:            "#0ff",
        Generic.Output:            "#888",

        Error:                     "border:#FF0000"
    }
