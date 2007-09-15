# -*- coding: utf-8 -*-
"""
    pygments.styles.vim
    ~~~~~~~~~~~~~~~~~~~

    A highlighting style for Pygments, inspired by vim.

    :copyright: 2006-2007 by Tim Hatch.
    :license: BSD, see LICENSE for more details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic, Whitespace, Token


class VimStyle(Style):
    """
    Styles somewhat like vim 7.0
    """

    background_color = "#000000"
    default_style = "#cccccc"

    styles = {
        Token:                     "#cccccc",
        Whitespace:                "",
        Comment:                   "#00cdcd",
        Comment.Preproc:           "",
        Comment.Special:           "bold #cd0000",

        Keyword:                   "#cdcd00",
        Keyword.Pseudo:            "",
        Keyword.Type:              "#00cd00",

        Operator:                  "#3399cc",
        Operator.Word:             "#cdcd00",

        Name:                      "",
        Name.Class:                "#cd00cd",
        Name.Builtin:              "#cd00cd",
        Name.Namespace:            "bold #5c5cff",
        Name.Exception:            "bold #666699",
        Name.Variable:             "#00cdcd",

        String:                    "#cd0000",
        Number:                    "#cd00cd",

        Generic.Heading:           "bold #000080",
        Generic.Subheading:        "bold #800080",
        Generic.Deleted:           "#A00000",
        Generic.Inserted:          "#00A000",
        Generic.Error:             "#FF0000",
        Generic.Emph:              "italic",
        Generic.Strong:            "bold",
        Generic.Prompt:            "bold #000080",
        Generic.Output:            "#888",
        Generic.Traceback:         "#04D",

        Error:                     "border:#FF0000"
    }
