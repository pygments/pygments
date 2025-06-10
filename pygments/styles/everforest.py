"""
pygments.styles.everforest
~~~~~~~~~~~~~~~~~~~~~~~~~~
pygments version of the "everforest" vim theme.
https://github.com/sainnhe/everforest
:copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
:license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import (
    Comment,
    Error,
    Generic,
    Keyword,
    Name,
    Number,
    Operator,
    Punctuation,
    String,
)

__all__ = ["EverforestDarkStyle", "EverforestLightStyle"]


class EverforestDarkStyle(Style):
    """
    A Pygments style based on the Everforest dark theme.
    """

    name = "everforest-dark"
    background_color = "#2d353b"  # bg0
    line_number_color = "#7a8478"  # grey1

    styles = {
        Comment: "italic #859289",  # grey2
        Comment.Preproc: "noitalic #7fbbb3",  # blue
        Comment.Special: "noitalic #e67e80",  # red
        Keyword: "bold #a7c080",  # green
        Keyword.Pseudo: "nobold #a7c080",  # green
        Keyword.Type: "nobold #d699b6",  # purple
        Operator: "#e69875",  # orange
        Operator.Word: "bold #a7c080",  # green
        Name.Builtin: "#7fbbb3",  # blue
        Name.Function: "#a7c080",  # green
        Name.Class: "bold #dbbc7f",  # yellow
        Name.Namespace: "bold #dbbc7f",  # yellow
        Name.Exception: "#e67e80",  # red
        Name.Variable: "#d699b6",  # purple
        Name.Constant: "#d699b6",  # purple
        Name.Label: "bold #d3c6aa",  # fg
        Name.Entity: "bold #e67e80",  # red
        Name.Attribute: "#7fbbb3",  # blue
        Name.Tag: "bold #a7c080",  # green
        Name.Decorator: "bold #e69875",  # orange
        String: "#83c092",  # aqua
        String.Doc: "italic #83c092",  # aqua
        String.Interpol: "italic #d3c6aa",  # fg
        String.Escape: "bold #83c092",  # aqua
        String.Regex: "#83c092",  # aqua
        String.Symbol: "#d699b6",  # purple
        String.Other: "#e69875",  # orange
        Number: "#d699b6",  # purple
        Punctuation: "#d3c6aa",  # fg
        Generic.Heading: "bold #d3c6aa",  # fg
        Generic.Subheading: "bold #d3c6aa",  # fg
        Generic.Deleted: "#e67e80",  # red
        Generic.Inserted: "#a7c080",  # green
        Generic.Error: "#e67e80",  # red
        Generic.Emph: "italic",
        Generic.Strong: "bold",
        Generic.EmphStrong: "bold italic",
        Generic.Prompt: "bold #e69875",  # orange
        Generic.Output: "#d3c6aa",  # fg
        Generic.Traceback: "#e67e80",  # red
        Error: "border:#e67e80",  # red
    }


class EverforestLightStyle(Style):
    """
    A Pygments style based on the Everforest light theme.
    """

    name = "everforest-light"
    background_color = "#fdf6e3"
    line_number_color = "#a6b0a0"

    styles = {
        Comment: "italic #829181",
        Comment.Preproc: "noitalic #3a94c5",
        Comment.Special: "noitalic #f85552",
        Keyword: "bold #8da101",
        Keyword.Pseudo: "nobold #8da101",
        Keyword.Type: "nobold #df69ba",
        Operator: "#fe8019",
        Operator.Word: "bold #8da101",
        Name.Builtin: "#3a94c5",
        Name.Function: "#8da101",
        Name.Class: "bold #dfa000",
        Name.Namespace: "bold #dfa000",
        Name.Exception: "#f85552",
        Name.Variable: "#df69ba",
        Name.Constant: "#df69ba",
        Name.Label: "bold #5c6a72",
        Name.Entity: "bold #f85552",
        Name.Attribute: "#3a94c5",
        Name.Tag: "bold #8da101",
        Name.Decorator: "bold #fe8019",
        String: "#35a77c",
        String.Doc: "italic #35a77c",
        String.Interpol: "italic #5c6a72",
        String.Escape: "bold #35a77c",
        String.Regex: "#35a77c",
        String.Symbol: "#df69ba",
        String.Other: "#fe8019",
        Number: "#df69ba",
        Punctuation: "#5c6a72",
        Generic.Heading: "bold #5c6a72",
        Generic.Subheading: "bold #5c6a72",
        Generic.Deleted: "#f85552",
        Generic.Inserted: "#8da101",
        Generic.Error: "#f85552",
        Generic.Emph: "italic",
        Generic.Strong: "bold",
        Generic.EmphStrong: "bold italic",
        Generic.Prompt: "bold #fe8019",
        Generic.Output: "#5c6a72",
        Generic.Traceback: "#f85552",
        Error: "border:#f85552",
    }
