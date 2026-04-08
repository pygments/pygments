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
        Comment: "italic #859289",
        Comment.Preproc: "noitalic #7fbbb3",
        Comment.Special: "noitalic #e67e80",
        Keyword: "bold #a7c080",
        Keyword.Pseudo: "nobold #a7c080",
        Keyword.Type: "nobold #d699b6",
        Operator: "#e69875",
        Operator.Word: "bold #a7c080",
        Name.Builtin: "#7fbbb3",
        Name.Function: "#a7c080",
        Name.Class: "bold #dbbc7f",
        Name.Namespace: "bold #dbbc7f",
        Name.Exception: "#e67e80",
        Name.Variable: "#d699b6",
        Name.Constant: "#d699b6",
        Name.Label: "bold #d3c6aa",
        Name.Entity: "bold #e67e80",
        Name.Attribute: "#7fbbb3",
        Name.Tag: "bold #a7c080",
        Name.Decorator: "bold #e69875",
        String: "#83c092",
        String.Doc: "italic #83c092",
        String.Interpol: "italic #d3c6aa",
        String.Escape: "bold #83c092",
        String.Regex: "#83c092",
        String.Symbol: "#d699b6",
        String.Other: "#e69875",
        Number: "#d699b6",
        Punctuation: "#d3c6aa",
        Generic.Heading: "bold #d3c6aa",
        Generic.Subheading: "bold #d3c6aa",
        Generic.Deleted: "#e67e80",
        Generic.Inserted: "#a7c080",
        Generic.Error: "#e67e80",
        Generic.Emph: "italic",
        Generic.Strong: "bold",
        Generic.EmphStrong: "bold italic",
        Generic.Prompt: "bold #e69875",
        Generic.Output: "#d3c6aa",
        Generic.Traceback: "#e67e80",
        Error: "border:#e67e80",
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
