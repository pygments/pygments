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
    String,
    Token,
)

__all__ = ["EverforestDarkStyle", "EverforestLightStyle"]


class EverforestDarkStyle(Style):
    """
    Pygments version of the "everforest" dark vim theme.
    """

    name = "everforest-dark"

    background_color = "#2b3339"
    highlight_color = "#d3c6aa"

    styles = {
        Token: "#d3c6aa",
        Comment: "italic #7a8478",
        Comment.PreProc: "#a7c080",
        Comment.Special: "bold italic #d3c6aa",
        Keyword: "#e67e80",
        Operator.Word: "#e67e80",
        String: "#a7c080",
        String.Escape: "#e69875",
        Number: "#d699b6",
        Name.Builtin: "#e69875",
        Name.Variable: "#83c092",
        Name.Constant: "#d699b6",
        Name.Class: "#a7c080",
        Name.Function: "#a7c080",
        Name.Namespace: "#a7c080",
        Name.Exception: "#e67e80",
        Name.Tag: "#a7c080",
        Name.Attribute: "#dbbc7f",
        Name.Decorator: "#e67e80",
        Generic.Heading: "bold #d3c6aa",
        Generic.Subheading: "underline #d3c6aa",
        Generic.Deleted: "bg:#e67e80 #2b3339",
        Generic.Inserted: "bg:#a7c080 #2b3339",
        Generic.Error: "#e67e80",
        Generic.Emph: "italic",
        Generic.Strong: "bold",
        Generic.EmphStrong: "bold italic",
        Generic.Prompt: "#a7c080",
        Generic.Output: "#d3c6aa",
        Generic.Traceback: "#e67e80",
        Error: "bg:#e67e80 #2b3339",
    }


class EverforestLightStyle(Style):
    """
    Pygments version of the "everforest" light vim theme.
    """

    name = "everforest-light"

    background_color = "#fdf6e3"
    highlight_color = "#5c6a72"

    styles = {
        Comment: "italic #7a8478",
        Comment.PreProc: "#a7c080",
        Comment.Special: "bold italic #5c6a72",
        Keyword: "#e67e80",
        Operator.Word: "#e67e80",
        String: "#a7c080",
        String.Escape: "#e69875",
        Number: "#d699b6",
        Name.Builtin: "#e69875",
        Name.Variable: "#83c092",
        Name.Constant: "#d699b6",
        Name.Class: "#a7c080",
        Name.Function: "#a7c080",
        Name.Namespace: "#a7c080",
        Name.Exception: "#e67e80",
        Name.Tag: "#a7c080",
        Name.Attribute: "#dbbc7f",
        Name.Decorator: "#e67e80",
        Generic.Heading: "bold #5c6a72",
        Generic.Subheading: "underline #5c6a72",
        Generic.Deleted: "bg:#e67e80 #fdf6e3",
        Generic.Inserted: "bg:#a7c080 #fdf6e3",
        Generic.Error: "#e67e80",
        Generic.Emph: "italic",
        Generic.Strong: "bold",
        Generic.Prompt: "#7a8478",
        Generic.Output: "#5c6a72",
        Generic.Traceback: "#e67e80",
        Error: "bg:#e67e80 #fdf6e3",
    }
