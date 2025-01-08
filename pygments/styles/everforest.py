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
    Token,
    Whitespace,
)

__all__ = ["EverforestDarkStyle"]


class EverforestDarkStyle(Style):
    """
    A Pygments style based on the Everforest theme.
    """

    name = "everforest-dark"

    background_color = "#2b3339"
    line_number_color = "#627e79"

    styles = {
        Comment: "italic #859289",
        Comment.Preproc: "noitalic #7fbbb3",
        Comment.Special: "noitalic bg:#ffffff",
        Keyword: "bold #a7c080",
        Keyword.Pseudo: "nobold",
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
        String: "#83C092",
        String.Doc: "italic",
        String.Interpol: "italic #d3c6aa",
        String.Escape: "bold #83C092",
        String.Regex: "#83C092",
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
