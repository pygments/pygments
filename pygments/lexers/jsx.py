"""
    pygments.lexers.jsx
    ~~~~~~~~~~~~~~~~~~~

    Lexers for JSX (React).

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import bygroups, default, include, inherit
from pygments.lexers.javascript import JavascriptLexer
from pygments.token import Name, Operator, Punctuation, String, Text

__all__ = ['JsxLexer']

# Use same tokens as `JavascriptLexer`, but with tags and attributes support
TOKENS = {
    "root": [
        include("jsx"),
        inherit,
    ],
    "jsx": [
        (
            r"(<)(/?)(>)",
            bygroups(Punctuation, Punctuation, Punctuation),
        ),  # JSXFragment <>|</>
        (r"(<)([\w]+)(\.?)", bygroups(Punctuation, Name.Tag, Punctuation), "tag"),
        (
            r"(<)(/)([\w]+)(>)",
            bygroups(Punctuation, Punctuation, Name.Tag, Punctuation),
        ),
        (
            r"(<)(/)([\w]+)",
            bygroups(Punctuation, Punctuation, Name.Tag),
            "fragment",
        ),  # Same for React.Context
    ],
    "tag": [
        (r"\s+", Text),
        (r"([\w-]+\s*)(=)(\s*)", bygroups(Name.Attribute, Operator, Text), "attr"),
        (r"[{}]+", Punctuation),
        (r"[\w\.]+", Name.Attribute),
        (r"(/?)(\s*)(>)", bygroups(Punctuation, Text, Punctuation), "#pop"),
    ],
    "fragment": [
        (r"(.)([\w]+)", bygroups(Punctuation, Name.Attribute)),
        (r"(>)", bygroups(Punctuation), "#pop"),
    ],
    "attr": [
        (r"\{", Punctuation, "expression"),
        (r'".*?"', String, "#pop"),
        (r"'.*?'", String, "#pop"),
        default("#pop"),
    ],
    "expression": [
        (r"\{", Punctuation, "#push"),
        (r"\}", Punctuation, "#pop"),
        include("root"),
    ],
}


class JsxLexer(JavascriptLexer):
    name = "react"
    aliases = ["jsx", "react"]
    filenames = ["*.jsx", "*.react"]
    mimetypes = ["text/jsx", "text/typescript-jsx"]

    flags = re.MULTILINE | re.DOTALL | re.UNICODE

    tokens = TOKENS
