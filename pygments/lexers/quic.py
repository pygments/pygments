"""
    pygments.lexers.quic
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for the QUIC notation

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, words
from pygments.token import (
    Operator,
    Keyword,
    Name,
    Number,
    Punctuation,
    Whitespace,
)

__all__ = ["QuicLexer"]


class QuicLexer(RegexLexer):
    """
    The QUIC notation, described in RFC9000.
    """

    name = "QUIC notation"
    url = "https://datatracker.ietf.org/doc/html/rfc9000#name-notational-conventions"
    filenames = []
    aliases = ["quic"]
    mimetypes = []
    version_added = "2.21"

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        "root": [
            (r"\s+", Whitespace),
            (r"[^\s\{\},\(\)=\[\]]+(?:\s+[^\s\{\},\(\)=\[\]]+)*", Name.Class),
            (r"\{", Punctuation, "class"),
        ],
        "class": [
            (r"\}", Punctuation, "#pop"),
            (r",", Punctuation),
            (r"\[", Whitespace, "optional"),
            include("field"),
        ],
        "field": [
            (r"\s+", Whitespace),
            (r",", Punctuation),
            (r"[^\s\{\},\(\)=\[\]]+(?:\s+[^\s\{\},\(\)=\[\]]+)*", Name.Property),
            (r"\(", Punctuation, "size"),
            (r"\.\.\.", Punctuation),
            (r"=", Operator, "value"),
        ],
        "optional": [
            (r"\s+", Whitespace),
            (r"\]", Punctuation, "#pop"),
            include("field"),
        ],
        "size": [
            (r"\s+", Whitespace),
            (words(["i"], suffix=r"\b"), Keyword),
            (r"0x[0-9a-f]+", Number.Hex),
            (r"[0-9]+", Number),
            (r"\.\.", Operator),
            (r"\)", Punctuation, "#pop"),
        ],
        "value": [
            (r"\s+", Whitespace),
            (r"0x[0-9a-f]+", Number.Hex, "#pop"),
            (r"[0-9]+", Number, "#pop"),
        ],
    }
