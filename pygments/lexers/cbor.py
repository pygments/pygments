"""
    pygments.lexers.cbor
    ~~~~~~~~~~~~~~~~~~~~

    Pygments lexers for CBOR.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import include, RegexLexer, words
from pygments.token import (
    Comment,
    Keyword,
    String,
    Number,
    Punctuation,
    Whitespace,
)

__all__ = ["CborDiagnosticLexer"]


def string_rules(quote_mark, type):
    return [
        (rf"[^{quote_mark}\\]+", type),
        (r"\\u[a-f0-9]{4}", String.Escape),
        (r"\\.", String.Escape),
        (r"\\", String.Escape),
        (quote_mark, type, "#pop"),
    ]


def string_hex_rules(quote_mark, type):
    return [
        (r"[0-9a-fA-F]+", type),
        include("comments"),
        (r"\s+", Whitespace),
        (quote_mark, type, "#pop"),
    ]


def string_base32_rules(quote_mark, type):
    return [
        (r"[A-Z2-7]+", type),
        include("comments"),
        (r"\s+", Whitespace),
        (quote_mark, type, "#pop"),
    ]


def string_base32_hex_rules(quote_mark, type):
    return [
        (r"[0-9A-V]+", type),
        include("comments"),
        (r"\s+", Whitespace),
        (quote_mark, type, "#pop"),
    ]


def string_base64_rules(quote_mark, type):
    return [
        (r"[ \r\n\tA-Za-z0-9/_]+", type),
        include("comments"),
        (quote_mark, type, "#pop"),
    ]


class CborDiagnosticLexer(RegexLexer):
    """
    Lexer for CBOR Diagnostic Notation
    """

    name = "CBOR Diagnostic Notation"
    aliases = ["cbor-diag"]
    filenames = ["*.diag"]
    mimetypes = ["application/cbor-diagnostic"]
    url = "https://datatracker.ietf.org/doc/html/draft-ietf-cbor-edn-literals-24"
    version_added = "2.21"

    # TODO, 2.5.3. Raw String Literals (if published)
    # TODO, 3.1. The "dt" Extension (if published)
    # TODO, 3.2. The "ip" Extension (if published)
    # TODO, 3.3. The "hash" Extension (if published)
    # TODO, 3.4. The "cri" Extension (if published)

    tokens = {
        "eolcomments": [
            (r"(//|#).*\n", Comment.Single),
        ],
        "comments": [
            (r"/\*([^/]|/(?!\*))*\*/", Comment.Multiline),
            (r"/[^/]+/", Comment.Multiline),
            include("eolcomments"),
        ],
        "root": [
            include("comments"),
            (r"\.\.\.", Punctuation),
            (r'""_', String),
            (r"''_", String.Binary),
            (r'"', String, "string"),
            (r"'", String.Binary, "bytestring"),
            (r"h'", String.Binary, "hstring"),
            (r"\(_", Punctuation, "streamstring"),
            (r"b32'", String.Binary, "b32string"),
            (r"h32'", String.Binary, "h32string"),
            (r"b64'", String.Binary, "b64string"),
            (r"\[(_[1-3]?)?", Punctuation, "array"),
            (r"\{(_[1-3]?)?", Punctuation, "object"),
            (r"[+-]?[0-9]+\(", Punctuation, "tag"),
            (r"(\d+\.\d*|\d*\.\d+)([eEp][+-]?[0-9]+)?(_[1-3])?", Number.Float),
            (
                r"[-+]?0x(\[0-9a-f]+\.[0-9a-f]*|[0-9a-f]*\.[0-9a-f]+)([eEp][+-]?[0-9]+)(_[1-3])?",
                Number.Float,
            ),
            (
                r"[-+]?0b(\[0-1]+\.[0-1]*|[0-1]*\.[0-1]+)([eEp][+-]?[0-9]+)?(_[1-3])?",
                Number.Float,
            ),
            (
                r"[-+]?0o(\[0-7]+\.[0-7]*|[0-7]*\.[0-7]+)([eEp][+-]?[0-9]+)?(_[1-3])?",
                Number.Float,
            ),
            (r"[-+]?0x[0-9a-f]+([eEp][+-]?[0-9]+)?(_[1-3])?", Number.Float),
            (r"[-+]?0b[0-1]+([eEp][+-]?[0-9]+)?(_[1-3])?", Number.Float),
            (r"[-+]?0o[0-8]+([eEp][+-]?[0-9]+)?(_[1-3])?", Number.Float),
            (r"[-+]?0x[0-9a-f]+(_[1-3])?", Number.Hex),
            (r"[-+]?0b[0-1]+(_[1-3])?", Number.Bin),
            (r"[-+]?0o[0-8]+(_[1-3])?", Number.Oct),
            (r"[-+]?[0-9]+(_[1-3])?", Number.Integer),
            (r"simple\(\s*-?\d+\s*\)", Keyword),
            (r"<<", Punctuation, "sequence"),
            (r",", Punctuation),
            (
                words(
                    [
                        "false",
                        "Infinity_1",
                        "Infinity_2",
                        "Infinity_3",
                        "Infinity",
                        "-Infinity_1",
                        "-Infinity_2",
                        "-Infinity_3",
                        "-Infinity",
                        "NaN_1",
                        "NaN_2",
                        "NaN_3",
                        "NaN",
                        "null",
                        "true",
                        "undefined",
                    ],
                    suffix=r"\b",
                ),
                Keyword.Constant,
            ),
            (r"\s+", Whitespace),
        ],
        "streamstring": [
            (r",", Punctuation),
            (r"\)", Punctuation, "#pop"),
            include("root"),
        ],
        "tag": [
            (r"\)", Punctuation, "#pop"),
            include("root"),
        ],
        "sequence": [
            (r">>", Punctuation, "#pop"),
            (r",", Punctuation),
            include("root"),
        ],
        "string": string_rules('"', String),
        "bytestring": string_rules("'", String.Binary),
        "hstring": string_hex_rules("'", String.Binary),
        "b32string": string_base32_rules("'", String.Binary),
        "h32string": string_base32_hex_rules("'", String.Binary),
        "b64string": string_base64_rules("'", String.Binary),
        "array": [
            (r",", Punctuation),
            (r"\]", Punctuation, "#pop"),
            include("root"),
        ],
        "object": [
            (r"\}", Punctuation, "#pop"),
            (r":", Punctuation),
            (r",", Punctuation),
            include("root"),
        ],
    }
