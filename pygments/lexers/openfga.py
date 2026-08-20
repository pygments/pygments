"""
    pygments.lexers.openfga
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the OpenFGA configuration DSL.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, words, bygroups
from pygments.token import (
    Comment,
    Keyword,
    Name,
    Number,
    Operator,
    Punctuation,
    String,
    Whitespace,
)


class OpenFgaLexer(RegexLexer):
    """
    For OpenFGA configuration DSL files.
    """

    name = "OpenFGA"
    url = "https://openfga.dev/docs/configuration-language"
    filenames = ["*.fga"]
    aliases = ["openfga", "fga"]
    mimetypes = ["text/x-openfga"]
    version_added = "2.20"
    flags = re.MULTILINE

    structural_keywords = (
        "model",
        "schema",
        "module",
        "type",
        "extend",
        "relations",
        "relation",
        "define",
        "condition",
    )

    logical_keywords = ("and", "or", "from", "with")

    type_keywords = (
        "map",
        "list",
        "bool",
        "string",
        "int",
        "uint",
        "double",
        "duration",
        "timestamp",
        "ipaddress",
    )

    constants = ("true", "false", "null")

    tokens = {
        "root": [
            (r"\n", Whitespace),
            (r"\s+", Whitespace),
            (r"//.*?$", Comment.Single),
            (r"(?<![A-Za-z0-9_./:-])#.*?$", Comment.Single),
            (r"but\s+not\b", Operator.Word),
            (words(structural_keywords + logical_keywords, suffix=r"\b"), Keyword),
            (words(type_keywords, suffix=r"\b"), Keyword.Type),
            (words(constants, suffix=r"\b"), Keyword.Constant),
            (
                r"(define)(\s+)([A-Za-z_][\w.-]*)",
                bygroups(Keyword, Whitespace, Name.Function),
            ),
            (
                r"(condition)(\s+)([A-Za-z_][\w.-]*)",
                bygroups(Keyword, Whitespace, Name.Function),
            ),
            (r"\bin\b", Operator.Word),
            (r"(==|!=|<=|>=|\|\||&&)", Operator),
            (r"[+\-*/%!?\*]", Operator),
            (r"[:.,#\[\](){}<>]", Punctuation),
            (r"0x[0-9a-fA-F]+[uU]?", Number.Hex),
            (r"(?:(?:\d+\.\d+)|(?:\.\d+))(?:[eE][+-]?\d+)?[uU]?", Number.Float),
            (r"\d+(?:[eE][+-]?\d+)?[uU]?", Number.Integer),
            (r'(?s)(?:[bB][rR]?|[rR][bB]?)?"""(?:\\.|[^\\])*?"""', String.Double),
            (r"(?s)(?:[bB][rR]?|[rR][bB]?)?'''(?:\\.|[^\\])*?'''", String.Single),
            (r'(?:[bB][rR]?|[rR][bB]?)?"(\\\\|\\.|[^"\\])*"', String.Double),
            (r"(?:[bB][rR]?|[rR][bB]?)?'(\\\\|\\.|[^'\\])*'", String.Single),
            (r"[A-Za-z_][\w.-]*(?::(?!\s)[A-Za-z0-9_*.-]+)+", Name),
            (r"[A-Za-z_][A-Za-z0-9_.-]*(?:[/.-][A-Za-z0-9_.-]+)*", Name),
        ]
    }


__all__ = ["OpenFgaLexer"]
