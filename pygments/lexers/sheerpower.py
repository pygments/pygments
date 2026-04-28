"""
    pygments.lexers.sheerpower
    ~~~~~~~~~~~~~~~~~~~

    Lexer for SheerPower 4GL.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import annotations

import re

from pygments.lexer import RegexLexer, bygroups, include, words
from pygments.token import (
    Comment,
    Keyword,
    Name,
    Number,
    Operator,
    Punctuation,
    String,
    Text,
    Whitespace,
)

__all__ = ["SheerpowerLexer"]


class SheerpowerLexer(RegexLexer):
    """Pygments lexer for the SheerPower 4GL language."""

    name = "SheerPower 4GL"
    aliases = ["sheerpower", "sp4gl", "spsrc"]
    filenames = ["*.spsrc", "*.spinc"]
    mimetypes = ["text/x-sheerpower"]
    url = "https://github.com/iheitlager/sheerpower"
    version_added = ""

    flags = re.IGNORECASE | re.MULTILINE

    # ── Keyword lists ────────────────────────────────────────────────────────
    # Sourced from the Vim, tmLanguage and tree-sitter syntax definitions,
    # which themselves derive from src/sheerpower/lang/lexer.py.

    _declaration_keywords = (
        "dim", "const", "declare", "enum", "program",
        "routine", "local", "private", "module", "cluster", "table",
    )

    _statement_keywords = (
        "print", "input", "prompt", "let", "stop", "delay", "pass", "call",
        "open", "close", "all", "file", "line",
        "add", "delete", "reset", "collect", "copy", "sort", "extract",
        "clear", "end",
    )

    _conditional_keywords = (
        "if", "then", "else", "elseif",
    )

    _loop_keywords = (
        "for", "to", "step", "next", "while", "until", "do", "loop",
        "exit", "iterate",
    )

    _exception_keywords = (
        "when", "exception", "handler", "use", "continue", "retry",
    )

    _operator_keywords = (
        "and", "or", "not", "mod", "is",
        "by", "ascending", "descending",
        "from", "include", "exclude", "unique", "nowait", "using",
        "select", "case", "in",
    )

    _type_keywords = (
        "real", "string", "integer", "boolean", "dynamic",
    )

    _builtin_functions = (
        # String functions
        "len", "left$", "right$", "mid$", "trim$", "ltrim$", "rtrim$",
        "ucase$", "lcase$", "pos", "replace$", "piece$", "pieces",
        "element$", "edit$", "str$", "val", "chr$", "ord",
        "space$", "repeat$", "lpad$", "rpad$", "cpad$", "match",
        "getword$", "format$",
        "base64encode$", "base64decode$", "regex_match$", "sprintf$",
        # Math functions
        "abs", "int", "rnd", "round", "sqr", "log", "log10", "exp",
        "sin", "cos", "tan", "atn", "max", "min",
        # Other builtins
        "size", "date$", "time$", "days",
    )

    tokens = {
        "root": [
            # ── Whitespace ───────────────────────────────────────────────
            (r"[ \t]+", Whitespace),
            (r"\n", Whitespace),

            # ── Comments ─────────────────────────────────────────────────
            (r"!.*$", Comment.Single),
            (r"//.*$", Comment.Single),

            # ── Preprocessor directives ──────────────────────────────────
            (r"(?i)(^\s*)(%define|%include|%if|%else|%endif|%todo|%compile)\b(.*$)",
             bygroups(Whitespace, Comment.Preproc, Comment.Preproc)),

            # ── String literals ──────────────────────────────────────────
            (r"'", String.Single, "string-single"),
            (r'"', String.Double, "string-double"),

            # ── Numbers ──────────────────────────────────────────────────
            (r"\b\d[\d_]*(\.\d[\d_]*)?\b", Number),

            # ── Declaration keywords ─────────────────────────────────────
            (words(_declaration_keywords, suffix=r"\b"), Keyword.Declaration),

            # ── Conditional keywords ─────────────────────────────────────
            (words(_conditional_keywords, suffix=r"\b"), Keyword),

            # ── Loop keywords ────────────────────────────────────────────
            (words(_loop_keywords, suffix=r"\b"), Keyword),

            # ── Exception keywords ───────────────────────────────────────
            (words(_exception_keywords, suffix=r"\b"), Keyword),

            # ── Statement keywords ───────────────────────────────────────
            (words(_statement_keywords, suffix=r"\b"), Keyword),

            # ── Operator keywords ────────────────────────────────────────
            (words(_operator_keywords, suffix=r"\b"), Operator.Word),

            # ── Type keywords ────────────────────────────────────────────
            (words(_type_keywords, suffix=r"\b"), Keyword.Type),

            # ── Built-in functions ───────────────────────────────────────
            (words(_builtin_functions, suffix=r"\b"), Name.Builtin),

            # ── Typed identifiers (suffix $, %, ?) ───────────────────────
            (r"\b[a-zA-Z_]\w*[$%?]", Name.Variable),

            # ── Plain identifiers ────────────────────────────────────────
            (r"\b[a-zA-Z_]\w*\b", Name),

            # ── Operators ────────────────────────────────────────────────
            (r"<>|!=|<=|>=|->", Operator),
            (r"[+\-*/^=<>]", Operator),

            # ── Punctuation ──────────────────────────────────────────────
            (r"[()[\]]", Punctuation),
            (r"[,;.:#]", Punctuation),

            # ── Anything else ────────────────────────────────────────────
            (r".", Text),
        ],

        # ── String states ────────────────────────────────────────────────
        "string-single": [
            (r"''", String.Escape),       # doubled-quote escape
            (r"[^']+", String.Single),
            (r"'", String.Single, "#pop"),
        ],
        "string-double": [
            (r'""', String.Escape),       # doubled-quote escape
            (r'[^"]+', String.Double),
            (r'"', String.Double, "#pop"),
        ],
    }


# Pygments entry-point alias for ``python -m pygments -x -l thisfile.py``
CustomLexer = SheerpowerLexer
