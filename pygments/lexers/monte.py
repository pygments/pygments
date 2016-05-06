# -*- coding: utf-8 -*-
# Copyright (C) 2016 Corbin Simpson. All rights reserved.
# Licensed under the BSD license; see LICENSE for details.

"""
    pygments.lexers.monte
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Monte programming language.

    :copyright: Copyright 2016 Corbin Simpson and the Pygments team; see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments import token
from pygments.lexer import RegexLexer, include, words


# `var` handled separately
# `interface` handled separately
_declarations = ["bind", "def", "fn", "object"]
_methods = ["method", "to"]
_keywords = [
    "as", "break", "catch", "continue", "else", "escape", "exit", "exports",
    "extends", "finally", "for", "guards", "if", "implements", "import",
    "in", "match", "meta", "pass", "return", "switch", "try", "via", "when",
    "while",
]
_operators = [
    # Unary
    '~', '!',
    # Binary
    '+', '-', '*', '/', '%', '**', '&', '|', '^', '<<', '>>',
    # Binary augmented
    '+=', '-=', '*=', '/=', '%=', '**=', '&=', '|=', '^=', '<<=', '>>=',
    # Comparison
    '==', '!=', '<', '<=', '>', '>=', '<=>',
    # Patterns and assignment
    ':=', '?', '=~', '!~', '=>',
    # Calls and sends
    '.', '<-', '->',
]
_escape_chars = [
    (r'\\x[0-9a-fA-F]{2}', token.String.Escape),
    (r'\\u[0-9a-fA-F]{4}', token.String.Escape),
    (r'\\U[0-9a-fA-F]{8}', token.String.Escape),
    (r'\\"', token.String.Escape),
    (r"\\'", token.String.Escape),
    (r"\\\\", token.String.Escape),
    (r"\\b", token.String.Escape),
    (r"\\f", token.String.Escape),
    (r"\\t", token.String.Escape),
    (r"\\n", token.String.Escape),
    (r"\\r", token.String.Escape),
]
_char = _escape_chars + [('.', token.String.Char)]
_identifier = "[_a-zA-Z][_0-9a-zA-Z]*"

_constants = [
    # Void constants
    "null",
    # Bool constants
    "false", "true",
    # Double constants
    "Infinity", "NaN",
    # Special objects
    "M", "Ref", "throw", "traceln",
]

_guards = [
    "Any", "Binding", "Bool", "Bytes", "Char", "DeepFrozen", "Double",
    "Empty", "Int", "List", "Map", "Near", "NullOk", "Same", "Selfless",
    "Set", "Str", "SubrangeGuard", "Transparent", "Void",
]

_safeScope = [
    "_accumulateList", "_accumulateMap", "_auditedBy", "_bind",
    "_booleanFlow", "_comparer", "_equalizer", "_iterForever", "_loop",
    "_makeBytes", "_makeDouble", "_makeFinalSlot", "_makeInt", "_makeList",
    "_makeMap", "_makeMessageDesc", "_makeOrderedSpace", "_makeParamDesc",
    "_makeProtocolDesc", "_makeSourceSpan", "_makeString", "_makeVarSlot",
    "_makeVerbFacet", "_mapExtract", "_matchSame", "_quasiMatcher",
    "_slotToBinding", "_splitList", "_suchThat", "_switchFailed",
    "_validateFor", "b__quasiParser", "eval", "import", "m__quasiParser",
    "makeBrandPair", "makeLazySlot", "safeScope", "simple__quasiParser",
]

class MonteLexer(RegexLexer):
    name = "Monte"
    aliases = ["monte"]
    filenames = ["*.mt"]

    tokens = {
        "root": [
            # Comments
            (r"#[^\n]*\n", token.Comment),

            # Docstrings
            # Apologies for the non-greedy matcher here.
            (r"/\*\*.*?\*/", token.String.Doc),

            # `var` declarations
            (r'\bvar\b', token.Keyword.Declaration, "var"),

            # `interface` declarations
            (r'\binterface\b', token.Keyword.Declaration, "interface"),

            # method declarations
            (words(_methods, prefix="\\b", suffix="\\b"),
                token.Keyword, "method"),

            # All other declarations
            (words(_declarations, prefix="\\b", suffix="\\b"),
                token.Keyword.Declaration),

            # Keywords
            (words(_keywords, prefix="\\b", suffix="\\b"), token.Keyword),

            # Literals
            ("[+-]?0x[0-9a-fo=A-F]+", token.Number.Hex),
            (r"[+-]?[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?", token.Number.Float),
            ("[+-]?[0-9]+", token.Number.Integer),
            ("'", token.String.Double, "char"),
            ('"', token.String.Double, "string"),

            # Quasiliterals
            ('`', token.String.Backtick, "ql"),

            # Operators
            (words(_operators), token.Operator),

            # Verb operators
            (_identifier + '=', token.Operator.Word),

            # Safe scope constants
            (words(_constants, prefix="\\b", suffix="\\b"),
                token.Keyword.Pseudo),

            # Safe scope guards
            (words(_guards, prefix="\\b", suffix="\\b"), token.Keyword.Type),

            # All other safe scope names
            (words(_safeScope, prefix="\\b", suffix="\\b"),
                token.Name.Builtin),

            # Identifiers
            (_identifier, token.Name),

            # Punctuation
            (r"\(|\)|\{|\}|\[|\]|:|,", token.Punctuation),

            # Whitespace
            (" +", token.Whitespace),

            # Definite lexer errors
            ("=", token.Error),
        ],
        "char": [
            # It is definitely an error to have a char of width == 0.
            ("'", token.Error, "root"),
        ] + [t + ("charEnd",) for t in _escape_chars] + [
            (".", token.String.Char, "charEnd"),
        ],
        "charEnd": [
            ("'", token.String.Char, "root"),
            # It is definitely an error to have a char of width > 1.
            ('.', token.Error),
        ],
        # The state of things coming into an interface.
        "interface": [
            (" +", token.Whitespace),
            (_identifier, token.Name.Class, "#pop"),
            include("root"),
        ],
        # The state of things coming into a method.
        "method": [
            (" +", token.Whitespace),
            (_identifier, token.Name.Function, "#pop"),
            include("root"),
        ],
        "string": [
            ('"', token.String.Double, "root"),
        ] + _escape_chars + [
            ("\n", token.String.Double),
            (".", token.String.Double),
        ],
        "ql": [
            ('`', token.String.Backtick, "root"),
        ] + [(r'\$' + t[0], t[1]) for t in _escape_chars] + [
            (r'\$\$', token.String.Escape),
            (r'@@', token.String.Escape),
            (r'\$\{', token.String.Interpol, "qlNest"),
            (r'@\{', token.String.Interpol, "qlNest"),
            (r'\$' + _identifier, token.Name),
            ('@' + _identifier, token.Name),
            ('.', token.String.Backtick),
        ],
        "qlNest": [
            (r'\}', token.String.Interpol, "#pop"),
            include("root"),
        ],
        # The state of things immediately following `var`.
        "var": [
            (" +", token.Whitespace),
            (_identifier, token.Name.Variable, "#pop"),
            include("root"),
        ],
    }

__all__ = "MonteLexer",
