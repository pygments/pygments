import re

from pygments.lexer import RegexLexer, default, include, words
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
    Literal,
)

__all__ = ["IecStLexer"]


class IecStLexer(RegexLexer):
    """
    For `IEC 61131-3 Structured Text
    <https://en.wikipedia.org/wiki/Structured_text>`_ source code.

    This lexer covers the Structured Text (ST) language, including support
    for common data types, control structures, and IEC-specific literals
    like time (T#) and base-prefixed numbers (16#, 2#).

    .. versionadded:: 2.20
    """

    name = "IEC Structured Text"
    aliases = ["iecst", "structured-text", "structuredtext"]
    filenames = ["*.st"]
    mimetypes = ["text/x-iecst"]
    url = "https://en.wikipedia.org/wiki/Structured_text"
    version = "3.0"
    version_added = "2.20"

    flags = re.IGNORECASE

    _duration_keywords = ("TIME", "T")
    _date_time_keywords = (
        "DATE", "TIME_OF_DAY", "DATE_AND_TIME", "D", "TOD", "DT"
    )
    # Hexadecimal part in an hexadecimal integer/floating-point literal.
    # This includes decimal separators matching.
    _hexpart = r"[0-9a-fA-F](\'?[0-9a-fA-F])*"
    # Decimal part in an decimal integer/floating-point literal.
    # This includes decimal separators matching.
    _decpart = r"\d(\'?\d)*"
    # Integer literal suffix (e.g. 'ull' or 'll').
    _intsuffix = r"(([uU][lL]{0,2})|[lL]{1,2}[uU]?)?"

    # Identifier regex with C and C++ Universal Character Name (UCN) support.
    _ident = r"(?!\d)(?:[\w$]|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8})+"
    _namespaced_ident = (
        r"(?!\d)(?:[\w$]|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}|::)+"
    )

    tokens = {
        "whitespace": [
            # Labels:
            (r"\n", Whitespace),
            (r"[^\S\n]+", Whitespace),
            (r"\\\n", Text),  # line continuation
            (r"\(\*", Comment.Multiline, "comment"),
            (r"\/\*", Comment.Multiline, "comment"),
            (r"//[^\n]*", Comment.Singleline),
        ],
        "keywords": [
            (
                words(
                    (
                        "ABS",
                        "ACOS",
                        "ADD",
                        "ADR",
                        "ADRINST",
                        "ASIN",
                        "AT",
                        "ATAN",
                        "BITADR",
                        "BREAK",
                        "CASE",
                        "CONSTANT",
                        "CONTINUE",
                        "COS",
                        "DIV",
                        "DO",
                        "ELSE",
                        "ELSIF",
                        "END_CASE",
                        "END_FOR",
                        "END_FUNCTION",
                        "END_FUNCTION_BLOCK",
                        "END_IF",
                        "END_PROGRAM",
                        "END_REPEAT",
                        "END_VAR",
                        "END_WHILE",
                        "EXP",
                        "EXPT",
                        "FOR",
                        "FUNCTION",
                        "FUNCTION_BLOCK",
                        "IF",
                        "INDEXOF",
                        "INTERNAL",
                        "LIMIT",
                        "LN",
                        "LOG",
                        "MAX",
                        "MIN",
                        "MOD",
                        "MOVE",
                        "MUL",
                        "MUX",
                        "OF",
                        "PRIVATE",
                        "PROGRAM",
                        "PROTECTED",
                        "PUBLIC",
                        "REF",
                        "REPEAT",
                        "RETURN",
                        "ROL",
                        "ROR",
                        "SEL",
                        "SHL",
                        "SHR",
                        "SIN",
                        "SIZEOF",
                        "SQRT",
                        "SUB",
                        "TAN",
                        "THEN",
                        "TO",
                        "TRUNC",
                        "UNTIL",
                        "VAR",
                        "VAR_EXTERNAL",
                        "VAR_GLOBAL",
                        "VAR_INPUT",
                        "VAR_IN_OUT",
                        "VAR_OUTPUT",
                        "VAR_STAT",
                        "WHILE",
                    ),
                    suffix=r"\b",
                ),
                Keyword,
            ),
            (
                words(("TASK", "WITH", "USING", "USES", "FROM"), suffix=r"\b"),
                Keyword,
            ),
            (
                words(
                    (
                        "CONFIGURATION",
                        "TCP",
                        "RESOURCE",
                        "CHANNEL",
                        "LIBRARY",
                        "FOLDER",
                        "BINARIES",
                        "INCLUDES",
                        "SOURCES",
                        "ACTION",
                        "STEP",
                        "INITIAL_STEP",
                        "TRANSITION",
                    ),
                    prefix=r"\b(END_)?",
                    suffix=r"\b",
                ),
                Keyword,
            ),
        ],
        "types": [
            (
                words(
                    (
                        "BOOL",
                        "BYTE",
                        "WORD",
                        "DWORD",
                        "LWORD",
                        "SINT",
                        "USINT",
                        "INT",
                        "UINT",
                        "DINT",
                        "UDINT",
                        "UDINT",
                        "LINT",
                        "ULINT",
                        "REAL",
                        "LREAL",
                        "STRING",
                        "WSTRING",
                        "TIME",
                        "LTIME",
                        "TIME_OF_DAY",
                        "TOD",
                        "DATE",
                        "DATE_AND_TIME",
                        "DT",
                        "D",
                        "T",
                        "POINTER",
                        "ARRAY",
                        "REFERENCE",
                    ),
                    suffix=r"\b",
                ),
                Keyword.Type,
            ),
        ],
        "statements": [
            include("keywords"),
            # Duration and date/time literals must come before types
            # because T, D, DT, etc. are also type keywords
            (
                r"(?i)(T|TIME)#\-?\s*"
                r"(?:(?:[0-9]+\.?[0-9]*)(?:d|h|ms|m|s|us|ns)_?)+",
                Literal.Duration,
            ),
            (
                r"(?i)(TOD|TIME_OF_DAY)#[0-9]{2}:[0-9]{2}:[0-9]{2}"
                r"(?:\.[0-9]{1,3})?",
                Literal.TimeOfDay,
            ),
            (
                r"(?i)(DT|DATE_AND_TIME)#[0-9]{4}-[0-9]{2}-[0-9]{2}-"
                r"[0-9]{2}:[0-9]{2}:[0-9]{2}(?:\.[0-9]{1,3})?",
                Literal.DateTime,
            ),
            (
                r"(?i)(D|DATE)#[0-9]{4}-[0-9]{2}-[0-9]{2}",
                Literal.DateTime,
            ),
            include("types"),
            # Direct addressing: %IX0.0, %QX0.0, %MW100, etc.
            (r"%[IQM][XBWDL]?[0-9]+(\.[0-9]+)*", Name.Label),
            (r"'", String.Single, "string"),
            (r"16#[0-9A-F](_?[0-9A-F])*", Number.Hex),
            (r"8#[0-7](_?[0-7])*", Number.Oct),
            (r"2#[01](_?[01])*", Number.Bin),
            # Hexadecimal floating-point literals (C11, C++17)
            (
                r"0[xX]("
                + _hexpart
                + r"\."
                + _hexpart
                + r"|\."
                + _hexpart
                + r"|"
                + _hexpart
                + r")[pP][+-]?"
                + _hexpart
                + r"[lL]?",
                Number.Float,
            ),
            (
                r"(-)?("
                + _decpart
                + r"\."
                + _decpart
                + r"|\."
                + _decpart
                + r"|"
                + _decpart
                + r")[eE][+-]?"
                + _decpart
                + r"[fFlL]?",
                Number.Float,
            ),
            (
                r"(-)?(("
                + _decpart
                + r"\.("
                + _decpart
                + r")?|\."
                + _decpart
                + r")[fFlL]?)|("
                + _decpart
                + r"[fFlL])",
                Number.Float,
            ),
            (r"(-)?0[xX]" + _hexpart + _intsuffix, Number.Hex),
            (r"(-)?0[bB][01](\'?[01])*" + _intsuffix, Number.Bin),
            (r"(-)?0(\'?[0-7])+" + _intsuffix, Number.Oct),
            (r"(-)?"+_decpart+_intsuffix, Number.Integer),
            (r":=|=>|:|\+|-|\*|/|>=|<=|<>|>|<|&|\|", Operator),
            (
                r"""(?x)\b(?:
                AND|OR|NOT|
                TO_(BOOL|BYTE|D?L?WORD|L?TIME|DATE|DT|TOD|W?CHAR|W?STRING|U?S?D?L?INT|L?REAL)|
                (ANY|BOOL|BYTE|D?L?WORD|L?TIME|DATE|DT|TOD|W?CHAR|W?STRING|U?S?D?L?INT|L?REAL)_TO_(BOOL|BYTE|D?L?WORD|L?TIME|DATE|DT|TOD|W?CHAR|W?STRING|U?S?D?L?INT|L?REAL)
              )\b""",
                Operator.Word,
            ),
            (r"[()\[\],.]", Punctuation),
            (r"(true|false|null)\b", Name.Builtin),
            (r"([\w]+)(\()", Name.Function),
            (_ident, Name),
        ],
        "string": [("[^'\n]+", String), ("'", String.Single, "#pop")],
        "root": [
            include("whitespace"),
            include("keywords"),
            include("types"),
            default("statement"),
        ],
        "statement": [
            include("whitespace"),
            include("statements"),
            (r"\}", Punctuation),
            (r"[{;]", Punctuation, "#pop"),
        ],
        "comment": [
            (r"\(\*", Comment.Multiline, "#push"),
            (r"\*\)", Comment.Multiline, "#pop"),
            (r"\/\*", Comment.Multiline, "#push"),
            (r"\*\/", Comment.Multiline, "#pop"),
            (r"[^(*/)]+", Comment.Multiline),
            (r"[(*/)]", Comment.Multiline),
        ],
    }
