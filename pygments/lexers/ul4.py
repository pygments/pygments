"""
    pygments.lexers.ul4
    ~~~~~~~~~~~~~~~~~~~

    Lexer for the UL4 templating language.

    More information: https://python.livinglogic.de/UL4.html

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments import lexer, token
from pygments.lexers import web, python

__all__ = [
    'UL4Lexer',
    'HTMLUL4Lexer',
    'XMLUL4Lexer',
    'CSSUL4Lexer',
    'JavascriptUL4Lexer',
    'PythonUL4Lexer',
]


class UL4Lexer(lexer.RegexLexer):
    """
    Generic lexer for UL4.

    .. versionadded:: 2.12
    """

    flags = re.MULTILINE | re.DOTALL

    name = 'UL4'
    aliases = ['ul4']
    filenames = ['*.ul4']

    tokens = {
        "root": [
            (
                # Template header without name:
                # ``<?ul4?>``
                r"(<\?)(\s*)(ul4)(\s*)(\?>)",
                lexer.bygroups(token.Comment.Preproc, token.Text.Whitespace, token.Keyword, token.Text.Whitespace, token.Comment.Preproc),
            ),
            (
                # Template header with name (potentially followed by the signature):
                # ``<?ul4 foo(bar=42)?>``
                r"(<\?)(\s*)(ul4)(\s*)([a-zA-Z_][a-zA-Z_0-9]*)?\b",
                lexer.bygroups(token.Comment.Preproc, token.Text.Whitespace, token.Keyword, token.Text.Whitespace, token.Token.Name.Function),
                "ul4", # Switch to "expression" mode
            ),
            (
                # Comment:
                # ``<?note foobar?>``
                r"<\?\s*note\s+.*?\?>",
                token.Comment,
            ),
            (
                # Template documentation:
                # ``<?doc foobar?>``
                r"<\?\s*doc\s+.*?\?>",
                token.String.Doc,
            ),
            (
                # ``<?ignore?>`` tag for commenting out code:
                # ``<?ignore?>...<?end ignore?>``
                r"<\?\s*ignore\s*\?>",
                token.Comment,
                "ignore", # Switch to "ignore" mode
            ),
            (
                # ``<?def?>`` tag for defining local templates
                # ``<?def foo(bar=42)?>...<?end def?>``
                r"(<\?)(\s*)(def)(\s*)([a-zA-Z_][a-zA-Z_0-9]*)?\b",
                lexer.bygroups(token.Comment.Preproc, token.Text.Whitespace, token.Keyword, token.Text.Whitespace, token.Token.Name.Function),
                "ul4", # Switch to "expression" mode
            ),
            (
                # The rest of the supported tags
                r"(<\?)(\s*)(printx|print|for|if|elif|else|while|code|renderblocks?|render)\b",
                lexer.bygroups(token.Comment.Preproc, token.Text.Whitespace, token.Keyword),
                "ul4", # Switch to "expression" mode
            ),
            (
                # ``<?end?>`` tag for ending ``<?def?>``, ``<?for?>``,
                # ``<?if?>``, ``<?while?>``, ``<?renderblock?>`` and
                # ``<?renderblocks?>`` blocks.
                r"(<\?)(\s*)(end)\b",
                lexer.bygroups(token.Comment.Preproc, token.Text.Whitespace, token.Keyword),
                "end", # Switch to "end tag" mode
            ),
            (
                # ``<?whitespace?>`` tag for configuring whitespace handlng
                r"(<\?)(\s*)(whitespace)\b",
                lexer.bygroups(token.Comment.Preproc, token.Text.Whitespace, token.Keyword),
                "whitespace", # Switch to "whitespace" mode
            ),
            # Plain text
            (r"[^<]+", token.Token.Other),
            (r"<", token.Token.Other),
        ],
        # Ignore mode ignores everything upto the matching ``<?end ignore?>`` tag
        "ignore": [
            # Nested ``<?ignore?>`` tag
            (r"<\?\s*ignore\s*\?>", token.Comment, "#push"),
            # ``<?end ignore?>`` tag
            (r"<\?\s*end\s+ignore\s*\?>", token.Comment, "#pop"),
            # Everything else
            (r".+", token.Comment),
        ],
        # UL4 expressions
        "ul4": [
            # End the tag
            (r"\?>", token.Comment.Preproc, "#pop"),
            # Start triple quoted string constant
            ("'''", token.String, "string13"),
            ('""""', token.String, "string23"),
            # Start single quoted string constant
            ("'", token.String, "string1"),
            ('"', token.String, "string2"),
            # Floating point number
            (r"\d+\.\d*([eE][+-]?\d+)?", token.Number.Float),
            (r"\.\d+([eE][+-]?\d+)?", token.Number.Float),
            (r"\d+[eE][+-]?\d+", token.Number.Float),
            # Binary integer: ``0b101010``
            (r"0[bB][01]+", token.Number.Bin),
            # Octal integer: ``0o52``
            (r"0[oO][0-7]+", token.Number.Oct),
            # Hexadecimal integer: ``0x2a``
            (r"0[xX][0-9a-fA-F]+", token.Number.Hex),
            # Date or datetime: ``@(2000-02-29)``/``@(2000-02-29T12:34:56.987654)``
            (r"@\(\d\d\d\d-\d\d-\d\d(T(\d\d:\d\d(:\d\d(\.\d{6})?)?)?)?\)", token.Literal.Date),
            # Color: ``#fff``, ``#fff8f0`` etc.
            (r"#[0-9a-fA-F]{8}", token.Literal.Color),
            (r"#[0-9a-fA-F]{6}", token.Literal.Color),
            (r"#[0-9a-fA-F]{3,4}", token.Literal.Color),
            # Decimal integer: ``42``
            (r"\d+", token.Number.Integer),
            # Operators
            (r"//|==|!=|>=|<=|<<|>>|\+=|-=|\*=|/=|//=|<<=|>>=|&=|\|=|^=|=|[\[\]{},:*/().~%&|<>^+-]", token.Token.Operator),
            # Keywords
            (lexer.words(("for", "in", "if", "else", "not", "is", "and", "or"), suffix=r"\b"), token.Keyword),
            # Builtin constants
            (lexer.words(("None", "False", "True"), suffix=r"\b"), token.Keyword.Constant),
            # Variable names
            (r"[a-zA-Z_][a-zA-Z0-9_]*", token.Name),
            # Whitespace
            (r"\s+", token.Text),
        ],
        # ``<?end ...?>`` tag for closing the last open block
        "end": [
            (r"\?>", token.Comment.Preproc, "#pop"),
            (lexer.words(("for", "if", "def", "while", "renderblock", "renderblocks"), suffix=r"\b"), token.Keyword),
            (r"\s+", token.Text),
        ],
        # Content of the ``<?whitespace ...?>`` tag:
        # ``keep``, ``string`` or ``smart``
        "whitespace": [
            (r"\?>", token.Comment.Preproc, "#pop"),
            (lexer.words(("keep", "strip", "smart"), suffix=r"\b"), token.Comment.Preproc),
            (r"\s+", token.Text.Whitespace),
        ],
        # Inside a string constant
        "string": [
            ("\\\\['\"abtnfr]", token.String.Escape),
            (r"\\x[0-9a-fA-F]{2}", token.String.Escape),
            (r"\\u[0-9a-fA-F]{4}", token.String.Escape),
            (r"\\U[0-9a-fA-F]{8}", token.String.Escape),
            (r".", token.String),
        ],
        # Inside a triple quoted string started with ``'''``
        "string13": [
            (r"'''", token.String, "#pop"),
            lexer.include("string"),
        ],
        # Inside a triple quoted string started with ``"""``
        "string23": [
            (r'"""', token.String, "#pop"),
            lexer.include("string"),
        ],
        # Inside a single quoted string started with ``'``
        "string1": [
            (r"'", token.String, "#pop"),
            lexer.include("string"),
        ],
        # Inside a single quoted string started with ``"``
        "string2": [
            (r'"', token.String, "#pop"),
            lexer.include("string"),
        ],
    }

class HTMLUL4Lexer(lexer.DelegatingLexer):
    """
    Lexer for UL4 embedded in HTML.
    """

    name = 'HTML+UL4'
    aliases = ['html+ul4']
    filenames = ['*.htmlul4']

    def __init__(self, **options):
        super().__init__(web.HtmlLexer, UL4Lexer, **options)


class XMLUL4Lexer(lexer.DelegatingLexer):
    """
    Lexer for UL4 embedded in XML.
    """

    name = 'XML+UL4'
    aliases = ['xml+ul4']
    filenames = ['*.xmlul4']

    def __init__(self, **options):
        super().__init__(web.XmlLexer, UL4Lexer, **options)


class CSSUL4Lexer(lexer.DelegatingLexer):
    """
    Lexer for UL4 embedded in CSS.
    """

    name = 'CSS+UL4'
    aliases = ['css+ul4']
    filenames = ['*.cssul4']

    def __init__(self, **options):
        super().__init__(web.CssLexer, UL4Lexer, **options)


class JavascriptUL4Lexer(lexer.DelegatingLexer):
    """
    Lexer for UL4 embedded in Javascript.
    """

    name = 'Javascript+UL4'
    aliases = ['js+ul4']
    filenames = ['*.jsul4']

    def __init__(self, **options):
        super().__init__(web.JavascriptLexer, UL4Lexer, **options)


class PythonUL4Lexer(lexer.DelegatingLexer):
    """
    Lexer for UL4 embedded in Python.
    """

    name = 'Python+UL4'
    aliases = ['py+ul4']
    filenames = ['*.pyul4']

    def __init__(self, **options):
        super().__init__(python.PythonLexer, UL4Lexer, **options)
