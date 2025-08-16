"""
    pygments.lexers.magik
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for Magik file format.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer

__all__ = ["MagikLexer"]

from pygments.token import (
    Comment,
    Operator,
    Keyword,
    Name,
    String,
    Number,
    Punctuation,
    Whitespace,
    Literal,
)


class MagikLexer(RegexLexer):
    """
    Lexer for Magik source code.
    """

    name = "Magik"
    aliases = ["magik"]
    filenames = ["*.magik"]
    mimetypes = ["text/x-magik"]
    version_added = "2.20"
    url = "https://en.wikipedia.org/wiki/Magik_(programming_language)"

    tokens = {
        "root": [
            # Comments
            (r"##.*$", Comment.Special),
            (r"#.*$", Comment.Single),
            # Whitespace
            (r"\s+", Whitespace),
            # Variables
            (r"(?i)\b(_dynamic|_global|_import|_local|_constant|_class)\b", Keyword),
            # Method
            (r"(?i)\b(_abstract|_private|_iter|_method|_endmethod)\b", Keyword),
            # Procedure
            (r"(?i)\b(_proc|_endproc)\b", Keyword),
            # Block
            (r"(?i)\b(_block|_endblock)\b", Keyword),
            # If
            (r"(?i)\b(_if|_then|_elif|_else|_endif)\b", Keyword),
            # Loop
            (
                r"(?i)\b(_loop|_for|_over|_while|_finally|_loopbody|_leave|_continue|_endloop)\b",
                Keyword,
            ),
            # Handling
            (r"(?i)\b(_handling|_default)\b", Keyword),
            # Catch
            (r"(?i)\b(_catch|_endcatch)\b", Keyword),
            # Throw
            (r"(?i)_throw", Keyword),
            # Primitive
            (r"(?i)_primitive", Keyword),
            # Try
            (r"(?i)\b(_try|_when|_endtry)\b", Keyword),
            # Protect
            (r"(?i)\b(_protect|_locking|_protection|_endprotect)\b", Keyword),
            # Lock
            (r"(?i)\b(_lock|_endlock)\b", Keyword),
            # Arguments
            (r"(?i)\b(_gather|_scatter|_allresults|_optional)\b", Keyword),
            # With
            (r"(?i)_with", Keyword),
            # Built-ins
            (r"(?i)\b(_package|_thisthread)\b", Name.Builtin),
            # Special keywords
            (r"(?i)\b(_self|_super|_clone)\b", Name.Class),
            # Kleenean
            (r"(?i)\b(_true|_false|_maybe)\b", Literal),
            # Unset
            (r"(?i)_unset", Literal),
            # Assignment operators
            (
                r"(<<|\^<<|_and<<|_andif<<|_or<<|_orif<<|_xor<<|\*\*<<|\*\*\^<<|\*<<|\*\^<<|/<<|/^<<|_mod<<|_div<<|-<<|-\^<<|\+<<|\+\^<<)",
                Operator,
            ),
            # Return operators
            (r"(>>|_return)", Operator),
            # Relational operators
            (r"(?i)(_is|_isnt|_cf|=|~=|<>|>=|<=|<|>)", Operator),
            # Logical operators
            (r"(?i)\b(_and|_or|_xor|_andif|_orif)\b", Operator),
            # Arithmetic operators
            (r"(?i)(\*\*|\*|/|_mod|_div)", Operator),
            # Unary operators
            (r"(?i)(\+|-|_not|~)", Operator),
            # Numbers
            (r"\b\d+\.\d+([eE&][+-]?\d+)?\b", Number.Float),
            (r"\b\d+[eE&][+-]?\d+\b", Number.Float),
            (r"\b\d+\b", Number.Integer),
            (r"\b(?:[2-9]|[1-2]\d|3[0-6])[rR][a-zA-Z0-9]+\b", Number.Integer),
            # Symbols
            (r":[\w_?!]+", String.Symbol),
            # Piped symbols
            (r":\|[^|]*\|", String.Symbol),
            # Pragma
            (r"(?i)_pragma.*", String.Doc),
            # Identifiers
            (r"\|[\w_?!]+\|", Name.Variable),
            (r"![\w_?!]+!", Name.Variable),
            (r"[\w_?!]+:[\w_?!]+", Name.Variable.Global),
            (r"@[\w_?!:]+", Name.Label),
            (r"[\w_?!]+", Name),
            # Strings
            (r'"([^"\\\n]|\\.)*"', String.Double),
            (r"'([^'\\\n]|\\.)*'", String.Single),
            # Character literal
            (r"%([a-zA-Z][a-zA-Z0-9_?!]*|.| )", String.Char),
            # Regex literal
            (r"/([^/\\]|\\.)*/[qisdlmuCX]*", String.Regex),
            (r"\\", String.Regex),
            # Punctuation
            (r"[\[\]\|@{}().,;$]", Punctuation),
        ]
    }
