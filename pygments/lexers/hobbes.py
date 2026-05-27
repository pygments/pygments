"""
    pygments.lexers.hobbes
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Hobbes language.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace

__all__ = ['HobbesLexer']


class HobbesLexer(RegexLexer):
    """
    A lexer for the Hobbes language, a Haskell-like functional language
    used for high-performance data processing.
    """
    name = 'Hobbes'
    url = 'https://github.com/morganstanley/hobbes'
    aliases = ['hobbes']
    filenames = ['*.hob']
    version_added = '2.20'

    tokens = {
        'root': [
            # Whitespace
            (r'\s+', Whitespace),

            # Pragmas: {-# ... #-}
            (r'\{-#.*?#-\}', Comment.Special),

            # Nested block comments: /* ... */
            (r'/\*', Comment.Multiline, 'comment'),

            # Single-line comments
            (r'//.*$', Comment.Single),

            # String literal
            (r'"', String, 'string'),

            # Character literal
            (r"'", String.Char, 'character'),

            # Type-annotated names: <std.string>, <hobbes.storage.Transaction>
            (r'<[\w.]+>', Keyword.Type),

            # Declaration keywords
            (r'\b(class|instance|where|type|data|import|module)\b',
             Keyword.Declaration),

            # Reserved keywords
            (r'\b(if|then|else|let|in|do|return|match|with|case|of'
             r'|exists|forall|where)\b', Keyword.Reserved),

            # Boolean constants
            (r'\b(true|false)\b', Keyword.Constant),

            # Built-in type names (lowercase in hobbes)
            (r'\b(bool|byte|char|short|int|long|int128'
             r'|float|double|timespan|time|datetime)\b', Keyword.Type),

            # Special operators
            (r'::', Operator.Word),
            (r'->', Operator.Word),
            (r'=>', Operator.Word),
            (r'<-', Operator.Word),

            # Lambda
            (r'\\', Name.Function),

            # Operators
            (r'[+\-*/%]=?', Operator),
            (r'[=!<>]=', Operator),
            (r'===', Operator),
            (r'[<>]', Operator),
            (r'&&|\|\||!|~', Operator),
            (r'\+\+', Operator),

            # Hex number literals (before integer to match first)
            (r'0[xX][\da-fA-F]+', Number.Hex),

            # Float literals (with optional f suffix)
            (r'\d+\.\d+[fF]?', Number.Float),

            # Integer literals with type suffixes: L (long), S (short),
            # H (int128), s (timespan)
            (r'\d+[LSHs]\b', Number.Integer),

            # Plain integer literals
            (r'\d+', Number.Integer),

            # Uppercase identifiers are types
            (r'[A-Z]\w*', Keyword.Type),

            # Function definitions at start of line
            (r'^[a-z_]\w*', Name.Function),

            # Other lowercase identifiers
            (r'[a-z_]\w*', Name),

            # Variant/record syntax and other punctuation
            (r'[|(){}\[\],;.^@?#:=]', Punctuation),
        ],

        'comment': [
            (r'[^/*]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),  # nested
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[/*]', Comment.Multiline),
        ],

        'string': [
            (r'[^"\\]+', String),
            (r'\\[abfnrtv"\'\\]', String.Escape),
            (r'\\x[\da-fA-F]{2}', String.Escape),
            (r'"', String, '#pop'),
        ],

        'character': [
            (r"[^'\\]", String.Char),
            (r"\\[abfnrtv\"'\\]", String.Char),
            (r"'", String.Char, '#pop'),
        ],
    }
