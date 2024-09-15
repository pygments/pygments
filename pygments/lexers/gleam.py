"""
pygments.lexers.gleam
~~~~~~~~~~~~~~~~~~~~~

Lexers for the Gleam language

:copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
:license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import (
    Text, Comment, Keyword, Name, String, Number, Operator, Punctuation
)

# Import builtins from _gleam_builtins.py
from pygments.lexers._gleam_builtins import KEYWORDS, BUILTINS, CONSTANTS

__all__ = ['GleamLexer']

class GleamLexer(RegexLexer):
    name = 'Gleam'
    aliases = ['gleam']
    filenames = ['*.gleam']
    mimetypes = ['text/x-gleam']

    # Define common patterns
    ID = r'_[a-z][a-zA-Z0-9_]*|[a-z][a-zA-Z0-9_]*|_'
    MODULE_NAME = r'(?:[a-z][a-zA-Z0-9_]*/)*[a-z][a-zA-Z0-9_]*'
    TYPE_NAME = r'[A-Z][A-Za-z0-9_]*'

    # Operators and punctuation
    OPERATORS = (
        r'>>|<<|\|\>|::|==|!=|<=|>=|&&|\|\||->|=>|<-|<>|\|>|[+\-*/%!=<>&|.^~]+'
    )
    PUNCTUATION = r'[()\[\]{}.,:;]'

    # Numbers with underscores
    BINARY_NUMBER = r'0b[01](?:_?[01]+)*'
    OCTAL_NUMBER = r'0o[0-7](?:_?[0-7]+)*'
    HEX_NUMBER = r'0x[0-9a-fA-F](?:_?[0-9a-fA-F]+)*'
    FLOAT_NUMBER = r'\d[\d_]*\.\d[\d_]*(?:[eE][+-]?\d[\d_]*)?'
    INTEGER_NUMBER = r'\d[\d_]*'

    # Bit string keywords
    BIT_STRING_KEYWORDS = [
        'binary', 'bits', 'bytes', 'int', 'float', 'bit_string', 'bit_array',
        'utf8', 'utf16', 'utf32', 'utf8_codepoint', 'utf16_codepoint',
        'utf32_codepoint', 'signed', 'unsigned', 'big', 'little', 'native',
        'unit', 'size'
    ]

    tokens = {
        'root': [
            # Whitespace
            (r'\s+', Text),

            # Comments
            (r'//.*?$', Comment.Single),

            # Bit arrays
            (r'<<', Operator, 'bitarray'),

            # Attributes (decorators)
            (r'(@' + ID + r')(\s*)(\()',
             bygroups(Name.Decorator, Text, Punctuation)),

            # Strings
            (r'"""', String.Double, 'triple-string'),
            (r'"', String.Double, 'string'),
            (r"'", String.Single, 'char'),

            # Numbers
            (BINARY_NUMBER, Number.Bin),
            (OCTAL_NUMBER, Number.Oct),
            (HEX_NUMBER, Number.Hex),
            (FLOAT_NUMBER, Number.Float),
            (INTEGER_NUMBER, Number.Integer),

            # Keywords
            (r'\b(' + '|'.join(KEYWORDS) + r')\b', Keyword),

            # Built-in types and constants
            (r'\b(' + '|'.join(BUILTINS) + r')\b', Name.Builtin.Type),
            (r'\b(' + '|'.join(CONSTANTS) + r')\b', Name.Constant),

            # Type names (user-defined)
            (r'\b' + TYPE_NAME + r'\b', Name.Class),

            # Module imports with nested modules and selective imports
            (r'(\bimport)(\s+)(' + MODULE_NAME + r')(\.\{[^\}]+\})?',
             bygroups(Keyword, Text, Name.Namespace, Punctuation)),

            # Function definitions with optional return types
            (r'(\b(?:pub\s+)?fn\b)(\s+)(' + ID + r')(\s*)(\()',
             bygroups(Keyword, Text, Name.Function, Text, Punctuation), 'funcparams'),

            # Anonymous functions
            (r'\bfn\b(\s*)(\()', bygroups(Keyword, Punctuation), 'anonymous-function'),

            # Use expressions
            (r'(\buse\b)(\s+)([^\s<-]+)(\s*)(<-)',
             bygroups(Keyword, Text, Name.Variable, Text, Operator)),

            # Pattern matching in variable bindings
            (r'(\b(?:let|assert)\b)(\s+)([^\s=]+)(\s*)(=)',
             bygroups(Keyword, Text, Name.Variable, Text, Operator)),

            # Module-qualified function calls
            (r'(' + ID + r')(\.)([a-z_][a-zA-Z0-9_]*)' r'(\s*)(\()',
             bygroups(Name.Namespace, Punctuation, Name.Function, Text, Punctuation)),

            # Function calls
            (r'(' + ID + r')(\s*)(\()', bygroups(Name.Function, Text, Punctuation)),

            # Field access
            (r'(' + ID + r')(\.)([a-z_][a-zA-Z0-9_]*)',
             bygroups(Name.Variable, Punctuation, Name.Attribute)),

            # Discard names (e.g., _var)
            (r'\b_[a-z][a-zA-Z0-9_]*\b', Name.Builtin.Pseudo),

            # Operators and punctuation
            (OPERATORS, Operator),
            (PUNCTUATION, Punctuation),

            # Identifiers (variables)
            (r'\b' + ID + r'\b', Name.Variable),
        ],

        'funcparams': [
            (r'\)', Punctuation, '#pop'),
            (r'[^)]', Text),
        ],

        'string': [
            (r'[^"\\]+', String.Double),
            (r'\\.', String.Escape),
            (r'"', String.Double, '#pop'),
        ],

        'triple-string': [
            (r'[^"]+', String.Double),
            (r'"""', String.Double, '#pop'),
            (r'"', String.Double),
        ],

        'char': [
            (r"[^'\\]+", String.Char),
            (r"\\.", String.Escape),
            (r"'", String.Char, '#pop'),
        ],

        'bitarray': [
            (r'>>', Operator, '#pop'),
            (r'\s+', Text),
            (r'\b(' + '|'.join(BIT_STRING_KEYWORDS) + r')\b', Keyword),
            include('root'),
        ],

        'anonymous-function': [
            (r'\)', Punctuation, '#pop'),
            include('root'),
        ],
    }
