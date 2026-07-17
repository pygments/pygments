"""
    pygments.lexers.v
    ~~~~~~~~~~~~~~~~~

    Lexer for the V programming language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words
from pygments.token import (Comment, Keyword, Name, Number, Operator,
                             Punctuation, String, Whitespace)

__all__ = ['VLexer']


class VLexer(RegexLexer):
    """
    For `V <https://vlang.io/>`_ source code.

    .. versionadded:: 2.21
    """

    name = 'V'
    url = 'https://vlang.io/'
    aliases = ['v', 'vlang']
    filenames = ['*.v', '*.vv']
    mimetypes = ['text/x-v']
    version_added = '2.21'

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'//.*?$', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment'),
            # Keywords
            (words((
                'fn', 'struct', 'interface', 'enum', 'type', 'module',
                'import', 'pub', 'mut', 'const', 'global', 'static',
                'if', 'else', 'for', 'in', 'match', 'return', 'or',
                'go', 'spawn', 'defer', 'unsafe', 'asm', 'as', 'is',
                'typeof', 'sizeof', 'isreftype', 'assert',
                'break', 'continue', 'goto',
            ), suffix=r'\b'), Keyword),
            # Built-in types
            (words((
                'bool', 'string', 'rune', 'byte', 'voidptr',
                'i8', 'i16', 'int', 'i32', 'i64', 'i128',
                'u8', 'u16', 'u32', 'u64', 'u128',
                'f32', 'f64', 'usize', 'isize',
            ), suffix=r'\b'), Keyword.Type),
            # Constants
            (words(('true', 'false', 'none', 'nil'), suffix=r'\b'),
             Keyword.Constant),
            # Attributes  @[inline]
            (r'@\[[^\]]+\]', Name.Decorator),
            # Compile-time  $if  $for
            (r'\$[a-zA-Z_]\w*', Name.Builtin),
            # Strings
            (r"r'[^']*'", String.Other),
            (r'r"[^"]*"', String.Other),
            (r"c'[^'\\]*(?:\\.[^'\\]*)*'", String.Other),
            (r"'[^'\\]*(?:\\.[^'\\]*)*'", String.Single),
            (r'"[^"\\]*(?:\\.[^"\\]*)*"', String.Double),
            # Numbers
            (r'0[xX][0-9a-fA-F_]+', Number.Hex),
            (r'0[bB][01_]+', Number.Bin),
            (r'0[oO][0-7_]+', Number.Oct),
            (r'\d[\d_]*\.[\d_]*(?:[eE][+-]?\d+)?', Number.Float),
            (r'\d[\d_]*', Number.Integer),
            # Operators
            (r':=|<<=?|>>=?|&&|\|\||[+\-*/%&|^~]=?|==|!=|<=?|>=?',
             Operator),
            (r'[(){};:,\[\].]', Punctuation),
            # Identifiers
            (r'[A-Z][A-Z0-9_]+\b', Name.Constant),
            (r'[A-Z]\w*', Name.Class),
            (r'[a-z_]\w*', Name),
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
    }
