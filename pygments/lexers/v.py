"""
    pygments.lexers.v
    ~~~~~~~~~~~~~~~~~

    Lexer for the V programming language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import (Comment, Keyword, Name, Number, Operator,
                             Punctuation, String, Whitespace)

__all__ = ['VLexer']


class VLexer(RegexLexer):
    """
    For `V <https://vlang.io>`_ source code.

    .. versionadded:: 2.21
    """

    name = 'V'
    url = 'https://vlang.io'
    aliases = ['v', 'vlang']
    filenames = ['*.v', '*.vv']
    mimetypes = ['text/x-v']
    version_added = '2.21'

    _KEYWORDS = (
        'as', 'asm', 'assert', 'atomic', 'break', 'const', 'continue',
        'defer', 'else', 'enum', 'fn', 'for', 'go', 'goto', 'if',
        'import', 'in', 'interface', 'is', 'isreftype', 'lock', 'match',
        'module', 'mut', 'nil', 'none', 'or', 'pub', 'return', 'rlock',
        'select', 'shared', 'sizeof', 'spawn', 'static', 'struct',
        'type', 'typeof', 'union', 'unsafe', 'volatile',
    )

    _TYPES = (
        'bool', 'byte', 'byteptr', 'char', 'charptr',
        'f32', 'f64', 'i8', 'i16', 'i32', 'i64', 'i128',
        'int', 'isize', 'rune', 'string', 'u8', 'u16', 'u32',
        'u64', 'u128', 'usize', 'voidptr',
    )

    _BUILTINS = (
        'cap', 'close', 'copy', 'delete', 'dump', 'error',
        'exit', 'flush_stdout', 'free', 'isnil', 'len', 'panic',
        'print', 'println', 'eprintln', 'eprint',
    )

    tokens = {
        'root': [
            (r'\n', Whitespace),
            (r'[^\S\n]+', Whitespace),
            (r'//.*$', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment'),
            # String literals
            (r'c\"', String.Other, 'string_c'),
            (r'r\"', String.Other, 'string_raw'),
            (r'\"', String.Double, 'string'),
            (r"\'[^\'\\]\'|\'\\.[^\']*\'", String.Char),
            # Numbers
            (r'0[xX][0-9a-fA-F_]+', Number.Hex),
            (r'0[oO][0-7_]+', Number.Oct),
            (r'0[bB][01_]+', Number.Bin),
            (r'[0-9][0-9_]*\.[0-9_]*([eE][+-]?[0-9_]+)?', Number.Float),
            (r'[0-9][0-9_]*', Number.Integer),
            # Attributes  @[attribute]  or  [deprecated]
            (r'@\[|\[', Punctuation, 'attribute'),
            # Keywords
            (words(_KEYWORDS, suffix=r'\b'), Keyword),
            (r'(true|false)\b', Keyword.Constant),
            (words(_TYPES, suffix=r'\b'), Keyword.Type),
            (words(_BUILTINS, prefix=r'(?<!\.)', suffix=r'\b'), Name.Builtin),
            # Compile-time identifiers  $if  $for  etc.
            (r'\$[a-z_]\w*', Name.Variable.Magic),
            # C-interop  C.something
            (r'C\.[a-zA-Z_]\w*', Name.Other),
            # Operators
            (r'<<=|>>=|>>>|<<|>>|:=|&&|\|\||\.\.\.|\.\.|[+\-*/%&|^~<>=!]=?', Operator),
            (r'[(){};:,\[\].]', Punctuation),
            (r'[A-Z][a-zA-Z0-9_]*', Name.Class),
            (r'[a-z_]\w*', Name),
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
        'string': [
            (r'[^"\\$]+', String.Double),
            (r'\$\{', String.Interpol, 'interpolation'),
            (r'\$[a-z_]\w*', String.Interpol),
            (r'\\.', String.Escape),
            (r'\"', String.Double, '#pop'),
        ],
        'string_raw': [
            (r'[^"]+', String.Other),
            (r'\"', String.Other, '#pop'),
        ],
        'string_c': [
            (r'[^"\\]+', String.Other),
            (r'\\.', String.Escape),
            (r'\"', String.Other, '#pop'),
        ],
        'interpolation': [
            (r'\}', String.Interpol, '#pop'),
            (r'[^}]+', String.Interpol),
        ],
        'attribute': [
            (r'\]', Punctuation, '#pop'),
            (r'[a-zA-Z_]\w*', Name.Attribute),
            (r'[^\]\[]+', Name.Attribute),
        ],
    }
