"""
    pygments.lexers.zig
    ~~~~~~~~~~~~~~~~~~~

    Lexers for Zig.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words
from pygments.token import Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace

__all__ = ['ZigLexer']


class ZigLexer(RegexLexer):
    """
    Lexer for the Zig language.

    grammar: https://ziglang.org/documentation/master/#Grammar
    """
    name = 'Zig'
    url = 'http://www.ziglang.org'
    aliases = ['zig']
    filenames = ['*.zig']
    mimetypes = ['text/zig']
    version_added = ''

    # === Keywords ===

    type_keywords = (
        words((
            'bool', 'void', 'noreturn', 'type', 'anyerror', 'anyopaque',
            'f16', 'f32', 'f64', 'f80', 'f128',
            'i8', 'u8', 'i16', 'u16', 'i32', 'u32', 'i64', 'u64', 'i128', 'u128',
            'isize', 'usize', 'comptime_int', 'comptime_float',
            'c_char', 'c_short', 'c_ushort', 'c_int', 'c_uint', 'c_long',
            'c_ulong', 'c_longlong', 'c_ulonglong', 'c_longdouble',
        ), suffix=r'\b'),
        Keyword.Type)

    storage_keywords = (
        words((
            'const', 'var', 'extern', 'packed', 'export', 'pub', 'noalias',
            'inline', 'noinline', 'comptime', 'volatile', 'allowzero',
            'align', 'addrspace', 'linksection', 'threadlocal', 'callconv',
        ), suffix=r'\b'),
        Keyword.Reserved)

    structure_keywords = (
        words(('struct', 'enum', 'union', 'error', 'opaque'), suffix=r'\b'),
        Keyword)

    statement_keywords = (
        words((
            'break', 'return', 'continue', 'asm', 'defer', 'errdefer',
            'unreachable', 'try', 'catch', 'suspend', 'resume', 'nosuspend',
        ), suffix=r'\b'),
        Keyword)

    conditional_keywords = (
        words(('if', 'else', 'switch', 'and', 'or', 'orelse'), suffix=r'\b'),
        Keyword)

    repeat_keywords = (
        words(('while', 'for'), suffix=r'\b'),
        Keyword)

    other_keywords = (
        words(('fn', 'test', 'anyframe', 'anytype'), suffix=r'\b'),
        Keyword)

    constant_keywords = (
        words(('true', 'false', 'null', 'undefined'), suffix=r'\b'),
        Keyword.Constant)

    # === Number Literals ===

    hex_float = (
        r'0x[0-9a-fA-F][0-9a-fA-F_]*'
        r'(?:\.[0-9a-fA-F][0-9a-fA-F_]*(?:[pP][-+]?[0-9][0-9_]*)?'
        r'|[pP][-+]?[0-9][0-9_]*)', Number.Float)

    dec_float = (
        r'[0-9][0-9_]*'
        r'(?:\.[0-9][0-9_]*(?:[eE][-+]?[0-9][0-9_]*)?'
        r'|[eE][-+]?[0-9][0-9_]*)', Number.Float)

    # === Character Literal ===

    char_literal = (
        r"'(?:\\x[0-9a-fA-F]{2}|\\u\{[0-9a-fA-F]+\}|\\[nrt'\"\\]|[^\\'\n])'",
        String.Char)

    # === Operators (ordered longest-first) ===

    operators_4char = (r'<<\|=', Operator)

    operators_3char = (
        r'(?:\+%=|-%=|\*%=|\+\|=|-\|=|\*\|=|<<=|>>=|<<\|)', Operator)

    operators_2char = (
        r'(?:\+\+|\*\*|\|\||<<|>>|\+%|-%|\*%|\+\||-\||\*\|'
        r'|==|!=|<=|>=|\+=|-=|\*=|/=|%=|&=|\|=|\^=|\.\*|\.\?|\.\.)', Operator)

    # === String States ===

    string_rules = [
        (r'\\x[0-9a-fA-F]{2}', String.Escape),
        (r'\\u\{[0-9a-fA-F]+\}', String.Escape),
        (r'\\[nrt\'\"\\]', String.Escape),
        (r'[^\\\"\n]+', String),
        (r'"', String, '#pop'),
        (r'\\', String),
    ]

    quoted_ident_rules = [
        (r'\\x[0-9a-fA-F]{2}', String.Escape),
        (r'\\u\{[0-9a-fA-F]+\}', String.Escape),
        (r'\\[nrt\'\"\\]', String.Escape),
        (r'[^\\\"\n]+', Name),
        (r'"', Name, '#pop'),
        (r'\\', Name),
    ]

    # === Tokens ===

    tokens = {
        'root': [
            # Whitespace
            (r'\n', Whitespace),
            (r'\s+', Whitespace),

            # Comments
            (r'//!.*$', Comment.Special),
            (r'///.*$', Comment.Doc),
            (r'//.*$', Comment.Single),

            # Keywords
            statement_keywords,
            storage_keywords,
            structure_keywords,
            repeat_keywords,
            type_keywords,
            constant_keywords,
            conditional_keywords,
            other_keywords,

            # Number literals
            hex_float,
            dec_float,
            (r'0b[01][01_]*', Number.Bin),
            (r'0o[0-7][0-7_]*', Number.Oct),
            (r'0x[0-9a-fA-F][0-9a-fA-F_]*', Number.Hex),
            (r'[0-9][0-9_]*', Number.Integer),

            # Identifiers
            (r'@[a-zA-Z_][a-zA-Z0-9_]*', Name.Builtin),
            (r'@"', Name, 'quoted_ident'),
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name),

            # Character literal
            char_literal,

            # String literals
            (r'\\\\[^\n]*', String.Heredoc),
            (r'"', String, 'string'),

            # Operators
            operators_4char,
            operators_3char,
            operators_2char,
            (r'[+\-*/%&|^~!<>=]', Operator),

            # Punctuation
            (r'\.\.\.', Punctuation),
            (r'=>', Punctuation),
            (r'->', Punctuation),
            (r'[{}()\[\],.;:?.]+', Punctuation),
        ],

        'string': string_rules,

        'quoted_ident': quoted_ident_rules,
    }
