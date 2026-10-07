"""
    pygments.lexers.go
    ~~~~~~~~~~~~~~~~~~

    Lexers for the Google Go language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace

__all__ = ['GoLexer']


class GoLexer(RegexLexer):
    """
    For Go source.
    """
    name = 'Go'
    url = 'https://go.dev/'
    filenames = ['*.go']
    aliases = ['go', 'golang']
    mimetypes = ['text/x-gosrc']
    version_added = '1.2'

    tokens = {
        'root': [
            (r'\n', Whitespace),
            (r'\s+', Whitespace),
            (r'(\\)(\n)', bygroups(Text, Whitespace)),  # line continuations
            (r'//(.*?)$', Comment.Single),
            (r'/(\\\n)?[*][\s\S]*?[*](\\\n)?/', Comment.Multiline),
            (r'(import|package)\b', Keyword.Namespace),
            (r'(var|func|struct|type|interface|const)\b',
             Keyword.Declaration),
            (words((
                'break', 'default', 'select', 'case', 'defer', 'go',
                'else', 'goto', 'switch', 'fallthrough', 'if', 'range',
                'continue', 'for', 'return'), suffix=r'\b'),
             Keyword),
            (r'(true|false|iota|nil)\b', Keyword.Constant),
            (words((
                'print', 'println', 'panic', 'recover', 'close', 'complex',
                'real', 'imag', 'len', 'cap', 'append', 'copy', 'delete',
                'new', 'make', 'min', 'max', 'clear'), suffix=r'\b(\()'),
             bygroups(Name.Builtin, Punctuation)),
            (words((
                'uint', 'uint8', 'uint16', 'uint32', 'uint64',
                'int', 'int8', 'int16', 'int32', 'int64',
                'float32', 'float64', 'map', 'chan',
                'complex64', 'complex128', 'byte', 'rune',
                'string', 'bool', 'error', 'uintptr', 'any', 'comparable'), suffix=r'\b'),
             Keyword.Type),
            # https://go.dev/ref/spec
            # imaginary_lit
            # -- binary_lit "i"
            (r'(0[bB](?:_?[01])+)(i)', bygroups(Number.Bin, Name.Builtin)),
            # -- octal_lit "i" (but o or O required)
            (r'(0[oO](?:_?[0-7])+)(i)', bygroups(Number.Oct, Name.Builtin)),
            # -- hex_lit "i"
            (r'(0[xX](?:_?[0-9a-fA-F])+)(i)', bygroups(Number.Hex, Name.Builtin)),
            # -- hex_float_lit "i"
            (r'(0[xX](?:_?[a-fA-F\d])+\.?(?:[a-fA-F\d](?:_?[a-fA-F\d])*)?[pP][+-]?\d(?:_?\d)*)(i)', bygroups(Number.Float.Hex, Name.Builtin)),
            (r'(0[xX]\.[a-fA-F\d](?:_?[a-fA-F\d])*[pP][+-]?\d(?:_?\d)*)(i)', bygroups(Number.Float.Hex, Name.Builtin)),
            # -- decimal_digits "i"
            (r'([0-9](?:_?[0-9])*)(i)', bygroups(Number.Integer, Name.Builtin)),
            # -- decimal_float_lit "i"
            (r'(\d(?:_?\d)*\.(?:\d(?:_?\d)*)?(?:[eE][+-]?\d(?:_?\d)*)?)(i)', bygroups(Number.Float, Name.Builtin)),
            (r'(\d(?:_?\d)*[eE][+-]?\d(?:_?\d)*)(i)', bygroups(Number.Float, Name.Builtin)),
            (r'(\.\d(?:_?\d)*(?:[eE][+-]?\d(?:_?\d)*)?)(i)', bygroups(Number.Float, Name.Builtin)),
            # float_lit
            # -- hex_float_lit
            (r'0[xX](_?[a-fA-F\d])+\.?([a-fA-F\d](_?[a-fA-F\d])*)?[pP][+-]?\d(_?\d)*', Number.Float.Hex),
            (r'0[xX]\.[a-fA-F\d](_?[a-fA-F\d])*[pP][+-]?\d(_?\d)*', Number.Float.Hex),
            # -- decimal_float_lit
            (r'\d(_?\d)*\.(\d(_?\d)*)?([eE][+-]?\d(_?\d)*)?', Number.Float),
            (r'\d(_?\d)*[eE][+-]?\d(_?\d)*', Number.Float),
            (r'\.\d(_?\d)*([eE][+-]?\d(_?\d)*)?', Number.Float),
            # int_lit
            # -- binary_lit
            (r'0[bB](_?[01])+', Number.Bin),
            # -- octal_lit
            (r'0[oO]?(_?[0-7])+', Number.Oct),
            # -- hex_lit
            (r'0[xX](_?[0-9a-fA-F])+', Number.Hex),
            # -- decimal_lit
            (r'(0|[1-9](_?[0-9])*)', Number.Integer),
            # rune_lit
            (r"(')(\\(?:[0-7]{3}|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}|['\\abfnrtv]))(')",
             bygroups(String.Char, String.Escape, String.Char)),
            (r"'[^'\\\n]'", String.Char),
            # string_lit
            # -- raw_string_lit
            (r'`[^`]*`', String.Backtick),
            # -- interpreted_string_lit
            (r'"[^"\\\n]*"', String), # single token in simple cases
            (r'"', String, 'string'),
            # operators
            (r'(<<=|>>=|<<|>>|<=|>=|&\^=|&\^|\+=|-=|\*=|/=|%=|&=|\|=|\^=|&&|\|\|'
             r'|<-|\+\+|--|==|!=|:=|[+\-*/%&!=<>|^])', Operator),
            # punctuation
            (r'(\.\.\.|[()\[\]{}.,;:~])', Punctuation),
            # identifier
            (r'[^\W\d]\w*', Name.Other),
        ],
        'string': [
            (r'\\([0-7]{3}|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}|["\\abfnrtv])', String.Escape),
            (r'[^"\\\n]*"', String, '#pop'),
            (r'[^"\\\n]+', String)
        ]
    }
