"""
    pygments.lexers.go
    ~~~~~~~~~~~~~~~~~~

    Lexers for the Google Go language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, words, using, this
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

    __ident = r'[^\W\d]\w*'

    tokens = {
        'root': [
            (r'\n', Whitespace),
            (r'\s+', Whitespace),
            (r'(\\)(\n)', bygroups(Text, Whitespace)),  # line continuations
            (r'//(.*?)$', Comment.Single),
            (r'/(\\\n)?[*][\s\S]*?[*](\\\n)?/', Comment.Multiline),
            (r'(package)\b', Keyword.Namespace, 'packagename'),
            (r'(import)\b', Keyword.Namespace),
            (r'(func)\b', Keyword.Declaration, 'funcdef'),
            (r'(type)\b', Keyword.Declaration, 'typedef'),
            (r'(var|struct|interface|const)\b',
             Keyword.Declaration),
            (r'(goto|break|continue)(\b\s*)(' + __ident + ')?',
             bygroups(Keyword, Text.Whitespace, Name.Label)),
            (words((
                'break', 'default', 'select', 'case', 'defer', 'go',
                'else', 'switch', 'fallthrough', 'if', 'range',
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
            # imaginary_lit
            (r'\d+i', Number),
            (r'\d+\.\d*([Ee][-+]\d+)?i', Number),
            (r'\.\d+([Ee][-+]\d+)?i', Number),
            (r'\d+[Ee][-+]\d+i', Number),
            # float_lit
            (r'\d+(\.\d+[eE][+\-]?\d+|'
             r'\.\d*|[eE][+\-]?\d+)', Number.Float),
            (r'\.\d+([eE][+\-]?\d+)?', Number.Float),
            # int_lit
            # -- binary_lit
            (r'0[bB](_?[01])+', Number.Bin),
            # -- octal_lit
            (r'0[oO]?(_?[0-7])+', Number.Oct),
            # -- hex_lit
            (r'0[xX](_?[0-9a-fA-F])+', Number.Hex),
            # -- decimal_lit
            (r'(0|[1-9](_?[0-9])*)', Number.Integer),
            # char_lit
            (r"""'(\\['"\\abfnrtv]|\\x[0-9a-fA-F]{2}|\\[0-7]{1,3}"""
             r"""|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}|[^\\])'""",
             String.Char),
            # StringLiteral
            # -- raw_string_lit
            (r'`[^`]*`', String),
            # -- interpreted_string_lit
            (r'"(\\\\|\\[^\\]|[^"\\])*"', String),
            # Tokens
            (r'(&\^=?|<-|<<=?|>>=?|&&|\|\||\+\+|--|[+\-*/%=!<>&|^]=?|:=|~)', Operator),
            (r'(\.\.\.|[()\[\]{}.,;:~])', Punctuation),
            # identifier
            (__ident, Name),
        ],

        'funcdef': [
            (r'\s+', Whitespace),
            (__ident, Name.Function, '#pop'),
            (fr'(\()([^)]+)(\))(?=\s+{__ident})',  # Method syntax
             bygroups(Punctuation, using(this), Punctuation)),
            (r'\(', Punctuation, '#pop'),  # Lambda
        ],

        'packagename': [
            (r'\s+', Whitespace),
            (__ident, Name.Namespace, '#pop'),
        ],

        'typedef': [
            (r'\s+', Whitespace),
            (__ident, Name.Class, '#pop'),
            (r'[{\(]', Punctuation, '#pop'),
        ]
    }
