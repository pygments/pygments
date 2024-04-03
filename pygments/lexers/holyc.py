"""
    pygments.lexers.holyc
    ~~~~~~~~~~~~~~~~~~

    Lexers for the HolyC language.

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace

__all__ = ['HolyCLexer']


class HolyCLexer(RegexLexer):
    """
    For HolyC source.
    """
    name = 'HolyC'
    url = 'https://templeos.org/'
    filenames = ['*.HC', '*.hc', '*.ZC', '*.zc']
    aliases = ['hc', 'holyc']
    mimetypes = ['text/x-holycsrc']
    version_added = '1.2'

    tokens = {
        'root': [
            (r'#.*?\n', Comment.Preproc),
            (r'\n', Whitespace),
            (r'\s+', Whitespace),
            (r'//(.*?)$', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
            (r'(class|enum|union)\b',
             Keyword.Declaration),
            (words((
                'break', 'default', 'select', 'case', 'defer', 'go',
                'else', 'goto', 'switch', 'fallthrough', 'if', 'range',
                'continue', 'for', 'return'), suffix=r'\b'),
             Keyword),
            (words((
                'U8', 'I8', 'U16', 'I16', 'U32', 'I32', 'U64', 'I64', 'F64'
                'U8i', 'I8i', 'U16i', 'I16i', 'U32i', 'I32i', 'U64i', 'I64i', 'F64i'
                ), suffix=r'\b'),
             Keyword.Type),
            # float_lit
            (r'\d+(\.\d+[eE][+\-]?\d+|'
             r'\.\d*|[eE][+\-]?\d+)', Number.Float),
            (r'\.\d+([eE][+\-]?\d+)?', Number.Float),
            # int_lit
            # -- octal_lit
            (r'0[0-7]+', Number.Oct),
            # -- hex_lit
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            # -- decimal_lit
            (r'(0|[1-9][0-9]*)', Number.Integer),
            # char_lit
            (r"""'(\\['"\\abfnrtv]|\\x[0-9a-fA-F]{2}|\\[0-7]{1,3}"""
             r"""|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}|[^\\])'""",
             String.Char),
            # StringLiteral
            # -- interpreted_string_lit
            (r'"(\\\\|\\[^\\]|[^"\\])*"', String),
            # Tokens
            (r'(<<=|>>=|<<|>>|<=|>=|&\^=|&\^|\+=|-=|\*=|/=|%=|&=|\|=|&&|\|\|'
             r'|<-|\+\+|--|==|!=|:=|\.\.\.|[+\-*/%&]'
             r'|~|\|)', Operator),
            (r'[|^<>=!()\[\]{}.,;:]', Punctuation),
            # identifier
            (r'[^\W\d\@][\@\w]*', Name.Other),
            (r'(TRUE|FALSE|ON|OFF|NULL)\b', Name.Builtin),
        ]
    }
