"""
    pygments.lexers.fift
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for fift.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, words
from pygments.token import Literal, Comment, Name, String, \
    Number, Whitespace

__all__ = ['FiftLexer']


class FiftLexer(RegexLexer):
    """
    For Fift source code.
    """

    name = 'Fift'
    aliases = ['fift', 'fif']
    filenames = ['*.fif']

    tokens = {
        'root': [
            (r'\s+', Whitespace),

            include('comments'),

            # allow escapes in strings
            (r'\"([^\"\r\n\\]|\\.)*\"', String),

            # print string literal
            (r'\.\"([^\"\r\n\\]|\\.)*\"', String),

            # numbers
            (r'-?[0-9]+("/"-?[0-9]+)?', Number.Dec),
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'0[bB][01]+', Number.Bin),

            # slices
            (r'b\{[01]+}', Literal),
            (r'x\{[0-9a-fA-F_]+}', Literal),

            # byte literal
            (r'B\{[0-9a-fA-F_]+}', Literal),

            # treat anything as word
            (r'[^\s]+', Name)
        ],

        'comments': [
            (r'//.*', Comment.Singleline),
            (r'/\*', Comment.Multiline, 'comment'),
        ],
        'comment': [
            (r'[^/*]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
    }
