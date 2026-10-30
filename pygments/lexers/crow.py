"""
    pygments.lexers.crow
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for the Crow programming language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include, words
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Whitespace

__all__ = ['CrowLexer']


class CrowLexer(RegexLexer):
    """
    Lexer for the Crow programming language. Calls are written with square
    brackets, comparisons are words (``eq``, ``lt``), comments start with
    ``---``, and a single-quoted string interpolates the code between
    backticks.
    """

    name = 'Crow'
    url = 'https://github.com/agncrow/crow'
    filenames = ['*.co']
    aliases = ['crow']
    mimetypes = ['text/x-crow']
    version_added = '2.22'

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'---.*?$', Comment.Single),
            (r"'", String.Single, 'string'),
            # a column named with an interpolation: df.lag_`k`
            (r'`', String.Interpol, 'interpolation'),
            (r'\d+\.\d+([eE][+-]?\d+)?', Number.Float),
            (r'\d+([eE][+-]?\d+)?', Number.Integer),
            (r'(fn|view|derive)(\s+)([A-Za-z_]\w*)',
             bygroups(Keyword.Declaration, Whitespace, Name.Function)),
            (r'(struct)(\s+)([A-Za-z_]\w*)',
             bygroups(Keyword.Declaration, Whitespace, Name.Class)),
            (r'^(test)(?=\s*(?:[A-Za-z_]\w*\s*)?\[)', Keyword.Declaration),
            (r'(use)\b', Keyword.Namespace, 'use'),
            (words(('if', 'else', 'for', 'in', 'while', 'return', 'break',
                    'continue', 'then'), suffix=r'\b'), Keyword),
            (words(('and', 'or', 'not', 'eq', 'ne', 'gt', 'lt', 'gte', 'lte'),
                   suffix=r'\b'), Operator.Word),
            (words(('true', 'false'), suffix=r'\b'), Keyword.Constant),
            (r'[A-Za-z_]\w*(?=\[)', Name.Function),
            (r'[A-Za-z_]\w*', Name),
            (r'\.[*/^]|[-+*/%^=]', Operator),
            (r'[\[\](),.:]', Punctuation),
        ],
        'string': [
            (r"\\[ntr'`\\]", String.Escape),
            (r'`', String.Interpol, 'interpolation'),
            (r"[^'`\\]+", String.Single),
            (r'\\', String.Single),
            (r"'", String.Single, '#pop'),
        ],
        # code between backticks; a string inside it is quoted with \'
        'interpolation': [
            (r'`', String.Interpol, '#pop'),
            (r"\\'", String.Single, 'nested-string'),
            include('root'),
        ],
        'nested-string': [
            (r"\\\\\\'", String.Escape),
            (r"\\'", String.Single, '#pop'),
            (r'\\[ntr`\\]', String.Escape),
            (r'`', String.Interpol, 'interpolation'),
            (r'[^\\`]+', String.Single),
            (r'\\', String.Single),
        ],
        'use': [
            (r'[ \t]+', Whitespace),
            (r'as\b', Keyword.Namespace),
            (r'[A-Za-z_][\w/]*', Name.Namespace),
            (r',', Punctuation),
            (r'', Whitespace, '#pop'),
        ],
    }
