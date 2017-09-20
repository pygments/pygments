# -*- coding: utf-8 -*-
"""
    pygments.lexers.clean
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Clean language.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import ExtendedRegexLexer, words, include, bygroups
from pygments.token import Comment, Error, Keyword, Literal, Name, Number, \
    Operator, Punctuation, String, Whitespace

__all__ = ['CleanLexer']


class CleanLexer(ExtendedRegexLexer):
    """
    Lexer for the general purpose, state-of-the-art, pure and lazy functional
    programming language Clean (http://clean.cs.ru.nl/Clean).

    .. versionadded: 2.2
    """
    name = 'Clean'
    aliases = ['clean']
    filenames = ['*.icl', '*.dcl']

    keywords = (
        'case', 'ccall', 'class', 'code', 'code inline', 'derive', 'export',
        'foreign', 'from', 'generic', 'if', 'import', 'in', 'infix', 'infixl',
        'infixr', 'instance', 'let', 'of', 'otherwise', 'qualified', 'special',
        'stdcall', 'where', 'with')

    modulewords = ('implementation', 'definition', 'system')

    lowerId = r'[a-z][\w\d`]*'
    upperId = r'[A-Z][\w\d`]*'
    funnyId = r'[~@#\$%\^?!+\-*<>\\/|&=:]+'
    scoreUpperId = r'_' + upperId
    scoreLowerId = r'_' + lowerId

    tokens = {
        'root': [
            include('comments'),
            include('keywords'),
            include('module'),
            include('whitespace'),
            include('literals'),
            include('operators'),
            include('delimiters'),
            include('names'),
        ],
        'whitespace': [
            (r'\s+', Whitespace),
        ],
        'comments': [
            (r'//.*\n', Comment.Single),
            (r'/\*', Comment.Multi, 'comments.in'),
            (r'/\*\*', Comment.Special, 'comments.in'),
        ],
        'comments.in': [
            (r'\*\/', Comment.Multi, '#pop'),
            (r'/\*', Comment.Multi, '#push'),
            (r'[^*/]+', Comment.Multi),
            (r'\*(?!/)', Comment.Multi),
            (r'/', Comment.Multi),
        ],
        'keywords': [
            (words(keywords, prefix=r'\b', suffix=r'\b'), Keyword),
        ],
        'module': [
            (words(modulewords, prefix=r'\b', suffix=r'\b'), Keyword.Namespace),
            (r'\bmodule\b', Keyword.Namespace, 'module.name'),
        ],
        'module.name': [
            include('whitespace'),
            (lowerId, Name.Class, '#pop'),
            (upperId, Name.Class, '#pop'),
            (funnyId, Name.Class, '#pop'),
            (scoreLowerId, Name.Class, '#pop'),
            (scoreUpperId, Name.Class, '#pop'),
        ],
        'literals': [
            (r'\'([^\'\\]|\\(x[\da-fA-F]+|\d+|.))\'', Literal.Char),
            (r'[+~-]?0[0-7]+\b', Number.Oct),
            (r'[+~-]?\d+\.\d+(E[+-]?\d+)?', Number.Float),
            (r'[+~-]?\d+\b', Number.Integer),
            (r'[+~-]?0x[\da-fA-F]+\b', Number.Hex),
            (r'True|False', Literal),
            (r'"', String.Double, 'literals.stringd'),
        ],
        'literals.stringd': [
            (r'[^\\"\n]+', String.Double),
            (r'"', String.Double, '#pop'),
            (r'\\.', String.Double),
            (r'[$\n]', Error, '#pop'),
        ],
        'operators': [
            (r'[-~@#\$%\^?!+*<>\\/|&=:\.]+', Operator),
            (r'\b_+\b', Operator),
        ],
        'delimiters': [
            (r'[,;(){}\[\]]', Punctuation),
            (r'(\')([\w`.]+)(\')',
                bygroups(Punctuation, Name.Class, Punctuation)),
        ],
        'names': [
            (r'\b' + lowerId, Name),
            (scoreLowerId, Name),
            (r'\b' + funnyId, Name.Function),
            (r'\b' + upperId, Name.Class),
            (scoreUpperId, Name.Class),
        ]
    }
