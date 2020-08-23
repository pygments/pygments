# -*- coding: utf-8 -*-
"""
    pygments.lexers.pointless
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for Pointless.

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *

__all__ = ['PointlessLexer']


class PointlessLexer(RegexLexer):
    """
    For `Pointless <https://ptls.dev>`_ source code.

    .. versionadded:: 2.7
    """

    name = 'Pointless'
    aliases = ['pointless']
    filenames = ['*.ptls']

    ops = words([
        "+", "-", "*", "/", "**", "%", "+=", "-=", "*=",
        "/=", "**=", "%=", "|>", "=", "==", "!=", "<", ">",
        "<=", ">=", "=>", "$", "++",
    ])

    keywords = words([
        "if", "then", "else", "where", "with", "cond",
        "case", "and", "or", "not", "in", "as", "for",
        "requires", "throw", "try", "catch", "when", "yield",
    ], suffix=r'\b')

    tokens = {
        'root': [
            (r'[ \n\r]+', Text),
            (r'--.*$', Comment.Single),
            (r'"', String, 'string'),
            (r'[\[\](){}:;,.]', Punctuation),
            (ops, Operator),
            (keywords, Keyword),
            (r'\d+|\d*\.\d+', Number),
            (r'(true|false)\b', Name.Builtin),
            (r'[A-Z][a-zA-Z0-9]*\b', String.Symbol),
            (r'output\b', Name.Variable.Magic),
            (r'(export|import)\b', Keyword.Namespace),
            (r'[a-z][a-zA-Z0-9]*\b', Name.Variable)
        ],
        'string': [
            (r'\\.', String.Escape),
            (r'[^\\"\n]+', String),
            (r'"', String, '#pop')
        ]
    }
