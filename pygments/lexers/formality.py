# -*- coding: utf-8 -*-
"""
    pygments.lexers.fm
    ~~~~~~~~~~~~~~~~~~~

    Lexer for the Formality programming language.

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words, include
from pygments.token import Comment, Keyword, Name, Number, Punctuation, String, Text

__all__ = ['FormalityLexer']


class FormalityLexer(RegexLexer):
    """
    For `Formality <https://github.com/moonad/formality/>`_ source code.

    """

    name = 'Formality'
    aliases = ['Formality']
    filenames = ['*.fm']
    mimetypes = ['text/x-Formality']

    validName = r'[a-z_][a-zA-Z0-9_\']*'

    # specialName = r'^main '

    builtinOps = (
        '~', '.|', '.&', '^', '.^', '.>>', '.<<', '.<', '.>', '.=',
        '+', '*', '/', '%', '**', '.&', '.|', '.!', ':', '-',
        '->', '=>', '!', '#', '$', '@'
    )

    reservedWords = words((
        'let', 'dup', 'cpy', 'get', 'fst', 'snd', 'import',
        'open', 'if', 'else', 'then', 'case', 'T'
        ), suffix=r'\b')

    tokens = {
        'root': [

            # Comments
            (r'//.*', Comment.Single),

            # Whitespace
            (r'\s+', Text),

            # Strings
            (r'"', String, 'doublequote'),

            # Imports
            (r'^\s*import\s*', Keyword.Namespace, 'imports'),

            # Keywords
            (reservedWords, Keyword.Reserved),

            # Types
            (r'[A-Z]\w*', Keyword.Type),

            # Main
            # (specialName, Keyword.Reserved),

            # Prefix Operators
            (words((builtinOps), prefix=r'\(', suffix=r'\)'), Name.Function),

            # Infix Operators
            (words((builtinOps)), Name.Function),

            # Numbers
            include('numbers'),

            # Variable Names
            (validName, Name.Variable),

            # Parens
            (r'[,()\[\]{},\.|;=<>]', Punctuation),

        ],

        # 'comment': [
        #     (r'\{-', Comment.Multiline, 'comment'),
        #     (r'[^-}]', Comment.Multiline),
        #     (r'-\}', Comment.Multiline, '#pop'),
        # ],

        'doublequote': [
            (r'\\u[0-9a-fA-F]{4}', String.Escape),
            (r'\\[nrfvb\\"]', String.Escape),
            (r'[^"]', String),
            (r'"', String, '#pop'),
        ],

        'imports': [
            (r'\w+(\.\w+)*', Name.Class, '#pop'),
        ],

        'numbers': [
            (r'_?\d+\.(?=\d+)', Number.Float),
            (r'_?\d+', Number.Integer),
        ],

        # 'shader': [
        #     (r'\|(?!\])', Name.Entity),
        #     (r'\|\]', Name.Entity, '#pop'),
        #     (r'.*\n', Name.Entity),
        # ],
    }
