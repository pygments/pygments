# -*- coding: utf-8 -*-
"""
    pygments.lexers.boogie
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for esoteric languages.

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, words
from pygments.token import Comment, Operator, Keyword, Name, Number, \
    Punctuation, Whitespace

__all__ = ['BoogieLexer',]



class BoogieLexer(RegexLexer):
    """
    For `Boogie <https://boogie.codeplex.com/>`_ source code.

    .. versionadded:: 2.1
    """
    name = 'Boogie'
    aliases = ['boogie']
    filenames = ['*.bpl']

    tokens = {
        'root': [
            # Whitespace and Comments
            (r'\n', Whitespace),
            (r'\s+', Whitespace),
            (r'//[/!](.*?)\n', Comment.Doc),
            (r'//(.*?)\n', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment'),

            (words((
                'axiom', 'break', 'call', 'ensures', 'else', 'exists', 'function',
                'forall', 'if', 'invariant', 'modifies', 'procedure',  'requires',
                'then', 'var', 'while'),
             suffix=r'\b'), Keyword),
            (words(('const',), suffix=r'\b'), Keyword.Reserved),

            (words(('bool', 'int', 'ref'), suffix=r'\b'), Keyword.Type),
            include('numbers'),
            (r"(>=|<=|:=|!=|==>|&&|\|\||[+/\-=>*<\[\]])", Operator),
            (r"([{}():;,.])", Punctuation),
            # Identifier
            (r'[a-zA-Z_]\w*', Name),
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
        'numbers': [
            (r'[0-9]+', Number.Integer),
        ],
    }
