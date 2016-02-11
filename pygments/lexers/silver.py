# -*- coding: utf-8 -*-
"""
    pygments.lexers.esoteric
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for esoteric languages.

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, words
from pygments.token import Text, Comment, Operator, Keyword, Name, Number, \
    Punctuation, Whitespace

__all__ = ['SilverLexer',]


class SilverLexer(RegexLexer):
    """
    For `Silver <https://bitbucket.org/viperproject/silver>`_ source code.

    .. versionadded:: 2.1
    """
    name = 'Silver'
    aliases = ['silver']
    filenames = ['*.sil']

    tokens = {
        'root': [
            # Whitespace and Comments
            (r'\n', Whitespace),
            (r'\s+', Whitespace),
            (r'//[/!](.*?)\n', Comment.Doc),
            (r'//(.*?)\n', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment'),

            (words((
                'result', 'true', 'false', 'null', 'method', 'function',
                'predicate', 'program', 'domain', 'axiom', 'var', 'returns',
                'field', 'define', 'requires', 'ensures', 'invariant',
                'fold', 'unfold', 'inhale', 'exhale', 'new', 'assert',
                'assume', 'goto', 'while', 'if', 'elseif', 'else', 'fresh',
                'constraining', 'Seq', 'Set', 'Multiset', 'union', 'intersection',
                'setminus', 'subset', 'unfolding', 'in', 'old', 'forall', 'exists',
                'acc', 'wildcard', 'write', 'none', 'epsilon', 'perm', 'unique'),
             suffix=r'\b'), Keyword),
            (words(('Int', 'Perm', 'Bool', 'Ref'), suffix=r'\b'), Keyword.Type),
            include('numbers'),

            (r'[!%&*+=|?:<>/-]', Operator),
            (r"([{}():;,.])", Punctuation),
            # Identifier
            (r'[a-zA-Z_$0-9]\w*', Name),
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
