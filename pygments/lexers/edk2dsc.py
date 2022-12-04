"""
    pygments.lexers.edk2dsc
    ~~~~~~~~~~~~~~~~~~~~~~~
    Lexers for edk2dsc.
    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, words
from pygments.token import Literal, Comment, Name, Whitespace

__all__ = ['Edk2DscLexer']


class Edk2DscLexer(RegexLexer):
    """
    For edk2dsc source code.
    """

    name = 'DSC'
    aliases = ['dsc']
    filenames = ['*.dsc']
    url = 'https://edk2-docs.gitbook.io/edk-ii-dsc-specification/'

    tokens = {
        'root': [
            (r'^\s+', Whitespace),
            (r'#.*?$', Comment.Single),

            include('comments'),

            # treat anything as word
            (r'\S+', Name)
        ],

        'comments': [
            (r'^\s*#.*$', Comment),
            (r'\n', Whitespace),
            (r'[^\S\n]+', Whitespace),
        ],
    }
