"""
    pygments.lexers.gomod
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for Go module files.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, default, include, words
from pygments.token import Comment, Keyword, Name, Number, Operator, Punctuation, String, Whitespace

__all__ = ['GoModLexer']


class GoModLexer(RegexLexer):
    """For Go module (``go.mod``) files."""

    name = 'Go Module'
    url = 'https://go.dev/ref/mod#go-mod-file'
    aliases = ['gomod', 'go-mod']
    filenames = ['go.mod']
    version_added = '2.22'

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'//[^\n]*', Comment.Single),
            (words(('module', 'go', 'toolchain', 'require', 'exclude',
                    'replace', 'retract', 'godebug', 'tool', 'ignore'),
                   suffix=r'(?=[\s(]|$)'), Keyword, 'directive'),
            default('directive'),
        ],
        'directive': [
            (r'\n', Whitespace, '#pop'),
            (r'\(', Punctuation, 'block'),
            include('common'),
        ],
        'block': [
            (r'\)', Punctuation, '#pop'),
            (r'\n', Whitespace),
            include('common'),
        ],
        'common': [
            (r'[ \t\r]+', Whitespace),
            (r'//[^\n]*', Comment.Single),
            (r'=>', Operator),
            (r'[()\[\],]', Punctuation),
            (r'"', String.Double, 'string'),
            (r'v?\d+(?:\.\d+)+(?:[-+][\w.+-]+)?(?=[\s,)\]]|$)', Number),
            (r'[^\s()\[\],"]+', Name),
        ],
        'string': [
            (r'\\.', String.Escape),
            (r'"', String.Double, '#pop'),
            (r'[^"\\]+', String.Double),
        ],
    }

    def analyse_text(text):
        if re.search(r'^\s*module\s+\S+', text, re.MULTILINE):
            if re.search(r'^\s*go\s+\d+\.\d+', text, re.MULTILINE):
                return 0.8
            return 0.2
