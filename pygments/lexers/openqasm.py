# -*- coding: utf-8 -*-
"""
    pygments.lexers.openqasm
    ~~~~~~~~~~~~~~~~~~~

    Lexer for Open Quantum Assembly Language

    :copyright: Copyright 2006-2018 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, default, words
from pygments.token import Keyword, Name, Text, Whitespace, Comment, Number


__all__ = ['OpenQASMLexer']


class OpenQASMLexer(RegexLexer):
    """
    For OpenQASM (Open Quantum Computing Assembly code.
    """
    name = 'OpenQASM'
    aliases = ['openqasm']
    filenames = ['qasm']
    mimetypes = ['text/x-csrc']

    tokens = {
        'root': [
            default('base'),
        ],

        'base': [
            (r'\n', Whitespace),
            (r'\s+', Whitespace),

            ('qreg', Keyword.Declaration),
            ('creg', Keyword.Declaration),
            (words((
                'cx', 'measure', 'if'), suffix=r'\b'),
            Keyword),
            (r'include\b', Keyword, 'incname'),
            (r'//(.*?)\n', Comment.Single),
            (r'gate\b', Keyword, 'gatename'),
            (r'OPENQASM\b', Keyword, 'version'),
        ],
        'incname': [
            (r'\s+', Text),
            (r'[ia-zA-Z_]\w*', Name.Namespace, '#pop'),
            default('#pop'),
        ],
        'gatename': [
            (r'\s+', Text),
            (r'[a-zA-Z_]\w*', Name.Function, '#pop'),
            default('#pop'),
        ],
        'version': [
           (r'[0-9][0-9_]*(\.[0-9_]+[eE][+\-]?[0-9_]+|'
            '\.[0-9_]*(?!\.)|[eE][+\-]?[0-9_]+)', Number.Float),
        ],

    }
