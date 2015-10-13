# -*- coding: utf-8 -*-
"""
    pygments.lexers.supercollider
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for SuperCollider

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, default, using, this
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Other
from pygments.util import get_bool_opt, iteritems
import pygments.unistring as uni

__all__ = ['SuperColliderLexer']

class SuperColliderLexer(RegexLexer):
    """
    For SuperCollider source code.
    """

    name = 'SuperCollider'
    aliases = ['sc', 'supercollider']
    filenames = ['*.sc', '*.scd']
    mimetypes = ['application/supercollider', 'text/supercollider', ]

    flags = re.DOTALL
    tokens = {
        'commentsandwhitespace': [
            (r'\s+', Text),
            (r'<!--', Comment),
            (r'//.*?\n', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline)
        ],
        'slashstartsregex': [
            include('commentsandwhitespace'),
            (r'/(\\.|[^[/\\\n]|\[(\\.|[^\]\\\n])*])+/'
             r'([gim]+\b|\B)', String.Regex, '#pop'),
            (r'(?=/)', Text, ('#pop', 'badregex')),
            (r'', Text, '#pop')
        ],
        'badregex': [
            (r'\n', Text, '#pop')
        ],
        'root': [
            (r'^(?=\s|/|<!--)', Text, 'slashstartsregex'),
            include('commentsandwhitespace'),
            (r'\+\+|--|~|&&|\?|:|\|\||\\(?=\n)|'
             r'(<<|>>>?|==?|!=?|[-<>+*%&\|\^/])=?', Operator, 'slashstartsregex'),
            (r'[{(\[;,]', Punctuation, 'slashstartsregex'),
            (r'[})\].]', Punctuation),
            (r'(for|in|while|do|break|return|continue|switch|case|default|if|else|'
             r'throw|try|catch|finally|new|delete|typeof|instanceof|void|'
             r'this)\b', Keyword, 'slashstartsregex'),
            (r'(var|let|with|function|arg)\b', Keyword.Declaration, 'slashstartsregex'),
            (r'(abstract|boolean|byte|char|class|const|debugger|double|enum|export|'
             r'extends|final|float|goto|implements|import|int|interface|long|native|'
             r'package|private|protected|public|short|static|super|synchronized|throws|'
             r'transient|volatile)\b', Keyword.Reserved),
            (r'(true|false|nil|inf)\b', Keyword.Constant),
            (r'(Array|Boolean|Date|Error|Function|'
             r'Number|Object|Packages|RegExp|String|'
             r'Error|isFinite|isNaN|parseFloat|parseInt|'
             r'super|thisFunctionDef|thisFunction|thisMethod|thisProcess|thisThread|'
             r'this)\b', Name.Builtin),
            (r'[$a-zA-Z_][a-zA-Z0-9_]*', Name.Other),
            (r'\\?[$a-zA-Z_][a-zA-Z0-9_]*', String.Symbol),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+', Number.Integer),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r"'(\\\\|\\'|[^'])*'", String.Single),
        ]
    }
