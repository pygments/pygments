"""
    pygments.lexers.wren
    ~~~~~~~~~~~~~~~~~~~~
    Lexer for Wren.
    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer
from pygments.token import Whitespace, Punctuation, Keyword, Name, Comment, \
    Operator, Number, String

__all__ = ['WrenLexer']

class WrenLexer(RegexLexer):
    name = 'Wren'
    aliases = ['wren']
    filenames = ['*.wren']

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'root': [
            # Whitespace.
            (r'\s+', Whitespace),
            (r'[,\\\[\]{}]', Punctuation),

            # Push a parenthesized state so that we know the corresponding ')'
            # is for a parenthesized expression and not interpolation.
            (r'\(', Punctuation, ('parenthesized', 'root')),

            # In this state, we don't know whether a closing ')' is for a
            # parenthesized expression or the end of an interpolation. So, do
            # a non-consuming match and let the parent state (either
            # 'parenthesized' or 'interpolation') decide.
            (r'(?=\))', Whitespace, '#pop'),

            # Keywords.
            (r'(as|break|class|construct|continue|else|for|foreign|if|import|'
             r'in|is|return|static|super|var|while)\b', Keyword),

            (r'(true|false|null)\b', Keyword.Constant),

            (r'this\b', Name.Builtin),

            # Comments.
            (r'/\*', Comment.Multiline, 'comment'), # Multiline, can nest.
            (r'//.*?$', Comment.Single),            # Single line.
            (r'#.*?(\(.*?\))?$', Comment.Special),  # Attribute or shebang.

            # Names and operators.
            (r'[!%&*+\-./:<=>?\\^|~]+', Operator),
            (r'[a-z][a-zA-Z_0-9]*', Name),
            (r'[A-Z][a-zA-Z_0-9]*', Name.Class),
            (r'__[a-zA-Z_0-9]*', Name.Variable.Class),
            (r'_[a-zA-Z_0-9]*', Name.Variable.Instance),

            # Numbers.
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'\d+(\.\d+)?([eE][-+]?\d+)?', Number.Float),

            # Strings.
            (r'"""', String, 'raw_string'),
            (r'"', String, 'string'),
        ],
        'comment': [
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'.', Comment.Multiline), # All other characters.
        ],
        'raw_string': [
            (r'"""', String, '#pop'),
            (r'"', String),
            (r'"', String, '#pop'),
            (r'.', String), # All characters.
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\[\\%"0abefnrtv]', String.Escape), # Escape.
            (r'\\x[a-fA-F0-9]{2}', String.Escape), # Byte escape.
            (r'\\u[a-fA-F0-9]{4}', String.Escape), # Unicode escape.
            (r'\\U[a-fA-F0-9]{8}', String.Escape), # Long Unicode escape.

            (r'%\(', String.Interpol, ('interpolation', 'root')),
            (r'.', String), # All other characters.
        ],
        'parenthesized': [
            # We only get to this state when we're at a ')'.
            (r'\)', Punctuation, '#pop'),
        ],
        'interpolation': [
            # We only get to this state when we're at a ')'.
            (r'\)', String.Interpol, '#pop'),
        ],
    }
