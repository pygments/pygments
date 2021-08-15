# -*- coding: utf-8 -*-
"""
    pygments.lexers.maxima
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the computer algebra system Maxima.

    Derived from pygments/lexers/algebra.py.
"""

import re

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

__all__ = ['MaximaLexer']

class MaximaLexer(RegexLexer):
    """
    A `Maxima <http://maxima.sourceforge.net>`_ lexer.
    Derived from pygments.lexers.MuPADLexer.
    """
    name = 'Maxima'
    aliases = ['maxima', 'macsyma']
    filenames = ['*.mac', '*.max']

    terminated = r'(?=[ "()\'\n,;`])'  # whitespace or terminating macro characters

    tokens = {
        'root': [
            (r'/\*', Comment.Multiline, 'comment'),
            (r'"(?:[^"\\]|\\.)*"', String),
            (r'\(|\)|\[|\]|\{|\}', Punctuation),
            (r'''(?x)\b(?:
                if|then|else|elseif|
                do|while|
                repeat|until|
                for|from|to|downto|step|thru
              )\b''', Keyword),
            (r'''(?x)\b(?:
                %pi|%e|%phi|%gamma|%i|
                und|ind|infinity|inf|minf|
                true|false|unknown
              )\b''',
             Name.Constant),
            (r'\.|:|=|#|\+|-|\*|/|\^|@|>|<|\||!|\'|~=', Operator),
            (r'''(?x)\b(?:
                and|or|not
              )\b''',
             Operator.Word),
            (r'''(?x)
              ((?:[a-zA-Z_#][\w#]*|`[^`]*`)
              (?:::[a-zA-Z_#][\w#]*|`[^`]*`)*)(\s*)([(])''',
             bygroups(Name.Function, Text, Punctuation)),
            (r'''(?x)
              (?:[a-zA-Z_#%][\w#%]*|`[^`]*`)
              (?:::[a-zA-Z_#%][\w#%]*|`[^`]*`)*''', Name.Variable),
            (r'[-+]?\d+\.?' + terminated, Number.Integer),
            (r'[-+]?(\d*\.\d+([bdefls][-+]?\d+)?|\d+(\.\d*)?[bdefls][-+]?\d+)'
                + terminated, Number.Float),
            (r'.', Text)
        ],
        'comment': [
            (r'[^*/]', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline)
        ]
    }
