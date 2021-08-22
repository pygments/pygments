"""
    pygments.lexers.maxima
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the computer algebra system Maxima.

    Derived from pygments/lexers/algebra.py.

    :license: BSD, see LICENSE for details.
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

    keywords = ('if', 'then', 'else', 'elseif',
                'do', 'while', 'repeat', 'until',
                'for', 'from', 'to', 'downto', 'step', 'thru')

    constants = ('%pi', '%e', '%phi', '%gamma', '%i',
                 'und', 'ind', 'infinity', 'inf', 'minf',
                 'true', 'false', 'unknown', 'done')

    operators = (r'.', r':', r'=', r'#',
                 r'+', r'-', r'*', r'/', r'^',
                 r'@', r'>', r'<', r'|', r'!', r"'")

    operator_words = ('and', 'or', 'not')

    tokens = {
        'root': [
            (r'/\*', Comment.Multiline, 'comment'),
            (r'"(?:[^"\\]|\\.)*"', String),
            (r'\(|\)|\[|\]|\{|\}', Punctuation),
            (r'[,;$]', Punctuation),
            (words (constants), Name.Constant),
            (words (keywords), Keyword),
            (words (operators), Operator),
            (words (operator_words), Operator.Word),
            (r'''(?x)
              ((?:[a-zA-Z_#][\w#]*|`[^`]*`)
              (?:::[a-zA-Z_#][\w#]*|`[^`]*`)*)(\s*)([(])''',
             bygroups(Name.Function, Text.Whitespace, Punctuation)),
            (r'''(?x)
              (?:[a-zA-Z_#%][\w#%]*|`[^`]*`)
              (?:::[a-zA-Z_#%][\w#%]*|`[^`]*`)*''', Name.Variable),
            (r'[-+]?\d+\.?' + terminated, Number.Integer),
            (r'[-+]?(\d*\.\d+([bdefls][-+]?\d+)?|\d+(\.\d*)?[bdefls][-+]?\d+)'
                + terminated, Number.Float),
            (r'\s+', Text.Whitespace),
            (r'.', Text)
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline)
        ]
    }
