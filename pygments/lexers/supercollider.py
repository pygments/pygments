"""
    pygments.lexers.supercollider
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for SuperCollider

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, words, include
from pygments.token import Whitespace, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Text

__all__ = ['SuperColliderLexer']


class SuperColliderLexer(RegexLexer):
    """
    For SuperCollider source code.
    """

    name = 'SuperCollider'
    url = 'http://supercollider.github.io/'
    aliases = ['supercollider', 'sc']
    filenames = ['*.sc', '*.scd']
    mimetypes = ['application/supercollider', 'text/supercollider']
    version_added = '2.1'

    flags = re.MULTILINE
    
    tokens = {
        'root': [
            include('whitespace'),
            include('comments'),
            include('keywords'),
            include('identifiers'),
            include('strings'),
            include('numbers'),
            include('punctuation'),
            include('operators'),
            include('generic_names'),
            include('catchall'),
        ],
        'whitespace': [
            (r'\s+', Whitespace),
        ],
        'comments': [
            (r'/\*', Comment.Multiline, 'multiline_comment'),
            (r'//.*?$', Comment.Single),
        ],
        'multiline_comment': [
            # Nested multiline comment support
            # https://doc.sccode.org/Reference/Comments.html
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline)
        ],
        'keywords': [
            (words(('var', 'arg', 'classvar'), prefix=r'\b', suffix=r'\b'),
                Keyword.Declaration),
            (words(('true', 'false', 'nil'), prefix=r'\b', suffix=r'\b'),
                Keyword.Constant),
            (words(('const', 'context'), prefix=r'\b', suffix=r'\b'),
                Keyword.Reserved),
        ],
        'identifiers': [
            (r'\b[A-Z]\w*\b', Name.Class),
            (r'~[a-z]\w*\b', Name.Variable),

            (words(('thisFunctionDef', 'thisFunction', 'thisMethod',
                'thisProcess', 'thisThread', 'this'), prefix=r'\b', suffix=r'\b'),
                Name.Builtin.Pseudo),
            
            # Primitives
            # https://doc.sccode.org/Guides/WritingPrimitives.html
            (r'_\w+', Name.Builtin),

            # _ is an argument placeholder in partial application
            # https://doc.sccode.org/Reference/Partial-Application.html
            (r'\b_\b', Name.Builtin.Pseudo),
        ],
        'strings': [
            (r'"', String.Double, 'double_quoted_string'),

            # https://doc.sccode.org/Reference/Literals.html#Symbols
            (r"'(?:[^']|\\[^fnrtv])*'", String.Symbol),
            (r'\\\w+\b', String.Symbol),
            (r'\\', String.Symbol),
            
            # Char literal escape characters
            # https://doc.sccode.org/Reference/Literals.html#Characters
            (r'\$\\[tnfvr\\]', String.Char),
            
            # Char literals (only ASCII characters are legal here)
            # https://doc.sccode.org/Classes/Char.html
            (r'\$[\x00-\x7F]', String.Char),
        ],
        'double_quoted_string': [
            (r'\\.', String.Escape),
            (r'"', String.Double, '#pop'),
            (r'[^\\"]+', String.Double),
        ],
        'numbers': [
            (r'(\binf\b|pi\b)', Number.Float),
            
            # Radix float (12r4A.ABC)
            # https://doc.sccode.org/Reference/Literals.html#Radix
            (r'\d+r[0-9a-zA-Z]+(?:\.[0-9A-Z]+)?', Number.Float),
            
            # Scale degree accidentals
            # https://doc.sccode.org/Reference/Literals.html#Scale%20Degrees
            (r'\d+[sb]\d{1,3}\b', Number.Float),
            (r'\d+[sb]{1,4}\b', Number.Float),

            # Numbers with optional scientific notation (1.2e4, 1E-4)
            (r'\d+\.\d+([eE][+-]?\d+)?[fd]?', Number.Float),
            (r'\d+[eE][+-]?\d+[fd]?', Number.Float),
            
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'\d+', Number.Integer),
        ],
        'punctuation': [
            (r'[{}()\[\],\.;:#^`]+', Punctuation),
            (r'<-', Punctuation), # for list comprehensions
        ],
        'operators': [
            # Binary operators are made with characters from this subset
            # https://doc.sccode.org/Classes/Char.html#*binaryOpCharacters
            (r'[!@%&*\-+=|<>?/]+', Operator),

            # Other operators in word form are matched as Name tokens below,
            # since they are synonymous with method names.

            # Further information:
            # https://doc.sccode.org/Overviews/Operators.html
            # https://doc.sccode.org/Overviews/SymbolicNotations.html
        ],
        'generic_names': [
            (r'[a-z]\w*\b', Name),

            # The rest of the word-character substrings must be generic
            # identifiers - names of methods, parameters, variables...
            # They are assigned the Name token, which is consistent with
            # the behaviour of the SuperCollider IDE.
        ],
        'catchall': [
            (r'[^\s]+', Text), 
        ],
    }