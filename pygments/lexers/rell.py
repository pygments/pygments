"""
    pygments.lexers.rell
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for the Rell language.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
""" 

import re
from pygments.lexer import RegexLexer
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace, Other, Generic

__all__ = ['RellLexer']

class RellLexer(RegexLexer):
    """
    A Lexer for Rell.
    """
    name = 'Rell'
    url = 'https://docs.chromia.com/rell/rell-intro'
    aliases = ['rell']
    filenames = ['*.rell']
    mimetypes = ['text/x-rell']
    version_added = '2.19.2'

    tokens = {
        'root': [
            (r'(big_integer|boolean|byte_array|decimal|gtv|integer|json|list|'
             r'map|mutable|set|text|virtual)\b',
             Keyword.Type),
            (r'(false|true|null)\b', Keyword.Constant),
            (r'(entity|enum|function|namespace|object|operation|query|'
             r'struct)\b', Keyword.Declaration),
            (r'(abstract|and|break|continue|create|delete|else|for|if|import|'
             r'in|index|key|limit|module|not|offset|or|override|return|update|'
             r'val|var|when|while)\b', Keyword.Reserved),
            (r'//.*?$', Comment.Single),
            (r'/\*(.|\n|\r)*?\*/', Comment.Multiline),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r'\'(\\\\|\\\'|[^\\\'])*\'', String.Single),
            (r'-?[0-9]*\.[0-9]+([eE][+-][0-9]+)?', Number.Float),
            (r'-?[0-9]+([eE][+-][0-9]+|[lL])?', Number.Integer),
            (r'x(\'[a-fA-F0-9]*\'|"[a-fA-F0-9]*")', String.Binary),
            (r'[{}():;,.]', Punctuation),
            (r'[ \n\t\r]+', Whitespace),
            (r'@[a-zA-Z_][a-zA-Z0-9_]*', Name.Decorator),
            # Not really keywords, but Keyword.Pseudo is more visually distinct
            (r'[~^*!%&\[\]<>|+=/?\-@\$]', Keyword.Pseudo),
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name.Variable),
        ],
    }
