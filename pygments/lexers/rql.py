# -*- coding: utf-8 -*-
"""
    pygments.lexers.rql
    ~~~~~~~~~~~~~~~~~~~

    Lexer for RQL the relation query language

    http://www.logilab.org/project/rql
"""

import re

from pygments.lexer import RegexLexer
from pygments.token import Punctuation, \
     Text, Comment, Operator, Keyword, Name, String, Number

__all__ = ['RqlLexer']

class RqlLexer(RegexLexer):
    """
    Lexer for Relation Query Language.
    """

    name = 'RQL'
    aliases = ['rql']
    filenames = ['*.rql']
    mimetypes = ['text/x-rql']

    flags = re.IGNORECASE
    tokens = {
        'root': [
            (r'\s+', Text),
            (r'(DELETE|SET|INSERT|UNION|DISTINCT|WITH|WHERE|BEING|OR'
             r'|AND|NOT|GROUPBY|HAVING|ORDERBY|ASC|DESC|LIMIT|OFFSET'
             r'|TODAY|NOW|TRUE|FALSE|NULL|EXISTS)\b', Keyword),
            (r'[+*/<>=%-]', Operator),
            (r'(Any|is|instance_of|CWEType|CWRelation)\b', Name.Builtin),
            (r'[0-9]+', Number.Integer),
            (r'[A-Z_][A-Z0-9_]*\??', Name),
            (r"'(''|[^'])*'", String.Single),
            (r'"(""|[^"])*"', String.Single),
            (r'[;:()\[\],\.]', Punctuation)
        ],
    }

