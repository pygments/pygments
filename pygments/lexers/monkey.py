# -*- coding: utf-8 -*-
"""
    pygments.lexers.monkey
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the monkey language

    :copyright: Copyright 2012
    :license: BSD, see LICENSE for details.
"""
import re

from pygments.lexer import RegexLexer, DelegatingLexer, bygroups, include, \
     using, this
from pygments.token import Punctuation, \
     Text, Comment, Operator, Keyword, Name, String, Number, Literal, Other
from pygments.util import get_choice_opt
from pygments import unistring as uni

from pygments.lexers.web import XmlLexer

__all__ = ['MonkeyLexer']


class MonkeyLexer(RegexLexer):
    """
    For
    `Monkey <https://en.wikipedia.org/wiki/Monkey_(programming_language)>`_
    source code.
    """

    name = 'Monkey'
    aliases = ['monkey']
    filenames = ['*.monkey']
    mimetypes = [] # (?)

    tokens = {
        'root': [
            (r'\n', Text), 
            (r'\s+', Text),
            (r"[\s\t]*'.*?$", Comment),
            (r'^#rem\s(.|\n)*?\n#end$', Comment.Multiline),
            (r'#If\s.*?|#ElseIf\s.*?|#ElseIf\s.*?|#End|#Print\s.*?|#Error\s.*?', Comment.Preproc),
            (r'(?<!\.)(Int|Float|String|Bool|Object|Array|Void)\b', Keyword.Type),
            (r'(\d+\.\d*|\d*\.\d+)([fF][+-]?[0-9]+)?', Number.Float),
            (r'\d+([SILDFR]|US|UI|UL)?', Number.Integer),
            (r'&H[0-9a-f]+([SILDFR]|US|UI|UL)?', Number.Integer),
            (r'&O[0-7]+([SILDFR]|US|UI|UL)?', Number.Integer),
            (r'(Import)\s+', Keyword.Namespace),
            (r'(Strict|Import|End|Return|If|Else)\s+', Keyword.Declaration),
            (r'(?<!\.)(Function|Method)(\s+)', bygroups(Keyword, Text), 'funcname'),
            (r'(?<!\.)(Class)(\s+)', bygroups(Keyword, Text), 'classname'),
            (r'New\s+', Keyword.Declaration, 'classname'),
            (r'(?<!\.)(Local|Field)(\s+)', Keyword, 'variables'),
            (r'[a-zA-Z_][a-zA-Z0-9_]*[%&@!#$]?', Name),
            (r'&=|[*]=|/=|\\=|\^=|\+=|-=|<<=|>>=|<<|>>|:=|'
             r'<=|>=|<>|[-&*/\\^+=<>]',
             Operator),
            (r'[\(\){}!#,.:]', Punctuation),
            ('"', String, 'string'),
            #(r'[A-Z][a-zA-Z0-9_]+(?=\()', Name.Function),
            (r'[a-zA-Z_]\w*', Name.Other),
        ],
        'funcname': [
            (r'[A-Z][a-zA-Z0-9_]*', Name.Function),
            (r':', Punctuation, 'classname'),
            (r'\(', Punctuation, 'variables'),
            (r'\)', Punctuation, '#pop'),  
        ],
        'classname': [
            (r'[A-Z][a-zA-Z0-9_]*', Name.Class, '#pop'),
        ],
        'variables': [
            (r'[a-z_][a-z0-9_]*?', Name.Variable),
            (r':', Punctuation, 'classname'),
            (r',\s?', Punctuation, '#push'),
            (r'', Text, '#pop'),
        ],
        'string': [
            (r'""', String),
            (r'"C?', String, '#pop'),
            (r'[^"]+', String),
        ],
    }