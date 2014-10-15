# -*- coding: utf-8 -*-
"""
    pygments.lexers.resource
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for resource definition files.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import Comment, String, Number, Operator, Text, Keyword, \
    Name, String

__all__ = ['ResourceLexer']


class ResourceLexer(RegexLexer):
    name = 'ResourceBundle'
    aliases = ['resource', 'resourcebundle']
    filenames = ['*.txt']

    _types = (':table', ':array', ':string', ':bin', ':import', ':intvector',
              ':int', ':alias')

    flags = re.MULTILINE | re.IGNORECASE
    tokens = {
        'root': [
            (r'//.*?$', Comment),
            (r'"', String, 'string'),
            (r'-?\d+', Number.Integer),
            (r'[,{}]', Operator),
            (r'([^\s{:]+)(\s*)(%s?)' % '|'.join(_types),
             bygroups(Name, Text, Keyword)),
            (r'\s+', Text),
            (words(_types), Keyword),
        ],
        'string': [
            (r'(\\x[0-9a-fA-F]{2}|\\u[0-9a-fA-F]{4}|\\U00[0-9a-fA-F]{6}|'
             r'\\[0-7]{1,3}|\\c.|\\[abtnvfre\'"?\\]|\\{|[^"{\\])+', String),
            (r'\{', String.Escape, 'msgname'),
            (r'"', String, '#pop')
        ],
        'msgname': [
            (r'([^{},]+)(\s*)', bygroups(Name, String.Escape), ('#pop', 'message'))
        ],
        'message': [
            (r'\{', String.Escape, 'msgname'),
            (r'\}', String.Escape, '#pop'),
            (r'(,)(\s*)([a-zA-Z]+)(\s*})',
             bygroups(Operator, String.Escape, Keyword, String.Escape), '#pop'),
            (r'(,)(\s*)([a-zA-Z]+)(\s*)(,)(\s*)(offset)(\s*)(:)(\s*)(-?\d+)(\s*)',
             bygroups(Operator, String.Escape, Keyword, String.Escape, Operator,
                      String.Escape, Operator.Word, String.Escape, Operator,
                      String.Escape, Number.Integer, String.Escape), 'choice'),
            (r'(,)(\s*)([a-zA-Z]+)(\s*)(,)(\s*)',
             bygroups(Operator, String.Escape, Keyword, String.Escape, Operator,
                      String.Escape), 'choice'),
            (r'\s+', String.Escape)
        ],
        'choice': [
            (r'(=|<|>|<=|>=|!=)(-?\d+)(\s*{)',
             bygroups(Operator, Number.Integer, String.Escape), 'message'),
            (r'([a-zA-Z]+)(\s*{)', bygroups(Keyword.Type, String.Escape), 'str'),
            (r'\}', String.Escape, ('#pop', '#pop')),
            (r'\s+', String.Escape)
        ],
        'str': [
            (r'\}', String.Escape, '#pop'),
            (r'\{', String.Escape, 'msgname'),
            (r'[^{}]+', String)
        ]
    }

    def analyse_text(text):
        return text.startswith('root:table')
