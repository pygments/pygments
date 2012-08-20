# -*- coding: utf-8 -*-
"""
    pygments.lexers.puppet
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Puppet DSL.

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

__all__ = ['PuppetLexer']


class PuppetLexer(RegexLexer):
    name = 'Puppet'
    aliases = ['puppet']
    filenames = ['*.pp']

    tokens = {
        'root': [
            (r'\s*#.*$', Comment),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),

            (r'\s+(\?|<|>|=|\+|-|\/|\*|~|!|\|)\s+', Operator),
            (r'(in|and|or|not)\b', Operator.Word),
            (r'[]{}:(),;[]', Punctuation),

            (r'(.*)(include)(\s*)(.*)$', bygroups(Text, Keyword, Text, Name.Variable)),

            (r'(if|else|elsif|case|class|true|false|define)\b', Keyword),
            (r'(inherits|notice|node|realize|import)\b', Keyword),

            (r'\$[^ ]*', Name.Variable),

            (r'\'(.*?)\'', String),
            (r'"(.*?)"', String),

            (r'(.*?)(\s*)(=>)(\s*)(.*?)$',
             bygroups(Name.Attribute, Text, Operator, Text, String)),

            (r'[^\S\n]+', Text),
            ],
        }
