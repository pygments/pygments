# -*- coding: utf-8 -*-
"""
    pygments.lexers.puppet
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Puppet DSL.

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Comment, Keyword, Name, Operator, Punctuation, \
    String, Text

__all__ = ['PuppetLexer']

## TODO/FIXME
# Multiline variable assignment
# One liners line this don't look right
#   class { 'python-custom': version => '2.7.3' }

class PuppetLexer(RegexLexer):
    name = 'Puppet'
    aliases = ['puppet']
    filenames = ['*.pp']

    tokens = {
        'root': [
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),

            include('comments'),
            include('operators'),

            (r'[]{}:(),;[]', Punctuation),

            (r'(.*)(include)(\s*)(.*)$',
             bygroups(Text, Keyword, Text, Name.Variable)),

            include('keywords'),
            include('strings'),

            (r'\$[^ ]*', Name),

            # FIXME puncuation at the end (such as a comma) is the wrong color
            (r'(.*?)(\s*)(=>)(\s*)(.*?)$',
             bygroups(Name.Attribute, Text, Operator, Text, String)),

            (r'[^\S\n]+', Text),
            ],

        'comments': [
            (r'\s*#.*$', Comment),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
            ],

        'operators': [
            (r'(=>|\?|<|>|=|\+|-|\/|\*|~|!|\|)', Operator),
            (r'\s+(in|and|or|not)\s+', Operator.Word),
            ],

        'keywords': [
            (r'(if|else|elsif|case|class|true|false|define)\b', Keyword),
            (r'(inherits|notice|node|realize|import)\b', Keyword),
            ],

        'strings': [
            (r'\'(.*?)\'', String),
            (r'"(.*?)"', String),
            ],

        }
