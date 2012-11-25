# -*- coding: utf-8 -*-
"""
    pygments.lexers.puppet
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Puppet DSL.

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Text


__all__ = ['PuppetLexer']


class PuppetLexer(RegexLexer):
    name = 'Puppet'
    aliases = ['puppet']
    filenames = ['*.pp']

    tokens = {
        'root': [
            include('comments'),
            include('keywords'),
            include('names'),
            include('numbers'),
            include('operators'),
            include('strings'),

            (r'[]{}:(),;[]', Punctuation),

            # FIXME end of line comments don't work on this rule
            (r'(.*)(include)(\s*)(.*)$',
            bygroups(Text, Keyword, Text, Name.Variable)),

            # FIXME puncuation at the end (such as a comma) is the wrong color
            # And one liners such as this are wrong:
            #  class { 'python-custom': version => '2.7.3' }

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

        'names': [
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
            (r'(.*\s*)({)', bygroups(Name.Attribute, Punctuation)),
            (r'(.*\s*)(\()', bygroups(Name.Attribute, Punctuation)),
        ],

        'numbers': [
            # Stolen from the Python lexer
            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?j?', Number.Float),
            (r'\d+[eE][+-]?[0-9]+j?', Number.Float),
            (r'0[0-7]+j?', Number.Oct),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+j?', Number.Integer)
        ],

        'keywords': [
            (r'(if|else|elsif|case|class|true|false|define)\b', Keyword),
            (r'(inherits|notice|node|realize|import)\b', Keyword),
        ],

        'strings': [
            # TODO FIXME collapse this into less rules

            # Normal, single line quoted strings
            (r'\'(.*?)\'', String),
            (r'"(.*?)"', String),

            # TODO FIXME more than one new line breaks this

            # Multi-line quoted strings
            (r'\'(.*?)\n(.*?)\'', String),
            (r'"(.*?)\n(.*?)"', String),

            # Vars starting with $
            ('\$[a-zA-Z_][a-zA-Z0-9_]*', String),
        ],

    }
