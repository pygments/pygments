# -*- coding: utf-8 -*-
"""
    pygments.lexers.ampl
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for the ampl language. <http://ampl.com/>

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, using, this
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation


__all__ = ['AmplLexer']


class AmplLexer(RegexLexer):
    """
    For AMPL source code.

    .. versionadded:: 2.2
    """
    name = 'Ampl'
    aliases = ['ampl']
    filenames = ['*.run']

    tokens = {
        'root':[
            (r'\n', Text),
            (r'\s+', Text.Whitespace),
            (r'#.*?\n', Comment.Single),
            (r'/[*](.|\n)*?[*]/', Comment.Multiline),
            (r'(call|cd|close|commands|data|delete|display|drop|end|environ|'
             r'exit|expand|include|load|model|objective|option|problem|purge|'
             r'quit|redeclare|reload|remove|reset|restore|shell|show|solexpand|'
             r'solution|solve|update|unload|xref|'
             r'coeff|coef|cover|obj|interval|'
             r'default|from|to|to_come|net_in|net_out|dimen|dimension|'
             r'check|complements|write|end|function|pipe|'
             r'format|if|then|else|in|while|repeat|for)\b', Keyword.Reserved),
            (r'(integer|binary|symbolic|ordered|circular|reversed|IN|INOUT|OUT|LOCAL)',
             Keyword.Type),
            (r'\".*?\"', String.Double),
            (r'\'.*?\'', String.Single),
            (r'[()\[\]{},;:]+', Punctuation),
            (r'\b(\w+)(\.)(astatus|init|init0|lb|lb0|lb1|lb2|lrc|'
             r'lslack|rc|relax|slack|sstatus|status|ub|ub0|ub1|'
             r'ub2|urc|uslack|val)',
             bygroups(Name.Variable, Punctuation, Keyword.Reserved)),
            (r'(set|param|var|arc|minimize|maximize|subject to|s\.t\.|subj to|'
             r'node|table|suffix|read table|write table)(\s+)(\w+)',
             bygroups(Keyword.Declaration, Text, Name.Variable)),
            (r'(param)(\s*)(:)(\s*)(\w+)(\s*)(:)(\s*)((\w|\s)+)',
             bygroups(Keyword.Declaration, Text, Punctuation, Text,
                      Name.Variable, Text, Punctuation, Text, Name.Variable)),
            (r'(let|fix|unfix)(\s*)(\{.*\}|)(\s*)(\w+)',
             bygroups(Keyword.Declaration, Text, using(this), Text, Name.Variable)),
            (r'\b(abs|acos|acosh|alias|'
             r'asin|asinh|atan|atan2|atanh|ceil|ctime|cos|exp|floor|log|log10|'
             r'max|min|precision|round|sin|sinh|sqrt|tan|tanh|time|trunc|Beta|'
             r'Cauchy|Exponential|Gamma|Irand224|Normal|Normal01|Poisson|Uniform|Uniform01|'
             r'num|num0|ichar|char|length|substr|sprintf|match|sub|gsub|print|printf'
             r'next|nextw|prev|prevw|first|last|ord|ord0|card|arity|indexarity)\b',
             Name.Builtin),
            (r'(\+|\-|\*|/|\*\*|=|<=|>=|==|\||\^|<|>|\!|\.\.|:=|\&|\!=|<<|>>)',
             Operator),
            (r'(or|exists|forall|and|in|not|within|union|diff|'
             r'difference|symdiff|inter|intersect|intersection|'
             r'cross|setof|by|less|sum|prod|product|div|mod)',
             Keyword.Reserved), #Operator.Name but not enough emphasized with Operator.Name
            (r'(\d+\.(?!\.)\d*|\.(?!.)\d+)([eE][+-]?\d+)?', Number.Float),
            (r'\d+([eE][+-]?\d+)?', Number.Integer),
            (r'[+-]?Infinity', Number.Integer),
            (r'(\w+|(\.(?!\.)))', Text)
        ]

    }
