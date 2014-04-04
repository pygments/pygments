# -*- coding: utf-8 -*-
"""
    pygments.lexers.golo
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for Golo source code.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
#TODO: single quoted string
#TODO: backquote escaped litterals

from pygments.lexer import RegexLexer, bygroups, include, combined
from pygments.token import Text, Comment, Operator, Punctuation, Name,\
                           Keyword, Number, String, Generic

__all__ = ['GoloLexer']


class GoloLexer(RegexLexer):
    """
    For `Golo <http://golo-lang.org/>`_ source code.
    """

    name = 'Golo'
    filenames = ['*.golo']
    aliases = ['golo']

    tokens = {
        'root': [
            (r'[^\S\n]+', Text),

            (r'#.*$', Comment),

            (r'(\^|\.\.\.|:|\?:|->|==|!=|=|\+|-|\*|%|/|<|<=|>|>=|=|\.)',
                Operator),
            (r'(is|isnt|and|or|not|oftype|in|orIfNull)\b', Operator.Word),
            (r'[]{}|(),[]', Punctuation),

            (r'(module|import)(\s+)',
                bygroups(Keyword.Namespace, Text),
                'modname'),
            (r'(let|var)(\s+)',
                bygroups(Keyword.Declaration, Text),
                'varname'),
            (r'(struct)(\s+)',
                bygroups(Keyword.Declaration, Text),
                'structname'),
            (r'(function)(\s+)',
                bygroups(Keyword.Declaration, Text),
                'funcname'),

            (r'(null|true|false)\b', Keyword.Constant),
            (r'(augment|pimp'
             r'|if|else|case|match|return'
             r'|case|when|then|otherwise'
             r'|while|for|foreach'
             r'|try|catch|finally|throw'
             r'|local'
             r'|continue|break)\b', Keyword),

            (r'(map|array|list|set|vector|tuple)(\[)',
                bygroups(Name.Builtin, Punctuation)),
            (r'(print|println|readln|raise|fun'
             r'|asInterfaceInstance)\b', Name.Builtin),
            (r'\b([a-zA-Z_][a-z$A-Z0-9_]*)(\()',
                bygroups(Name.Function, Punctuation)),

            (r'-?[\d_]*\.[\d_]*([eE][+-]?\d[\d_]*)?F?', Number.Float),
            (r'0[0-7]+j?', Number.Oct),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'-?\d[\d_]*L', Number.Integer.Long),
            (r'-?\d[\d_]*', Number.Integer),

            ('[a-zA-Z_][a-z$A-Z0-9_]*', Name),

            (r'"""', String, combined('stringescape', 'triplestring')),
            (r'"', String, combined('stringescape', 'doublestring')),
            (r'----', String.Doc, 'doc'),

        ],

        'funcname': [
            (r'[a-zA-Z_][a-z$A-Z0-9_]*', Name.Function, '#pop'),
        ],
        'modname': [
            (r'[a-zA-Z_][a-z$A-Z0-9._]*\*?', Name.Namespace, '#pop')
        ],
        'structname': [
            (r'[a-zA-Z0-9_.]+\*?', Name.Class, '#pop')
        ],
        'varname': [
            (r'[a-zA-Z_][a-z$A-Z0-9_]*', Name.Variable, '#pop'),
        ],
        'string': [
            (r'[^\\\"\n]+', String),
            (r'[\'"\\]', String)
        ],
        'stringescape': [
            (r'\\([\\abfnrtv"\']|\n|N{.*?}|u[a-fA-F0-9]{4}|'
             r'U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})', String.Escape)
        ],
        'triplestring': [
            (r'"""', String, '#pop'),
            include('string'),
            (r'\n', String),
        ],
        'doublestring': [
            (r'"', String.Double, '#pop'),
            (r'\\\\|\\"|\\n', String.Escape),
            include('string')
        ],
        'doc': [
            (r'----', String.Doc, '#pop'),
            (r'#\s+.*#?$', Generic.Heading),
            (r'##+\s+.*#?$', Generic.Subheading),
            (r'\*[^*]+\*', Generic.Emph),
            (r'_[^_]+_', Generic.Emph),
            (r'\*\*[^*]+\*\*', Generic.Strong),
            (r'__[^_]+__', Generic.Strong),
            #(r'`(.*?)`', using(GoloLexer)),
        ],

    }
