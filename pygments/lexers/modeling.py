# -*- coding: utf-8 -*-
"""
    pygments.lexers.modeling
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for modeling languages.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, using
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

from pygments.lexers.html import HtmlLexer

__all__ = ['ModelicaLexer']


class ModelicaLexer(RegexLexer):
    """
    For `Modelica <http://www.modelica.org/>`_ source code.

    .. versionadded:: 1.1
    """
    name = 'Modelica'
    aliases = ['modelica']
    filenames = ['*.mo']
    mimetypes = ['text/x-modelica']

    flags = re.IGNORECASE | re.DOTALL

    tokens = {
        'whitespace': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'//(\n|(.|\n)*?[^\\]\n)', Comment),
            (r'/(\\\n)?\*(.|\n)*?\*(\\\n)?/', Comment),
        ],
        'statements': [
            (r'"', String, 'string'),
            (r'(\d+\.\d*|\.\d+|\d+|\d.)[eE][+-]?\d+[lL]?', Number.Float),
            (r'(\d+\.\d*|\.\d+)', Number.Float),
            (r'\d+[Ll]?', Number.Integer),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'(true|false|NULL|Real|Integer|Boolean)\b', Name.Builtin),
            (r'([a-z_][\w]*|\'[^\']+\')'
             r'([\[\d,:\]]*)'
             r'(\.([a-z_][\w]*|\'[^\']+\'))+'
             r'([\[\d,:\]]*)', Name.Class),
            (r'(\'[\w\+\-\*\/\^]+\'|\w+)', Name),
            (r'[()\[\]{},.;]', Punctuation),
            (r'\'', Name, 'quoted_ident'),
        ],
        'root': [
            include('whitespace'),
            include('classes'),
            include('functions'),
            include('keywords'),
            include('operators'),
            (r'("<html>|<html>)', Name.Tag, 'html-content'),
            include('statements'),
        ],
        'keywords': [
            (r'(algorithm|annotation|break|connect|constant|constrainedby|'
            r'discrete|each|end|else|elseif|elsewhen|encapsulated|enumeration|'
            r'equation|exit|expandable|extends|'
            r'external|false|final|flow|for|if|import|impure|in|initial\sequation|'
            r'inner|input|loop|nondiscrete|outer|output|parameter|partial|'
            r'protected|public|pure|redeclare|replaceable|stream|time|then|true|'
            r'when|while|within)\b', Keyword),
        ],
        'functions': [
            (r'(abs|acos|acosh|asin|asinh|atan|atan2|atan3|ceil|cos|cosh|'
             r'cross|diagonal|div|exp|fill|floor|getInstanceName|identity|'
             r'linspace|log|log10|matrix|mod|max|min|ndims|ones|outerProduct|'
             r'product|rem|scalar|semiLinear|skew|sign|sin|sinh|size|'
             r'spatialDistribution|sum|sqrt|symmetric|tan|tanh|transpose|'
             r'vector|zeros)\b', Name.Function),
        ],
        'operators': [
            (r'(actualStream|and|assert|backSample|cardinality|change|Clock|'
             r'delay|der|edge|hold|homotopy|initial|inStream|noClock|noEvent|'
             r'not|or|pre|previous|reinit|return|sample|smooth|'
             r'spatialDistribution|shiftSample|subSample|superSample|terminal|'
             r'terminate)\b', Name.Builtin),
        ],
        'classes': [
            (r'(operator)?(\s+)?(block|class|connector|end|function|model|'
             r'operator|package|record|type)(\s+)'
             r'((?!if|for|when|while)[a-z_]\w*|\'[^\']+\')([;]?)',
             bygroups(Keyword, Text, Keyword, Text, Name.Class, Text))
        ],
        'quoted_ident': [
            (r'\'', Name, '#pop'),
            (r'[^\']+', Name), # all other characters
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-f0-9]{2,4}|[0-7]{1,3})',
             String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
        'html-content': [
            (r'<\s*/\s*html\s*>"', Name.Tag, '#pop'),
            (r'.+?(?=<\s*/\s*html\s*>)', using(HtmlLexer)),
        ]
    }
