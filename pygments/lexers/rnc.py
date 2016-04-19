# -*- coding: utf-8 -*-
"""
    pygments.lexers.rnc
    ~~~~~~~~~~~~~~~~~~~

    Lexer for Relax-NG Compact syntax

    :copyright: Copyright 2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Punctuation

__all__ = ['RNCCompactLexer']


class RNCCompactLexer(RegexLexer):
    """
    For `RelaxNG-compact <http://relaxng.org>`_ syntax.
    """

    name = 'Relax-NG Compact'
    aliases = ['rnc', 'rng-compact']
    filenames = ['*.rnc']

    tokens = {
        'root': [
            (r'namespace', Keyword.Namespace),
            (r'(default|datatypes)', Keyword.Declaration),
            (r'##.*$', Comment.Preproc),
            (r'#.*$', Comment.Single),
            (r'"[^"]*"', String.Double),
            (r'(element|attribute|mixed)', Keyword.Declaration, 'variable'),
            (r'(text|xsd:[^ ]+)', Keyword.Type, 'maybe_xsdattributes'),
            (r'[,?&*=|]', Operator),
            (r'[(){}]', Punctuation),
            (r'.', Text),
            ],

        # a variable has been declared using `element` or `attribute`
        'variable': [
            (r'[^{]+', Name.Variable),
            (r'\{', Punctuation, '#pop'),
            ],

        # after an xsd:<datatype> declaration there may be attributes
        'maybe_xsdattributes': [
            (r'\{', Punctuation, 'xsdattributes'),
            (r'\}', Punctuation, '#pop'),
            (r'.', Text),
            ],

        # attributes take the form { key1 = value1 key2 = value2 ... }
        'xsdattributes': [
            (r'[^ =}]', Name.Attribute),
            (r'=', Operator),
            (r'"[^"]*"', String.Double),
            (r'\}', Punctuation, '#pop'),
            (r'.', Text),
            ]
    }
