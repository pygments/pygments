# -*- coding: utf-8 -*-
"""
    pygments.lexers.terraform
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for Terraform tf files

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer, RegexLexer, bygroups, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, Punctuation, Number
__all__ = ['TerraformLexer']

class TerraformLexer(RegexLexer):
    """
    Lexer for terraformi .tf files: https://www.terraform.io/

    """
    name = 'Terraform'
    aliases = ['terraform', 'tf']
    filenames = ['*.tf']
    mimetypes = ['application/x-tf', 'application/x-terraform']
    tokens = {
       'root': [
            include ('string'),
            include ('punctuation'),
            include ('curly'),
            include('basic'),
            include('whitespace'),
            (r'[0-9]+', Number),
       ],
       'basic': [
            (r'\b(true|false)\b', Keyword.Type),
            (r'\s*/\*', Comment.Multiline, 'comment'),
            (r'\s*#.*\n', Comment.Single),
            (r'(.*?)(\s*)(=)',
             bygroups(Name.Attribute, Text, Operator)),
            (r'\b(variable|resource|provider|provisioner|module)\b',Keyword.Reserved,'function'),
            (r'\b(ingress|egress|listener|default|connection)\b',Keyword.Declaration),
            ('\$\{',String.Interpol,'var_builtin'),
       ],
       'function':[
            (r'(\s+)(".*")(\s+)', bygroups(Text,String,Text)),
            include ('punctuation'),
            include ('curly'),
        ],
        'var_builtin':[
                (r'\$\{', String.Interpol, '#push'),
                (r'\b(concat|file|join|lookup|element)\b',Name.Builtin),
                include ('string'),
                include ('punctuation'),
                (r'\s+', Text),
                (r'\}',String.Interpol, '#pop'),
         ],
       'string':[
           (r'(".*")',bygroups(String.Double)),
        ],
        'punctuation':[
            (r'[\[\]\(\),.]',Punctuation),
         ],
        # Keep this seperate from punctuation - we sometimes want to use different
        # Tokens for { }
       'curly':[
            (r'\{',Text.Punctuation),
            (r'\}',Text.Punctuation),
        ],
       'comment': [
            (r'[^*/]', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline)
        ],
       'whitespace': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text),
        ],
    }
