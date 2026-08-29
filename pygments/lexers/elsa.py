"""
    pygments.lexers.elsa
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for the ELSA Interactive Lambda Calculus Evaluator.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import re

from pygments.lexer import RegexLexer, bygroups
from pygments.token import *


__all__ = ['ElsaLexer']


class ElsaLexer(RegexLexer):
    """
    For Elsa source.
    """
    name = 'Elsa'
    url = 'https://github.com/ucsd-progsys/elsa'
    filenames = ['*.lc']
    aliases = ['elsa']
    mimetypes = ['text/x-elsa'] 
    version_added = '2.15' # idk
    

    flags = re.NOFLAG 

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'--.*', Comment),
            (r'{-', Comment, 'comment'),
            (r'[A-Z]+', Keyword),
            (r'eval', Keyword),
            (r'let', Keyword),
            (r'undefined', Keyword),
            (r'(eval\s+)([a-zA-Z]\w*\s+)(:)', bygroups(Keyword, Literal, Operator)),
            (r'(let\s+)([A-Z]+\s+)(=)', bygroups(Keyword, Keyword, Operator)),
            (r'(let\s+)([a-zA-Z]\w*\s+)(=)', bygroups(Keyword, Literal, Operator)),
            (r'\\[a-zA-Z]\w*', Name.Function, 'function'),
            (r'[a-zA-Z]\w*', Name.Variable),
            (r'->', Name.Operator),
            (r'=(a|b|d|~|\*)>', Name.Operator),
            
        ],
        'comment' : [
              # if elsa begins nested multiline comment support, uncomment this
        # doesn't matter tbh
            (r'\s+', Whitespace),
            (r'[^\s\{\}\-]+', Comment),
            (r"{-", Comment, '#push'),
            (r"-}", Comment, '#pop'),          
        ],
        'function' : [
            (r'[a-zA-Z]\w*', Name.Function),
            (r'\\[a-zA-Z]\w*', Name.Function, "#push"),
            (r'->', Name.Operator, "#pop")
        ], 
        
    }
