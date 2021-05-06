"""
    pygments.lexers.bdd
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for Bdd (Behaviour Driven Development).

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, words
from pygments.token import Token, Text, Keyword, Punctuation, Number, Comment, Whitespace

__all__ = ['BddLexer']

class BddLexer(RegexLexer):
    """
    Lexer for Bdd.

    grammar: https://www.agilealliance.org/glossary/bdd
    """

    name = 'Bdd'
    aliases = ['bdd']
    filenames = ['*.feature']
    mimetypes = []
    
    structure_keywords = (
        words(('Feature', 'Scenario', 'Given', 'When', 'Then', 
               'Examples', 'And', 'But', 'Scenario Outline', 'Background'), suffix=r'\b'),
        Keyword)

    tokens = {
        'root': [
            # Whitespace
            (r'\s+', Whitespace), 
            (r'\n', Whitespace),
            
            #Punctuation
            #(r'[:\|<>"{}()\[\]]', Punctuation),
            (r'<.*>|".*"|\||:', Token.Punctuation),

            # Comments
            (r'//.*', Comment.Single),

            # Keywords
            structure_keywords,

            # Float numbers
            (r'0x[0-9a-fA-F]+\.[0-9a-fA-F]+([pP][\-+]?[0-9a-fA-F]+)?', Number.Float),
            (r'0x[0-9a-fA-F]+\.?[pP][\-+]?[0-9a-fA-F]+', Number.Float),
            (r'[0-9]+\.[0-9]+([eE][-+]?[0-9]+)?', Number.Float),
            (r'[-+]?[0-9]*\.?[0-9]+', Number.Float),

            # Integer numbers
            (r'0b[01]+', Number.Bin),
            (r'0o[0-7]+', Number.Oct),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+', Number.Integer),

            #(r'.', Text), 
        ],    
    }
