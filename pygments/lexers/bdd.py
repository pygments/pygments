"""
    pygments.lexers.bdd
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for BDD language.

    :copyright: Copyright 2006-2021 by group 4 of SPI project of TUoA.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer
from pygments.token import Text, Keyword, Punctuation

__all__ = ['BddLexer']


class BddLexer(RegexLexer):

    name = 'BDD'
    aliases = ['bdd']
    filenames = ['*.feature']

    keywords = [
        'Given'
        'When',  
        'Then', 
        'Add',
        'Feature', 
        'Scenario', 
        'Scenario Outline', 
        'Background',
        'Examples',
        'But',       
    ]

    Punctuation = [
        '<',
        '>',
        ':',
        '|',

    ]

    tokens = {
        'root': [
            # Text /get the Text token 
            (r' .*\n', Text),
            (r'.*\n', Text),

            # Keywords /get the Keyword token 

            (words(keywords), Keyword),

            #Punctuation /get the Punctuation 
            
            (words(Punctuation), Punctuation),
        ],
       
    }