# -*- coding: utf-8 -*-
"""
    pygments.lexers.snobol_lexer
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Just one lone lexer (SNOBOL4)

    :copyright: Copyright 2011 by Martin Harriman
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

__all__ = ['SnobolLexer']

class SnobolLexer(RegexLexer):
    """
    Lexer for the SNOBOL4 programming language

    Recognizes the common ASCII equivalents of the original SNOBOL4 operators.
    Does not require spaces around binary operators.
    """

    name = "Snobol"
    aliases = ["snobol"]
    filenames = ['*.snobol']
    
    tokens = {
        # root state, start of line
        # comments, continuation lines, and directives start in column 1
        # as do labels
        'root': [
            (r'\*.*\n', Comment),
            (r'[\+\.] ', Punctuation, 'statement'),
            (r'-.*\n', Comment),
            (r'END\s*\n', Name.Label, 'heredoc'),
            (r'[A-Za-z\$][\w$]*', Name.Label, 'statement'),
            (r'\s+', Whitespace, 'statement'),
        ],
        # statement state, line after continuation or label
        'statement': [
            (r'\s*\n', Whitespace, '#pop'),
            (r'\s+', Whitespace),
            (r'(?<=[^\w.])(LT|LE|EQ|NE|GE|GT|INTEGER|IDENT|DIFFER|LGT|SIZE|'
             r'REPLACE|TRIM|DUPL|REMDR|DATE|TIME|EVAL|APPLY|OPSYN|LOAD|UNLOAD|'
             r'LEN|SPAN|BREAK|ANY|NOTANY|TAB|RTAB|REM|POS|RPOS|FAIL|FENCE|'
             r'ABORT|ARB|ARBNO|BAL|SUCCEED|INPUT|OUTPUT|TERMINAL)(?=[^\w.])',
             Name.Builtin),
            (r'[A-Za-z][\w\.]*', Name),
            # ASCII equivalents of original operators
            # | for the EBCDIC equivalent, ! likewise
            # \ for EBCDIC negation
            (r'\*\*|[\?\$\.!%\*/#+\-@\|&\\!=]', Operator),
            (r'"[^"]*"', String),
            (r"'[^']*'", String),
            # Accept SPITBOL syntax for real numbers
            # as well as Macro SNOBOL4
            (r'[0-9]+(?=[^\.EeDd])', Number.Integer),
            (r'[0-9]+(\.[0-9]*)?([EDed][-+]?[0-9]+)?', Number.Float),
            # Goto
            (r':', Punctuation, 'goto'),
            (r'[\(\)<>,;]', Punctuation),
        ],
        # Goto block
        'goto': [
            (r'\s*\n', Whitespace, "#pop:2"),
            (r'\s+', Whitespace),
            (r'F|S', Keyword),
            (r'(\()([A-Za-z][\w.]*)(\))',
             bygroups(Punctuation, Name.Label, Punctuation))
        ],
        # everything after the END statement is basically one
        # big heredoc.
        'heredoc': [
            (r'.*\n', String.Heredoc)
        ]
    }

    
