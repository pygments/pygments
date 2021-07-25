# -*- coding: utf-8 -*-
"""
    pygments.lexers.mumps
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for MUMPS

    :copyright: Copyright 2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
from pygments.lexer import ExtendedRegexLexer, words, bygroups, default, include
from pygments.token import Whitespace, Keyword, Operator, Number, Name, Punctuation, Comment, String

__all__ = ['MumpsLexer']

class MumpsLexer(ExtendedRegexLexer):
    """
    For MUMPS source code.
    
    Derived from `The Annotated M[UMPS] Standard <http://71.174.62.16/Demo/AnnoStd>`
    Section numbers below refer to the sections on that site, most often from the latest (1995) standard.
    """

    name = 'MUMPS'
    aliases = ['Mumps', 'mumps', 'M']
    filenames = ['*.m', '*.mumps', '*.epc', '*.int']
    # Filenames aren't meaningful in M, but some implementations allow file export/import of routines
    # For example, YottaDB would have the source file for "dmex" in "dmex.m"
    flags = re.IGNORECASE

    # Definitions of groups that we implement as regular expressions
    name_re = '[A-Za-z]+'
    # 7.1.4.1 - String literal 'strlit'
    strlit_re = '"(""|[^"])*"'
    # 7.1.4.11 - Unary operator 'unaryop'
    unaryop_re = '[-+]'
    # 7.2.1 - binaryop
    binaryop_re = '\\*\\*|[-_+*/\\\\#]'
    # 7.2.2 - truthop
    relation_re = '\\]\\]|[<>=\\[\\]]' # 7.2.2.1 - Relational operator 'relation'
    logicalop_re = '[&!]' # 7.2.2.4 - Logical operator 'logicalop'
    truthop_re = relation_re + '|' + logicalop_re

    # Parsing states
    tokens = {
        'root': [
            ###
            # 6 - Routine
            ###
            default('routinebody')
            ],
        # 6.1 - Routine head 'routinehead'
        'name': [
            (name_re, Name, '#pop'),
            ],
        # 6.2 - Routine body 'routinebody'
        'routinebody': [ 
            # Not handled: EOR, special characters
            default('line'),           
            ],
        'line': [
            # 6.2.1 levelline - Label is optional
            ('(' + name_re + ')( +)', bygroups(Name.Label, Whitespace), ('#pop', 'linebody')), 
            (' ', Whitespace, ('#pop', 'linebody')),
            # 6.2.2 formalline
            ('(' + name_re + ')(\\()', bygroups(Name.Function, Punctuation), 'formallist'),
            ],
        # 6.2.2 Formal line 'formalline'
        'formallist': [
            ('\\)', Punctuation, '#pop'),
            default('l_name'),
            ],
        # list of 'name' continuation
        'l_name': [
            ('(' + name_re + ')(,)', bygroups(Name.Variable, Punctuation)),
            ( name_re, Name.Variable, '#pop')
            ],
        # 6.2.3 Label 'label'
        'label': [
            (name_re, Name.Label, '#pop'),
            ],
        # 6.2.5 - Line body 'linebody'
        'linebody': [
	        (';.*', Comment, '#pop'),
            default('commands'),
            ],
        'commands': [
            (' +', Whitespace),
            default('command')
            ],
        'cs_commands': [
            (' ', Whitespace, 'command'),
            default('#pop'),
            ],    
        ###
        # 7 - Expression 'expr'
        ###
        'expr': [
            default(('#pop', 'exprtail', 'expratom')) 
            ],
        # 7.1 - expratom
        'expratom': [
            include('glvn'),
            include('expritem'),
            ],
    	# 7.1.2 - Variable name 'glvn'
    	'glvn': [
                ( name_re, Name.Variable, '#pop'),
    	        ],
        # 7.1.4 - Expression item 'expritem'
        'expritem': [
            include('strlit'),
            include('numlit'),
            #include('exfunc'),
            #include('exvar'),
            include('svn'),
            #include('function'),
            ( unaryop_re, Operator, ('#pop', 'expratom')),
            ],
        # 7.1.4.1 - String literal 'strlit'
        'strlit': [
                ( strlit_re , String, '#pop'),
                ],

        # 7.1.4.2 - Numeric literal 'numlit'
        'numlit': [
                ('[0-9]+', Number, '#pop'),  
                ],
        # 7.1.4.10 - Intrinsic special variable names 'svn'
        'svn': [
            ('\\$QUIT', Keyword, '#pop'),
            ],
        # 7.2 - exprtail
        'exprtail': [
            # TODO
	        ( binaryop_re, Operator, 'expratom'),
            ( '(\')(' + truthop_re + ')', bygroups(Operator, Operator), 'expratom'),
	        ( truthop_re, Operator, 'expratom'),
            ( '\\?', Operator, 'pattern'),
            default('#pop')
            ],
        # 7.2.3 - Pattern match 'pattern'
        'pattern': [
                ( '@', Operator, ('#pop', 'expratom')),
                default(('#pop', 'more_patatoms', 'patatom')),
                ],
        'patatom': [
                # Detect the repcount, then jump to detecting the pattern code
                ( '([0-9]+)(\\.)([0-9]*)', bygroups(Number, Punctuation, Number), ('#pop', 'patatom_choice')),
                ( '(\\.)([0-9]*)', bygroups(Punctuation, Number), ('#pop', 'patatom_choice')),
                ( '[0-9]+', Number, ('#pop', 'patatom_choice')),
                ],
        'more_patatoms': [
                ( '([0-9]+)(\\.?)([0-9]*)', bygroups(Number, Punctuation, Number), 'patatom_choice'),
                ( '(\\.?)([0-9]*)', bygroups(Punctuation, Number), 'patatom_choice'),
                ( '[0-9]+', Number, 'patatom_choice'),
                default('#pop')
                ],
        'patatom_choice': [
                ('\\(', Punctuation, ('#pop', 'alternation', 'patatom')),
                include('strlit'),
                include('patcode'),
                ],
        'patcode': [
                ('[A-Xa-x]+', Name.Entity, '#pop'),
                ('Y[A-XZa-xz]*Y', Name.Entity, '#pop'),
                ('Z[A-Ya-y]*Z', Name.Entity, '#pop'),
                ],
        'alternation': [
                (',', Punctuation, 'patatom'),
                ('\\)', Punctuation, '#pop')
                ],
        ###
        # 8 - Commands
        ###
        # 8.1 - Command general rules
        # 8.1.1 - Spaces in commands
        'argumentsp': [
                (' ', Whitespace, '#pop'),
                ],
        'noargsp': [
                ('  +', Whitespace, '#pop'),
                ],
        'optargsp': [
                ('  +', Whitespace, '#pop:2'),
                include('argumentsp'),
                ],
        # 8.1.4 - postcond
        'postcond': [
            (':', Operator, 'expr'),
            default('#pop')  
            ],
        # 8.2 - Command
        'command': [
                # 8.2.1 - BREAK - no argument syntax
                ('break|b', Keyword, ('#pop', 'noargsp', 'postcond')),
                # 8.2.16 - QUIT
                ('quit|q', Keyword, ('#pop', 'expr_or_indirect', 'optargsp', 'postcond')),
                ],
        'expr_or_indirect': [
            ('@', Operator, ('#pop', 'expratom')),
            include('expr')
            ],
        }
