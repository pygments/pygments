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
from pygments.token import Whitespace, Keyword, Operator, Number, Name, Punctuation, Comment

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

    name_re = '[A-Za-z]+'
    binaryop_re = '<'
    unaryop_re = '[-+]'

    ## Regular expressions
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
	    #('(' + name_re + ')(,)', bygroups(Name.Variable, Punctuation)),
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
            #include('strlit'),
            include('numlit'),
            #include('exfunc'),
            #include('exvar'),
            include('svn'),
            #include('function'),
	    ( unaryop_re, Operator, ('#pop', 'expratom')),
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
            default('#pop')
            ],
        ###
        # 8 - Commands
        ###
        # 8.1 - Command general rules
        # note - we include each 'commandword' here because each has its own sytax
        'command': [
            ('quit|q', Keyword, ('#pop', 'quitarg', 'postcond')),
            ],
        # 8.1.4 - postcond
        'postcond': [
            (':', Operator, 'expr'),
            default('#pop')  
            ],
        # 8.2.16 - QUIT
        'quitarg': [
            ('  ', Whitespace, '#pop'),
            (' ', Whitespace, ('#pop', 'expr_or_indirect')),
            default('#pop')
            ],
        'expr_or_indirect': [
            ('@', Operator, ('#pop', 'expratom')),
            include('expr')
            ],
        }
