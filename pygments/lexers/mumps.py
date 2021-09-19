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

    """
    Questions from the author:
    1. When '.' is used to indicate a pass-by-reference parameter, is that an operator or just punctuation?
    """

    name = 'MUMPS'
    aliases = ['Mumps', 'mumps', 'M']
    filenames = ['*.m', '*.mumps', '*.epc', '*.int']
    # Filenames aren't meaningful in M, but some implementations allow file export/import of routines
    # For example, YottaDB would have the source file for "dmex" in "dmex.m"
    flags = re.IGNORECASE

    # Definitions of groups that we implement as regular expressions
    name_re = '[%A-Za-z][A-Za-z0-9]*'
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
            default('routinebody')
            ],
        ###
        # 5 - Metalanguage Description
        ###
        # Lists of things are at least one element, with ',' between
        # List states 'l_*' should default on a comma and a single instance
        'list_comma': [
            # Pop back into the list that defaults comma and element
            (',', Punctuation, '#pop'),
            # Pop over the list state
            default('#pop:2')
            ],
        ###
        # 6 - Routine
        ###
        # 6.1 - Routine head 'routinehead'
        'name': [
            (name_re, Name.Variable, '#pop'),
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
            ( name_re + '(?=\\()', Name.Function, 'formallist'),
            ],
        # 6.2.2 Formal line 'formalline'
        'formallist': [
            ('(\\()(\\))', bygroups(Punctuation, Punctuation), '#pop'),
            ('\\(', Punctuation, 'l_name'),
            ('\\)', Punctuation, '#pop'),
            ],
        # list of 'name' continuation
        'l_name': [
            default(('list_comma', 'name')),
            ],
            #'(' + name_re + ')(,)', bygroups(Name.Variable, Punctuation)),
            # name_re, Name.Variable, '#pop')
            #,
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
        # 8.1.6 - Line reference lineref
        'lineref': [
                include('entryref'),
                include('labelref'),
                ],
        # 8.1.6.1 - Entry reference entryref
        'entryref': [
                ( name_re, Name.Label, ('#pop', 'opt_routineref', 'opt_offset')),
                ],
        'opt_offset': [
                ('\\+', Operator, ('#pop', 'expr')),
                default('#pop'),
                ],
        'opt_routineref': [
                ('(\\^)(' + name_re + ')', bygroups(Punctuation, Name.Namespace), '#pop'),
                default('#pop'),
                ],
        'labelref': [
                ('(^)(' + name_re + ')', bygroups(Punctuation, Name.Namespace), '#pop'),
                ],
        # 8.1.7 - Parameter passing
        'actuallist': [
                ('\\(', Punctuation, ('#pop', 'actuallist_contents'),)
                ],
        'actuallist_contents': [
                ('\\)', Punctuation, '#pop'),
                default('l_actual'),
                ],
        'l_actual': [
                default(('list_comma', 'actual')),
                ],
        'actual': [
                ('\\.', Punctuation, ('#pop', 'actualname')),
                default(('#pop', 'expr')),
                ],
        'actualname': [
                ( name_re, Name.Variable, '#pop'),
                ( '@', Operator, ('#pop', 'expratom')),
                ],
        # 8.2 - Command
        'command': [
                # 8.2.1 - BREAK - no argument syntax
                ('break|b', Keyword, ('#pop', 'noargsp', 'postcond')),
                # 8.2.2 - CLOSE - has specific syntax
                ('close|c', Keyword, ('#pop', 'l_closearg', 'argumentsp', 'postcond')),
                # 8.2.3 - DO
                ('do|d', Keyword, ('#pop', 'l_doargument', 'optargsp', 'postcond')),
                # 8.2.16 - QUIT - single expression, or indirect
                ('quit|q', Keyword, ('#pop', 'expr_or_indirect', 'optargsp', 'postcond')),
                ],
        # 8.2.2 - CLOSE arguments
        'closearg': [
                ('@', Operator, ('#pop', 'expratom')),
                default(('#pop', 'colon_deviceparameters', 'expr')),
                ],
        'colon_deviceparameters': [
                ('(:)(\\()', bygroups(Punctuation, Punctuation), ('#pop', 'deviceparams_paren', 'deviceparam')),
                (':', Punctuation, ('#pop', 'deviceparam')),
                default('#pop'),
                ],
        'deviceparam': [
                #('(' + name_re + ')(=)', bygroups(Name.Variable, Operator),('#pop', 'expr')),
                include('expr'),
                ],
        'deviceparams_paren': [
                (':', Punctuation, 'deviceparam'),
                ('\\)', Punctuation, '#pop'),
                ],
        'l_closearg': [
                default(('list_comma', 'closearg'))
                ],
        # 8.2.3 - DO arguments
        'doargument': [
                ('@', Operator, ('#pop', 'expratom')),
                # Spell out 'labelref' because only they may be followed by actuallist
                # (Otherwise, parsing would be undecideable because entryref allows indirection which introduces parentheses)
                ('(' + name_re + ')?(\\^)(' + name_re + ')', bygroups(Name.Label, Punctuation, Name.Namespace), ('#pop', 'postcond', 'opt_actuallist')),
                ( name_re + '(?=\\()', Name.Label, ('#pop', 'postcond', 'opt_actuallist')),
                default(('#pop', 'postcond', 'entryref')),
                ],
        'l_doargument': [
                default(('list_comma', 'doargument')),
                ],
        'opt_actuallist': [
                include('actuallist'),
                default('#pop'),
                ],
        # 8.2.16 - QUIT arguments
        'expr_or_indirect': [
            ('@', Operator, ('#pop', 'expratom')),
            include('expr')
            ],
        }
