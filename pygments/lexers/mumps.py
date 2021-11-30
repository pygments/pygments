# -*- coding: utf-8 -*-
"""
    pygments.lexers.mumps
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for MUMPS

    :copyright: Copyright 2020-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.


    :todo: Add LI / LEVELLINE line indent
    :todo: Add more functions
    :todo: Review exprtail
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
    name_re = '[%A-Za-z][A-Za-z0-9]*'
    # 7.1.4.1 - String literal 'strlit'
    strlit_re = '"(""|[^"])*"'
    # 7.1.4.10 - Intrinsic special variables 'svn'
    svn_re = words(('$DEVICE', '$D', '$ECODE', '$EC', '$ESTACK', '$ES', '$ETRAP', '$ET', '$HOROLOG', '$H', '$IO', '$I', '$JOB', '$J', '$KEY', '$K', '$PRINCIPAL', '$P', '$QUIT', '$Q', '$STACK', '$ST', '$STORAGE', '$S', '$SYSTEM', '$SY', '$TEST', '$T', '$TLEVEL', '$TL', '$TRESTART', '$TR', '$X', '$Y'), suffix=r'\b(?!\()')
    # 7.1.4.11 - Unary operator 'unaryop'
    unaryop_re = "[-+']"
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
        'comma': [
            (',', Punctuation, '#pop'),
            ],
        'list_comma': [
            # Pop back into the list that defaults comma and element
            include('comma'),
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
        'routinename': [
            (name_re, Name.Namespace, '#pop'),
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
        # 6.2.3 Label 'label'
        'label': [
            (name_re, Name.Label, '#pop'),
            ],
        # 6.2.5 - Line body 'linebody'
        'linebody': [
            (';.*', Comment, '#pop'),
            include('commands'),
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
        'l_expr': [
                default(('list_comma', 'expr')),
                ],
        # 7.1 - expratom
        'expratom': [
                include('glvn'),
                include('expritem'),
                ],
        # 7.1.2 - Variable name 'glvn'
        'glvn': [
                include('lvn'),
                include('gvn'),
                include('ssvn'),
                ],
        # 7.1.2.1 - Local variable name lvn
        'lvn': [
                include('rlvn'),
                include('namind')
                ],
        'rlvn': [
                ( name_re , Name.Variable, ('#pop', 'opt_subscripts')),
                ],
        # 7.1.2.4 - Global variable name gvn
        'gvn': [
                include('rgvn'),
                include('namind')
                ],
        'rgvn': [
                ('\\^(?=\\()', Name.Variable.Global, ('#pop', 'subscripts')),
                ('\\^(?=\\|)', Name.Variable.Global, ('#pop', 'opt_subscripts', 'gname', 'environment')), 
                ('\\^' + name_re, Name.Variable.Global, ('#pop', 'opt_subscripts')),
                ],
        'environment': [
                ('\\|', Punctuation, ('#pop', 'vert_bar', 'expr')),
                ],
        'vert_bar': [
                ('\\|', Punctuation, '#pop'),
                ],
        'gname': [
                (name_re, Name.Variable.Global, '#pop'),
                ],
        # Parsing for '( L expr )'
        'subscripts': [
                ('\\(', Punctuation, ('#pop', 'close_paren', 'l_expr'))
                ],
        'opt_subscripts': [
                include('subscripts'),
                default('#pop')
                ],
        'open_paren': [
                ('\\(', Punctuation, '#pop'),
                ],
        'close_paren': [
                ('\\)', Punctuation, '#pop'),
                ],
        # Name indirection, common syntax for rlvn and rgvn
        'namind': [
                ('@', Operator, ('#pop', 'namind_subscripts', 'rexpratom'))
                ],
        'namind_subscripts': [
                ('@', Operator, ('#pop', 'subscripts')),
                default('#pop'),
                ],
        'rexpratom': [
                include('rlvn'),
                include('rgvn'),
                include('expritem')
                ],
        # 7.1.3 - Structured system variable ssvn
        'ssvn': [
                ('\\^\\$[A-Z]+', Name.Variable.Magic, ('#pop', 'subscripts')),
                ],
        # 7.1.4 - Expression item 'expritem'
        'expritem': [
            include('strlit'),
            include('numlit'),
            include('exfunc'),
            #include('exvar'),
            include('svn'),
            include('function'),
            ( unaryop_re, Operator, ('#pop', 'expratom')),
            ( '\\(', Punctuation, ('#pop', 'close_paren', 'expr')),
            ],
        # 7.1.4.1 - String literal 'strlit'
        'strlit': [
                ( strlit_re , String, '#pop'),
                ],
        # 7.1.4.2 - Numeric literal 'numlit'
        'numlit': [
                ('[0-9]*\\.[0-9]+E[+-]?[0-9]+', Number, '#pop'),
                ('[0-9]*\\.[0-9]+', Number, '#pop'),
                ('[0-9]+E[+-]?[0-9]+', Number, '#pop'),
                ('[0-9]+', Number, '#pop'),
                ],
        # 7.1.4.8 - Extrinsic function exfunc
        # 7.1.4.9 - Extrinsic special variable exvar - Same syntax, no actuallist
        'exfunc': [
                ('\\$\\$', Punctuation, ('#pop', 'opt_actuallist', 'labelref_func')),
                ('\\$(?=&)', Punctuation, ('#pop', 'opt_actuallist', 'externref_func')),
                ],
        # 7.1.4.10 - Intrinsic special variable names 'svn'
        'svn': [
                (svn_re, Name.Variable.Magic, '#pop'),
                ],
        # 7.1.5 - Intrinsic function function
        'function': [
                (words(('$ASCII', '$A', '$CHAR', '$C', '$EXTRACT', '$E', '$FIND' , '$F'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'l_expr', 'open_paren')),
                (words(('$DATA', '$D'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'glvn', 'open_paren')),
                ],
        # 7.2 - exprtail
        'exprtail': [
                # TODO
                ( '\'', Operator), # The "not" can happen multiple times
                ( binaryop_re, Operator, 'expratom'),
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
        # 8.1.4 - Post Conditional postcond
        'postcond': [
            default(('#pop', 'expr', 'colon_sep'))
            ],
        # 8.1.5 - Command timeout timeout, syntactically the same as postcond but the numeric value is used
        'timeout': [
                default(('#pop', 'expr', 'colon_sep'))
                ],
        # 8.1.6 - Line reference lineref
        'lineref': [
                include('entryref'),
                # The standard says that lineref also has labelref, but every labelref is also a valid entryref
                ],
        # 8.1.6.1 - Entry reference entryref
        'entryref': [
                ( name_re, Name.Label, ('#pop', 'opt_routineref', 'opt_offset')),
                ( '\\^', Punctuation, ('#pop', 'routineref')),
                ( '@', Operator, ('#pop', 'opt_routineref', 'opt_offset', 'expratom')),
                ],
        'opt_offset': [
                ('\\+', Operator, ('#pop', 'expr')),
                default('#pop'),
                ],
        'routineref': [
                ('@', Operator, ('#pop', 'expratom')),
                include('routineref_strict')
                ],
        'routineref_strict': [
                default(('#pop', 'routinename', 'opt_environment'))
                ],
        'opt_routineref': [
                ('\\^', Punctuation, ('#pop', 'routineref')),
                default('#pop'),
                ],
        # 8.1.6.2 - Label reference labelref
        'labelref': [
                ( name_re , Name.Label, ('#pop', 'opt_routineref_strict')),
                ('\\^', Punctuation, ('#pop', 'routineref_strict')),
                ],
        'labelref_func': [
                ( name_re , Name.Function, ('#pop', 'opt_routineref_strict')),
                ('\\^', Punctuation, ('#pop', 'routineref_strict')),
                ],
        'opt_environment': [
                include('environment'),
                default('#pop')
                ],
        'opt_routineref_strict': [
                ('\\^', Punctuation, ('#pop', 'routineref_strict')),
                default('#pop')
                ],
        # 8.1.6.3 - External reference externref
        'externref': [
                ('&', Punctuation, ('#pop', 'labelref', 'opt_packagename'))
                ],
        'externref_func': [
                ('&', Punctuation, ('#pop', 'labelref_func', 'opt_packagename'))
                ],
        'opt_packagename': [
                ('(' + name_re + ')(\\.)', bygroups(Name.Namespace, Punctuation), '#pop'),
                default('#pop')
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
                (words(('break', 'b'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
                # 8.2.2 - CLOSE - has specific syntax
                (words(('close', 'c'), suffix=r'\b'), Keyword, ('#pop', 'l_closearg', 'argumentsp', 'postcond')),
                # 8.2.3 - DO
                (r'do?\b', Keyword, ('#pop', 'l_doargument', 'optargsp', 'postcond')),
                # 8.2.4 - ELSE
                (words(('else', 'e'), suffix=r'\b'), Keyword, ('#pop', 'noargsp')),
                # 8.2.5 - FOR
                (words(('for', 'f'), suffix=r'\b'), Keyword, ('#pop', 'for_argument', 'optargsp')),
                # 8.2.6 - GOTO
                (words(('goto', 'g'), suffix=r'\b'), Keyword, ('#pop', 'l_gotoargument', 'argumentsp', 'postcond')),
                # 8.2.7 - HALT
                (r'halt\b', Keyword, ('#pop', 'noargsp')),
                # 8.2.8 - HANG
                (r'hang\b', Keyword, ('#pop', 'l_expr', 'argumentsp', 'postcond')),
                # Combination halt/hang - both abbreviate to h, so the difference is whether there are arguments
                (r'h\b', Keyword, ('#pop', 'l_expr', 'optargsp', 'postcond')),
                # 8.2.9 - IF
                (r'if?\b', Keyword, ('#pop', 'l_expr', 'optargsp')),
                # 8.2.10 - JOB
                (words(('job', 'j'), suffix=r'\b'), Keyword, ('#pop', 'l_jobargument', 'argumentsp', 'postcond')),
                # 8.2.11 - KILL
                (words(('kill', 'k'), suffix=r'\b'), Keyword, ('#pop', 'l_killargument', 'optargsp', 'postcond')),
                # 8.2.12 - LOCK
                (words(('lock', 'l'), suffix=r'\b'), Keyword, ('#pop', 'l_lockargument', 'optargsp', 'postcond')),
                # 8.2.13 - MERGE
                (words(('merge', 'm'), suffix=r'\b'), Keyword, ('#pop', 'l_mergeargument', 'argumentsp', 'postcond')),
                # 8.2.14 - NEW
                (words(('new', 'n'), suffix=r'\b'), Keyword, ('#pop', 'l_newargument', 'optargsp', 'postcond')),
                # 8.2.15 - OPEN
                (words(('open', 'o'), suffix=r'\b'), Keyword, ('#pop', 'l_openargument', 'argumentsp', 'postcond')),
                # 8.2.16 - QUIT - single expression, or indirect
                (words(('quit', 'q'), suffix=r'\b'), Keyword, ('#pop', 'expr_or_indirect', 'optargsp', 'postcond')),
                # 8.2.17 - READ
                (words(('read', 'r'), suffix=r'\b'), Keyword, ('#pop', 'l_readargument', 'argumentsp', 'postcond')),
                # 8.2.18 - SET
                (words(('set', 's'), suffix=r'\b'), Keyword, ('#pop', 'l_setargument', 'argumentsp')),
                # 8.2.19 - TCOMMIT
                (words(('tcommit', 'tc'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
                # 8.2.20 - TRESTART
                (words(('trestart', 'tre'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
                # 8.2.21 - TROLLBACK
                (words(('trollback', 'tro'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
                # 8.2.22 - TSTART
                (words(('tstart', 'ts'), suffix=r'\b'), Keyword, ('#pop', 'tstartargument', 'optargsp', 'postcond')),
                # 8.2.23 - USE
                (words(('use', 'u'), suffix=r'\b'), Keyword, ('#pop', 'useargument', 'argumentsp', 'postcond')),
                # 8.2.24 - VIEW
                (words(('view', 'v'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
                # 8.2.25 - WRITE
                (words(('write', 'w'), suffix=r'\b'), Keyword, ('#pop', 'l_writeargument', 'argumentsp', 'postcond')),
                # 8.2.26 - XECUTE
                (words(('xecute', 'x'), suffix=r'\b'), Keyword, ('#pop', 'l_xargument', 'argumentsp', 'postcond')),
                ],
        # 8.2.2 - CLOSE arguments
        'closearg': [
                ('@', Operator, ('#pop', 'expratom')),
                default(('#pop', 'deviceparameters', 'colon_sep', 'expr')),
                ],
        'deviceparameters': [
                ('\\(', Punctuation, ('#pop', 'deviceparams_group')),
                include('deviceparam'),
                ],
        'deviceparams_group': [
                default(('colon_group', 'deviceparam'))
                ],
        'deviceparam': [
                #('(' + name_re + ')(=)', bygroups(Name.Variable, Operator),('#pop', 'expr')),
                include('expr'),
                ],
        'colon_group': [
                (':', Punctuation, '#pop'),
                ('\\)', Punctuation, '#pop:2'),
                ],
        'l_closearg': [
                default(('list_comma', 'closearg'))
                ],
        # 8.2.3 - DO arguments
        'doargument': [
                default(('#pop', 'postcond', 'opt_actuallist', 'lineref_or_externref')),
                ],
        'lineref_or_externref': [
                include('externref'),
                include('lineref'),
                ],
        'l_doargument': [
                default(('list_comma', 'doargument')),
                ],
        'opt_actuallist': [
                include('actuallist'),
                default('#pop'),
                ],
        # 8.2.5 - FOR parameters
        'for_argument': [
            default(('#pop', 'forparameter', 'equals', 'lvn')),
            ],
        'forparameter': [
            default(('#pop', 'expr', 'colon_sep', 'expr', 'colon_sep', 'expr')),
            ],
        'equals': [
                ('=', Operator, '#pop'),
                ],
        # 8.2.6 - GOTO parameters
        'gotoargument': [
                default(('#pop', 'postcond', 'entryref')),
                ],
        'l_gotoargument': [
                default(('list_comma', 'gotoargument')),
                ],
        # 8.2.10 - JOB arguments
        'jobargument': [
                default(('#pop', 'jobparameters', 'colon_sep', 'opt_actuallist', 'lineref_or_externref')),
                ],
        'l_jobargument': [
                default(('list_comma', 'jobargument')),
                ],
        'jobparameters': [
                default(('#pop', 'timeout', 'processparameters'))
                ],
        'processparameters': [
                ('\\(', Punctuation, ('#pop', 'processparameter_group')),
                include('expr'),
                ],
        'processparameter_group': [
                default(('colon_group', 'expr'))
                ],
        # 8.2.11 - KILL arguments
        'killargument': [
                ('\\(', Punctuation, ('#pop', 'exclusive_killargs', 'lname')),
                ('@', Operator, ('#pop', 'expratom')),
                include('glvn'),
                ],
        'l_killargument': [
                default(('list_comma', 'killargument')),
                ],
        'exclusive_killargs': [
                ('\\)', Punctuation, '#pop'),
                (',', Punctuation, 'lname'),
                ],
        'lname': [
                ('@', Operator, ('#pop', 'expratom')),
                include('name'),
                ],
        # 8.2.12 - LOCK arguments
        'lockargument': [
                ('[+-]', Operator),
                ('\\(', Punctuation, ('#pop', 'timeout', 'close_paren', 'l_nref')),
                default(('#pop', 'timeout', 'nref')),
                ],
        'l_lockargument': [
                default(('list_comma', 'lockargument'))
                ],
        'nref': [
                ('@', Operator, ('#pop', 'expratom')),
                ('\\^?'+name_re, Name, ('#pop', 'opt_subscripts')),
                ],
        'l_nref': [
                default(('list_comma', 'nref'))
                ],
        # 8.2.13 - MERGE arguments
        'mergeargument': [
                # Indirection could be an indirected argument list, or the beginning of a glvn to be set
                ('@', Operator, ('#pop', 'mergearg_post_indirect', 'expratom')),
                default(('#pop', 'glvn', 'equals', 'glvn'))
                ],
        'l_mergeargument': [
                default(('list_comma', 'mergeargument'))
                ],
        'mergearg_post_indirect': [
                # Was indirected variable name without subscripts
                ('=', Operator, ('#pop', 'glvn')),
                # Indirected with subscripts
                ('@', Operator, ('#pop', 'glvn', 'equals', 'subscripts')),
                # Otherwise, assume it was a full argument
                default('#pop')
                ],
        # 8.2.14 - NEW arguments
        'newargument': [
                ('\\(', Punctuation, ('#pop', 'close_paren', 'l_lname')),
                # newsvn, only exists here
                (words(('$ETRAP', '$ET', '$ESTACK', '$ES'), suffix=r'\b'), Name.Variable.Magic, '#pop'),
                include('lname')
                ],
        'l_newargument': [
                default(('list_comma', 'newargument'))
                ],
        'l_lname': [
                default(('list_comma', 'lname'))
                ],
        # 8.2.15 - OPEN arguments
        'openargument': [
            default(('#pop', 'mnemonicspec', 'colon_sep', 'timeout', 'deviceparameters', 'colon_sep', 'expr')),
            ],
        'l_openargument': [
            default(('list_comma', 'openargument')),
            ],
        'colon_sep': [
            (':(?=:)', Punctuation, '#pop:2'),
            (':', Punctuation, '#pop'),
            default('#pop:2')
            ],
        'mnemonicspec': [
            ('\\(', Punctuation, ('#pop', 'close_paren', 'l_mnemonicspace')),
            default(('#pop', 'mnemonicspace'))
            ],
        'mnemonicspace': [
            include('expr'),
            ],
        'l_mnemonicspace': [
            default(('list_comma', 'mnemonicspace'))
            ],
        # 8.2.16 - QUIT arguments
        'expr_or_indirect': [
            ('@', Operator, ('#pop', 'expratom')),
            include('expr')
            ],
        # 8.2.17 - READ
        'readargument': [
            include('format'),
            include('strlit'),
            ('\\*', Keyword.Pseudo, ('timeout', 'glvn')),
            default(('#pop', 'timeout', 'opt_readcount', 'glvn')),
            ],
        'l_readargument': [
            default(('list_comma', 'readargument'))
            ],
        'format': [
            ('[#!]+(?=\\?)', Keyword.Pseudo),
            ('[#!]+', Keyword.Pseudo, '#pop'),
            ('\\?', Keyword.Pseudo, ('#pop', 'expr')),
            ('(/[?A-Z][A-Z0-9]*)(\\()', bygroups(Keyword.Pseudo, Punctuation), ('#pop', 'close_paren', 'l_expr')),
            ('/[?A-Z][A-Z0-9]*', Keyword.Pseudo, '#pop'),
            ],
        'readcount': [
            ('#', Punctuation, ('#pop', 'expr')),
            ],
        'opt_readcount': [
            include('readcount'),
            default('#pop')
            ],
        # 8.2.18 - SET
        'setargument': [
            ('@', Operator, ('#pop', 'setarg_ind', 'expratom')),
            default(('#pop', 'expr', 'equals', 'setdestination'))
            ],
        'setarg_ind': [
            # If followed by @ or =, it's was an indirection from glvn
            ('@', Operator, ('#pop', 'expr', 'equals', 'subscripts')),
            ('=', Operator, ('#pop', 'expr')),
            # Otherwise, it's was a full setargument replacement
            default('#pop')
            ],
        'l_setargument': [
            default(('list_comma', 'setargument'))
            ],
        'setdestination': [
            ('\\(', Punctuation, ('#pop', 'close_paren', 'l_setleft')),
            default(('#pop', 'setleft'))
            ],
        'setleft': [
            include('leftrestricted'),
            include('leftexpr'),
            include('glvn')
            ],
        'l_setleft': [
            default(('list_comma', 'setleft'))
            ],
        'leftrestricted': [
            (words(('$DEVICE', '$D', '$KEY', '$K', '$X', '$Y'), suffix=r'\b'), Name.Variable.Magic, '#pop')
            ],
        'leftexpr': [
            include('setpiece'),
            include('setextract'),
            include('setev'),
            ],
        'setpiece': [
            (words(('$PIECE', '$P'), suffix=r'\b'), Name.Function.Magic, ('#pop', 'close_paren', 'l_expr', 'comma', 'glvn', 'open_paren')),
            ],
        'setextract': [
            (words(('$EXTRACT', '$E'), suffix=r'\b'), Name.Function.Magic, ('#pop', 'close_paren', 'l_expr', 'comma', 'glvn', 'open_paren'))
            ],
        'setev': [
            (words(('$ECODE', '$EC'), suffix=r'\b'), Name.Variable.Magic, '#pop'),
            (words(('$ETRAP', '$ET'), suffix=r'\b'), Name.Variable.Magic, '#pop'),
            ],
        # 8.2.22 - TSTART
        'tstartargument': [
            (':', Punctuation, ('#pop', 'transparameters')),
            default(('#pop', 'transparameters', 'colon_sep', 'restartargument'))
            ],
        'transparameters': [
            ('\\(', Punctuation, ('#pop', 'tsparam_group')),
            include('tsparam')
            ],
        'tsparam_group': [
            default(('colon_group', 'tsparam'))
            ],
        'tsparam': [
            default(('#pop', 'opt_equals_expr', 'tstartkeyword'))
            ],
        'opt_equals_expr': [
            ('=', Operator, ('#pop', 'expr')),
            default('#pop')
            ],
        'tstartkeyword': [
            ('[A-Z]+', Keyword, '#pop')
            ],
        'restartargument': [
            ('\\*', Keyword.Pseudo, '#pop'),
            ('(\\()(\\))', bygroups(Punctuation, Punctuation), '#pop'),
            ('\\(', Punctuation, ('#pop', 'close_paren', 'l_lname')),
            include('lname')
            ],
        # 8.2.23 - USE
        'useargument': [
            default(('#pop', 'mnemonicspace', 'colon_sep', 'deviceparameters', 'colon_sep', 'expr'))
            ],
        # 8.2.25 - WRITE
        'l_writeargument': [
            default(('list_comma', 'writeargument'))
            ],
        'writeargument': [
            include('format'),
            ('\\*', Keyword.Pseudo, ('#pop', 'expr')),
            include('expr'),
            ],
        # 8.2.26 - XECUTE
        'l_xargument': [
            default(('list_comma', 'xargument'))
            ],
        'xargument': [
            default(('#pop', 'postcond', 'expr'))
            ],
        }
