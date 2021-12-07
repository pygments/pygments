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
                include('function_ascii'),
                include('function_char'),
                include('function_data'),
                include('function_extract'),
                include('function_find'),
                include('function_fnumber'),
                include('function_get'),
                include('function_justify'),
                include('function_length'),
                include('function_name'),
                include('function_order'),
		include('function_piece'),
                ],
        # 7.1.5.1 - $ASCII
        'function_ascii': [
                (words(('$ASCII', '$A'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'list_comma', 'expr', 'open_paren')),
                ],
        # 7.1.5.2 - $CHAR
        'function_char': [
                (words(('$CHAR', '$C'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'l_expr', 'open_paren')),
                ],
        'function_exract': [
                ],
        # 7.1.5.3 - $DATA
        'function_data': [
                (words(('$DATA', '$D'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'glvn', 'open_paren')),
                ],
        # 7.1.5.4 - $EXTRACT
        'function_extract': [
                (words(('$EXTRACT', '$E'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'list_comma', 'expr', 'list_comma', 'expr', 'open_paren')),
                ],
        # 7.1.5.5 - $FIND
        'function_find': [
                (words(('$FIND' , '$F'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'list_comma', 'expr', 'comma', 'expr', 'open_paren')),
                ],
        # 7.1.5.6 - $FNUMBER
        'function_fnumber': [
                (words(('$FNUMBER', '$FN'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'list_comma', 'expr', 'comma', 'expr','open_paren')),
                ],
        # 7.1.5.7 - $GET
        'function_get': [
                (words(('$GET', '$G'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'list_comma', 'glvn', 'open_paren')),
                ],
        # 7.1.5.8 - $JUSTIFY
        'function_justify': [
                (words(('$JUSTIFY', '$J'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'list_comma', 'expr', 'comma', 'expr', 'open_paren'))
                ],
        # 7.1.5.9 - $LENGTH
        'function_length': [
                (words(('$LENGTH', '$L'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'list_comma', 'expr', 'open_paren'))
                ],
        # 7.1.5.10 - $NAME
        'function_name': [
                (words(('$NAME', '$NA'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'list_comma', 'glvn', 'open_paren'))
                ],
        # 7.1.5.11 - $ORDER
        'function_order': [
                (words(('$ORDER', '$O'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'list_comma', 'glvn', 'open_paren'))
                ],
        # 7.1.5.12 - $PIECE
	'function_piece': [
		(words(('$PIECE', '$P'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'list_comma', 'expr', 'list_comma', 'expr', 'comma', 'expr', 'open_paren'))
		],
        # 7.1.5.13 - $QLENGTH
        # 7.1.5.14 - $QSUBSCRIPT
        # 7.1.5.15 - $QUERY
        # 7.1.5.16 - $RANDOM
        # 7.1.5.17 - $REVERSE
        # 7.1.5.18 - $SELECT
        # 7.1.5.19 - $STACK
        # 7.1.5.20 - $TEXT
        # 7.1.5.21 - $TRANSLATE
        # 7.1.5.22 - $VIEW
        # 7.1.5.23 - $Z* functions are not in the standard
        # 7.2 - Expression tail exprtail
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
                include('command_break'),
                include('command_close'),
                include('command_do'),
                include('command_else'),
                include('command_for'),
                include('command_goto'),
                include('command_halt'),
                include('command_hang'),
                # Combination halt/hang - both abbreviate to h, so the difference is whether there are arguments
                (r'h\b', Keyword, ('#pop', 'l_expr', 'optargsp', 'postcond')),
                include('command_if'),
                include('command_job'),
                include('command_kill'),
                include('command_lock'),
                include('command_merge'),
                include('command_new'),
                include('command_open'),
                include('command_quit'),
                include('command_read'),
                include('command_set'),
                include('command_tcommit'),
                include('command_tstart'),
                include('command_trestart'),
                include('command_trollback'),
                include('command_use'),
                include('command_view'),
                include('command_write'),
                include('command_xecute'),
                ],
        # 8.2.1 - BREAK
        'command_break': [
                (words(('break', 'b'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
                ],
        # 8.2.2 - CLOSE
        'command_close': [
                (words(('close', 'c'), suffix=r'\b'), Keyword, ('#pop', 'l_closearg', 'argumentsp', 'postcond')),
                ],
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
        # 8.2.3 - DO
        'command_do': [
                (r'do?\b', Keyword, ('#pop', 'l_doargument', 'optargsp', 'postcond')),
                ],
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
        # 8.2.4 - ELSE
        'command_else': [
                (words(('else', 'e'), suffix=r'\b'), Keyword, ('#pop', 'noargsp')),
                ],
        # 8.2.5 - FOR
        'command_for': [
                (words(('for', 'f'), suffix=r'\b'), Keyword, ('#pop', 'for_argument', 'optargsp')),
                ],
        'for_argument': [
                default(('#pop', 'forparameter', 'equals', 'lvn')),
                ],
        'forparameter': [
                default(('#pop', 'expr', 'colon_sep', 'expr', 'colon_sep', 'expr')),
                ],
        'equals': [
                ('=', Operator, '#pop'),
                ],
        # 8.2.6 - GOTO
        'command_goto': [
                (words(('goto', 'g'), suffix=r'\b'), Keyword, ('#pop', 'l_gotoargument', 'argumentsp', 'postcond')),
                ],
        'gotoargument': [
                default(('#pop', 'postcond', 'entryref')),
                ],
        'l_gotoargument': [
                default(('list_comma', 'gotoargument')),
                ],
        # 8.2.7 - HALT
        'command_halt': [
                (r'halt\b', Keyword, ('#pop', 'noargsp')),
                ],
        # 8.2.8 - HANG
        'command_hang': [
                (r'hang\b', Keyword, ('#pop', 'l_expr', 'argumentsp', 'postcond')),
                ],
        # 8.2.9 - IF
        'command_if': [
                (r'if?\b', Keyword, ('#pop', 'l_expr', 'optargsp')),
                ],
        # 8.2.10 - JOB
        'command_job': [
                (words(('job', 'j'), suffix=r'\b'), Keyword, ('#pop', 'l_jobargument', 'argumentsp', 'postcond')),
                ],
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
        # 8.2.11 - KILL
        'command_kill': [
                (words(('kill', 'k'), suffix=r'\b'), Keyword, ('#pop', 'l_killargument', 'optargsp', 'postcond')),
                ],
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
        # 8.2.12 - LOCK
        'command_lock': [
                (words(('lock', 'l'), suffix=r'\b'), Keyword, ('#pop', 'l_lockargument', 'optargsp', 'postcond')),
                ],
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
        # 8.2.13 - MERGE
        'command_merge': [
                (words(('merge', 'm'), suffix=r'\b'), Keyword, ('#pop', 'l_mergeargument', 'argumentsp', 'postcond')),
                ],
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
        # 8.2.14 - NEW
        'command_new': [
                (words(('new', 'n'), suffix=r'\b'), Keyword, ('#pop', 'l_newargument', 'optargsp', 'postcond')),
                ],
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
        # 8.2.15 - OPEN
        'command_open': [
            (words(('open', 'o'), suffix=r'\b'), Keyword, ('#pop', 'l_openargument', 'argumentsp', 'postcond')),
            ],
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
        # 8.2.16 - QUIT
        'command_quit': [
            (words(('quit', 'q'), suffix=r'\b'), Keyword, ('#pop', 'expr_or_indirect', 'optargsp', 'postcond')),
            ],
        'expr_or_indirect': [
            ('@', Operator, ('#pop', 'expratom')),
            include('expr')
            ],
        # 8.2.17 - READ
        'command_read': [
            (words(('read', 'r'), suffix=r'\b'), Keyword, ('#pop', 'l_readargument', 'argumentsp', 'postcond')),
            ],
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
        'command_set': [
                (words(('set', 's'), suffix=r'\b'), Keyword, ('#pop', 'l_setargument', 'argumentsp')),
                ],
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
        # 8.2.19 - TCOMMIT
        'command_tcommit': [
            (words(('tcommit', 'tc'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
            ],
        # 8.2.20 - TRESTART
        'command_trestart': [
            (words(('trestart', 'tre'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
            ],
        # 8.2.21 - TROLLBACK
        'command_trollback': [
            (words(('trollback', 'tro'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
            ],
        # 8.2.22 - TSTART
        'command_tstart': [
            (words(('tstart', 'ts'), suffix=r'\b'), Keyword, ('#pop', 'tstartargument', 'optargsp', 'postcond')),
            ],
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
        'command_use': [
            (words(('use', 'u'), suffix=r'\b'), Keyword, ('#pop', 'useargument', 'argumentsp', 'postcond')),
            ],
        'useargument': [
            default(('#pop', 'mnemonicspace', 'colon_sep', 'deviceparameters', 'colon_sep', 'expr'))
            ],
        # 8.2.24 - VIEW
        'command_view': [
            (words(('view', 'v'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
            ],
        # 8.2.25 - WRITE
        'command_write': [
            (words(('write', 'w'), suffix=r'\b'), Keyword, ('#pop', 'l_writeargument', 'argumentsp', 'postcond')),
            ],
        'l_writeargument': [
            default(('list_comma', 'writeargument'))
            ],
        'writeargument': [
            include('format'),
            ('\\*', Keyword.Pseudo, ('#pop', 'expr')),
            include('expr'),
            ],
        # 8.2.26 - XECUTE
        'command_xecute': [
            (words(('xecute', 'x'), suffix=r'\b'), Keyword, ('#pop', 'l_xargument', 'argumentsp', 'postcond')),
            ],
        'l_xargument': [
            default(('list_comma', 'xargument'))
            ],
        'xargument': [
            default(('#pop', 'postcond', 'expr'))
            ],
        }
