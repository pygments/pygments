# -*- coding: utf-8 -*-
"""
    pygments.lexers.mumps
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for MUMPS

    :copyright: Copyright 2020-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.


    :todo: Add LI / LEVELLINE line indent
"""

import re
from pygments.lexer import ExtendedRegexLexer, words, bygroups, default, include
from pygments.token import Whitespace, Keyword, Operator, Number, Name, Punctuation, Comment, String

__all__ = ['MumpsLexer']

class MumpsLexer(ExtendedRegexLexer):
    """
    For MUMPS source code.

    Derived from `The Annotated M[UMPS] Standard <http://71.174.62.16/Demo/AnnoStd>`
    Section numbers below refer to the sections from the 1995 (current) standard on that site
    """

    name = 'MUMPS'
    aliases = ['Mumps', 'mumps', 'M']
    filenames = ['*.m', '*.mumps', '*.epc', '*.int']
    # Filenames aren't meaningful in M, but some implementations allow file export/import of routines
    # For example, YottaDB would have the source file for "dmex" in "dmex.m"
    flags = re.IGNORECASE

    # 5 - Metalanguage description
    # 'L': list of one or more, separated by commas
    def L(state):
        return [default(('list_comma', state))]

    # Definitions of groups that we implement as regular expressions
    # 6.1 - Routine head 'routinehead'
    name_re = '[%A-Za-z][A-Za-z0-9]*'
    # 6.2.4 - Label separator 'ls'
    ls_re = ' +'
    # 7.1.4.1 - String literal 'strlit'
    strlit_re = '"(""|[^"])*"'
    # 7.1.4.10 - Intrinsic special variables 'svn'
    svn_re = words(('$DEVICE', '$D', '$ECODE', '$EC', '$ESTACK', '$ES', '$ETRAP', '$ET', '$HOROLOG', '$H', '$IO', '$I', '$JOB', '$J', '$KEY', '$K', '$PRINCIPAL', '$P', '$QUIT', '$Q', '$STACK', '$ST', '$STORAGE', '$S', '$SYSTEM', '$SY', '$TEST', '$T', '$TLEVEL', '$TL', '$TRESTART', '$TR', '$X', '$Y'), suffix=r'\b(?!\()')
    # 7.1.4.11 - Unary operator 'unaryop'
    unaryop_re = "[-+']"
    # 7.2.1 - binaryop
    binaryop_re = r'\*\*|[-_+*/\\#]'
    # 7.2.2 - truthop
    relation_re = r'\]\]|[<>=\[\]]' # 7.2.2.1 - Relational operator 'relation'
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
            (name_re + r'(?= )', Name.Label),
            (ls_re, Whitespace, ('#pop', 'linebody')),
            # 6.2.2 formalline
            ( name_re + r'(?=\()', Name.Function, 'formallist'),
            ],
        # 6.2.2 Formal line 'formalline'
        'formallist': [
            default(('#pop', 'close_paren', 'l_name', 'open_paren_optempty'))
            ],
        'open_paren_optempty': [
            (r'\((?=\))', Punctuation, '#pop:2'),
            include('open_paren')
            ],
        # list of 'name' continuation
        'l_name': L('name'),
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
                default(('#pop', 'exprtail_chain', 'expratom'))
                ],
        'exprtail_chain': [
            default('exprtail_link')
            ],
        'exprtail_link': [
            include('exprtail'),
            default('#pop:2')
            ],
        'l_expr': L('expr'),
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
                (r'\^(?=\()', Name.Variable.Global, ('#pop', 'subscripts')),
                (r'\^(?=\|)', Name.Variable.Global, ('#pop', 'opt_subscripts', 'gname', 'environment')), 
                (r'\^' + name_re, Name.Variable.Global, ('#pop', 'opt_subscripts')),
                ],
        'environment': [
                (r'\|', Punctuation, ('#pop', 'vert_bar', 'expr')),
                ],
        'vert_bar': [
                (r'\|', Punctuation, '#pop'),
                ],
        'gname': [
                (name_re, Name.Variable.Global, '#pop'),
                ],
        # Parsing for '( L expr )'
        'subscripts': [
                (r'\(', Punctuation, ('#pop', 'close_paren', 'l_expr'))
                ],
        'opt_subscripts': [
                include('subscripts'),
                default('#pop')
                ],
        'open_paren': [
                (r'\(', Punctuation, '#pop'),
                ],
        'close_paren': [
                (r'\)', Punctuation, '#pop'),
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
                (r'\^\$[A-Z]+', Name.Variable.Magic, ('#pop', 'subscripts')),
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
            ( r'\(', Punctuation, ('#pop', 'close_paren', 'expr')),
            ],
        # 7.1.4.1 - String literal 'strlit'
        'strlit': [
                ( strlit_re , String, '#pop'),
                ],
        # 7.1.4.2 - Numeric literal 'numlit'
        'numlit': [
                (r'[0-9]*\.[0-9]+E[+-]?[0-9]+', Number, '#pop'),
                (r'[0-9]*\.[0-9]+', Number, '#pop'),
                ('[0-9]+E[+-]?[0-9]+', Number, '#pop'),
                ('[0-9]+', Number, '#pop'),
                ],
        # 7.1.4.6 - Integer interpretation
        'intexpr': [
                include('expr'),
                ],
        # 7.1.4.7 - Truth-value interpretation
        'tvexpr': [
                include('expr'),
                ],
        # 7.1.4.8 - Extrinsic function exfunc
        # 7.1.4.9 - Extrinsic special variable exvar - Same syntax, no actuallist
        'exfunc': [
                (r'\$\$', Punctuation, ('#pop', 'opt_actuallist', 'labelref_func')),
                (r'\$(?=&)', Punctuation, ('#pop', 'opt_actuallist', 'externref_func')),
                ],
        # 7.1.4.10 - Intrinsic special variable names 'svn'
        'svn': [
                (svn_re, Name.Variable.Magic, '#pop'),
                ],
        # 7.1.4.12 - Name value 'namevalue'
        'namevalue': [
                include('expr'),
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
                include('function_qlength'),
                include('function_qsubscript'),
                include('function_query'),
                include('function_random'),
                include('function_reverse'),
                include('function_select'),
                include('function_stack'),
                include('function_text'),
                include('function_translate'),
                include('function_view'),
                ],
        # 7.1.5.1 - $ASCII
        'function_ascii': [
                (words(('$ASCII', '$A'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'open_paren')),
                ],
        'opt_param_comma': [
                include('comma'),
                default('#pop:2')
                ],
        # 7.1.5.2 - $CHAR
        'function_char': [
                (words(('$CHAR', '$C'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'l_expr', 'open_paren')),
                ],
        # 7.1.5.3 - $DATA
        'function_data': [
                (words(('$DATA', '$D'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'glvn', 'open_paren')),
                ],
        # 7.1.5.4 - $EXTRACT
        'function_extract': [
                (words(('$EXTRACT', '$E'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'opt_param_comma', 'expr', 'open_paren')),
                ],
        # 7.1.5.5 - $FIND
        'function_find': [
                (words(('$FIND' , '$F'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'comma', 'expr', 'open_paren')),
                ],
        # 7.1.5.6 - $FNUMBER
        'function_fnumber': [
                (words(('$FNUMBER', '$FN'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'comma', 'expr','open_paren')),
                ],
        # 7.1.5.7 - $GET
        'function_get': [
                (words(('$GET', '$G'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'glvn', 'open_paren')),
                ],
        # 7.1.5.8 - $JUSTIFY
        'function_justify': [
                (words(('$JUSTIFY', '$J'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'comma', 'expr', 'open_paren'))
                ],
        # 7.1.5.9 - $LENGTH
        'function_length': [
                (words(('$LENGTH', '$L'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'open_paren'))
                ],
        # 7.1.5.10 - $NAME
        'function_name': [
                (words(('$NAME', '$NA'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'glvn', 'open_paren'))
                ],
        # 7.1.5.11 - $ORDER
        'function_order': [
                (words(('$ORDER', '$O'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'glvn', 'open_paren'))
                ],
        # 7.1.5.12 - $PIECE
        'function_piece': [
                (words(('$PIECE', '$P'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'opt_param_comma', 'expr', 'comma', 'expr', 'open_paren'))
                ],
        # 7.1.5.13 - $QLENGTH
        'function_qlength': [
                (words(('$QLENGTH', '$QL'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'open_paren'))
                ],
        # 7.1.5.14 - $QSUBSCRIPT
        'function_qsubscript': [
                (words(('$QSUBSCRIPT', '$QS'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'intexpr', 'comma', 'namevalue', 'open_paren'))
                ],
        # 7.1.5.15 - $QUERY
        'function_query': [
                (words(('$QUERY', '$Q'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'glvn', 'open_paren'))
                ],
        # 7.1.5.16 - $RANDOM
        'function_random': [
                (words(('$RANDOM', '$R'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'intexpr', 'open_paren'))
                ],
        # 7.1.5.17 - $REVERSE
        'function_reverse': [
                (words(('$REVERSE', '$RE'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'open_paren'))
                ],
        # 7.1.5.18 - $SELECT
        'function_select': [
                (words(('$SELECT', '$S'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'l_selectatom', 'open_paren'))
                ],
        'l_selectatom': L('selectatom'),
        'selectatom': [
                default(('#pop', 'expr', 'colon', 'tvexpr'))
                ],
        # 7.1.5.19 - $STACK
        'function_stack': [
                (words(('$STACK', '$ST'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'intexpr', 'open_paren'))
                ],
        # 7.1.5.20 - $TEXT
        'function_text': [
                (words(('$TEXT', '$T'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'textarg', 'open_paren'))
                ],
        'textarg': [
                (r'\+', Operator, ('#pop', 'opt_routineref', 'intexpr')),
                include('entryref')
                ],
        'opt_rouref': [
                (r'\^', Punctuation, ('#pop', 'routineref')),
                default('#pop')
                ],
        # 7.1.5.21 - $TRANSLATE
        'function_translate': [
            (words(('$TRANSLATE', '$TR'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'comma', 'expr', 'open_paren'))
            ],
        # 7.1.5.22 - $VIEW
        'function_view': [
            # The parameters for VIEW are not specified, so here we allow null only
            (words(( '$VIEW', '$V'), suffix=r'(?=\()'), Name.Function, ('#pop', 'close_paren', 'open_paren'))
            ],
        # 7.1.5.23 - $Z* functions are not in the standard
        # 7.2 - Expression tail 'exprtail'
        'exprtail': [
            include('exprtail_binaryop'),
            include('exprtail_truthop'),
            include('exprtail_patternop'),
            ("'", Operator, ('#pop', 'exprtail_nottable_ops')),
            ],
        # Operator clauses that may follow a 'not'
        'exprtail_nottable_ops': [
            include('exprtail_truthop'),
            include('exprtail_patternop'),
            ],
        'exprtail_truthop': [
            ( truthop_re, Operator, ('#pop', 'expratom'))
            ],
        'exprtail_binaryop': [
            ( binaryop_re, Operator, ('#pop', 'expratom'))
            ],
        'exprtail_patternop': [
            ( r'\?', Operator, ('#pop', 'pattern')),
            ],
        # 7.2.3 - Pattern match 'pattern'
        'pattern': [
                ( '@', Operator, ('#pop', 'expratom')),
                default(('#pop', 'more_patatoms', 'patatom')),
                ],
        'patatom': [
                # Detect the repcount, then jump to detecting the pattern code
                ( r'([0-9]+)(\.)([0-9]*)', bygroups(Number, Punctuation, Number), ('#pop', 'patatom_choice')),
                ( r'(\.)([0-9]*)', bygroups(Punctuation, Number), ('#pop', 'patatom_choice')),
                ( '[0-9]+', Number, ('#pop', 'patatom_choice')),
                ],
        'more_patatoms': [
                ( r'([0-9]+)(\.?)([0-9]*)', bygroups(Number, Punctuation, Number), 'patatom_choice'),
                ( r'(\.?)([0-9]*)', bygroups(Punctuation, Number), 'patatom_choice'),
                ( '[0-9]+', Number, 'patatom_choice'),
                default('#pop')
                ],
        'patatom_choice': [
                (r'\(', Punctuation, ('#pop', 'alternation', 'patatom')),
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
                (r'\)', Punctuation, '#pop')
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
                ( r'\^', Punctuation, ('#pop', 'routineref')),
                ( '@', Operator, ('#pop', 'opt_routineref', 'opt_offset', 'expratom')),
                ],
        'opt_offset': [
                (r'\+', Operator, ('#pop', 'expr')),
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
                (r'\^', Punctuation, ('#pop', 'routineref')),
                default('#pop'),
                ],
        # 8.1.6.2 - Label reference labelref
        'labelref': [
                ( name_re , Name.Label, ('#pop', 'opt_routineref_strict')),
                (r'\^', Punctuation, ('#pop', 'routineref_strict')),
                ],
        'labelref_func': [
                ( name_re , Name.Function, ('#pop', 'opt_routineref_strict')),
                (r'\^', Punctuation, ('#pop', 'routineref_strict')),
                ],
        'opt_environment': [
                include('environment'),
                default('#pop')
                ],
        'opt_routineref_strict': [
                (r'\^', Punctuation, ('#pop', 'routineref_strict')),
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
                ('(' + name_re + r')(\.)', bygroups(Name.Namespace, Punctuation), '#pop'),
                default('#pop')
                ],
        # 8.1.7 - Parameter passing
        'actuallist': [
                (r'\(', Punctuation, ('#pop', 'actuallist_contents'),)
                ],
        'actuallist_contents': [
                (r'\)', Punctuation, '#pop'),
                default('l_actual'),
                ],
        'l_actual': L('actual'),
        'actual': [
                (r'\.', Punctuation, ('#pop', 'actualname')),
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
                (r'\(', Punctuation, ('#pop', 'deviceparams_group')),
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
                include('colon'),
                (r'\)', Punctuation, '#pop:2'),
                ],
        'l_closearg': L('closearg'),
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
        'l_doargument': L('doargument'),
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
        'l_gotoargument': L('gotoargument'),
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
        'l_jobargument': L('jobargument'),
        'jobparameters': [
                default(('#pop', 'timeout', 'processparameters'))
                ],
        'processparameters': [
                (r'\(', Punctuation, ('#pop', 'processparameter_group')),
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
                (r'\(', Punctuation, ('#pop', 'exclusive_killargs', 'lname')),
                ('@', Operator, ('#pop', 'expratom')),
                include('glvn'),
                ],
        'l_killargument': L('killargument'),
        'exclusive_killargs': [
                (r'\)', Punctuation, '#pop'),
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
                (r'\(', Punctuation, ('#pop', 'timeout', 'close_paren', 'l_nref')),
                default(('#pop', 'timeout', 'nref')),
                ],
        'l_lockargument': L('lockargument'),
        'nref': [
                ('@', Operator, ('#pop', 'expratom')),
                (r'\^?'+name_re, Name, ('#pop', 'opt_subscripts')),
                ],
        'l_nref': L('nref'),
        # 8.2.13 - MERGE
        'command_merge': [
                (words(('merge', 'm'), suffix=r'\b'), Keyword, ('#pop', 'l_mergeargument', 'argumentsp', 'postcond')),
                ],
        'mergeargument': [
                # Indirection could be an indirected argument list, or the beginning of a glvn to be set
                ('@', Operator, ('#pop', 'mergearg_post_indirect', 'expratom')),
                default(('#pop', 'glvn', 'equals', 'glvn'))
                ],
        'l_mergeargument': L('mergeargument'),
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
                (r'\(', Punctuation, ('#pop', 'close_paren', 'l_lname')),
                # newsvn, only exists here
                (words(('$ETRAP', '$ET', '$ESTACK', '$ES'), suffix=r'\b'), Name.Variable.Magic, '#pop'),
                include('lname')
                ],
        'l_newargument': L('newargument'),
        'l_lname': L('lname'),
        # 8.2.15 - OPEN
        'command_open': [
            (words(('open', 'o'), suffix=r'\b'), Keyword, ('#pop', 'l_openargument', 'argumentsp', 'postcond')),
            ],
        'openargument': [
            default(('#pop', 'mnemonicspec', 'colon_sep', 'timeout', 'deviceparameters', 'colon_sep', 'expr')),
            ],
        'l_openargument': L('openargument'),
        'colon': [
            (':', Punctuation, '#pop')
            ],
        'colon_sep': [
            (':(?=:)', Punctuation, '#pop:2'),
            include('colon'),
            default('#pop:2')
            ],
        'mnemonicspec': [
            (r'\(', Punctuation, ('#pop', 'close_paren', 'l_mnemonicspace')),
            default(('#pop', 'mnemonicspace'))
            ],
        'mnemonicspace': [
            include('expr'),
            ],
        'l_mnemonicspace': L('mnemonicspace'),
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
            (r'\*', Keyword.Pseudo, ('timeout', 'glvn')),
            default(('#pop', 'timeout', 'opt_readcount', 'glvn')),
            ],
        'l_readargument': L('readargument'),
        'format': [
            (r'[#!]+(?=\?)', Keyword.Pseudo),
            ('[#!]+', Keyword.Pseudo, '#pop'),
            (r'\?', Keyword.Pseudo, ('#pop', 'expr')),
            (r'(/[?A-Z][A-Z0-9]*)(\()', bygroups(Keyword.Pseudo, Punctuation), ('#pop', 'close_paren', 'l_expr')),
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
        'l_setargument': L('setargument'),
        'setdestination': [
            (r'\(', Punctuation, ('#pop', 'close_paren', 'l_setleft')),
            default(('#pop', 'setleft'))
            ],
        'setleft': [
            include('leftrestricted'),
            include('leftexpr'),
            include('glvn')
            ],
        'l_setleft': L('setleft'),
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
            default('restartargument')
            ],
        'transparameters': [
            (r'\(', Punctuation, ('#pop', 'tsparam_group')),
            include('tsparam')
            ],
        'tsparam_group': [
            default(('colon_group', 'tsparam'))
            ],
        'tsparam': [
            default(('#pop', 'expr', 'opt_equals_sep', 'tstartkeyword'))
            ],
        'opt_equals_sep': [
            include('equals'),
            default('#pop:2')
            ],
        'tstartkeyword': [
            ('[A-Z]+', Keyword, '#pop')
            ],
        'restartargument': [
            (r'\*', Keyword.Pseudo, '#pop'),
            (r'(\()(\))', bygroups(Punctuation, Punctuation), '#pop'),
            (r'\(', Punctuation, ('#pop', 'close_paren', 'l_lname')),
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
        'l_writeargument': L('writeargument'),
        'writeargument': [
            include('format'),
            (r'\*', Keyword.Pseudo, ('#pop', 'expr')),
            include('expr'),
            ],
        # 8.2.26 - XECUTE
        'command_xecute': [
            (words(('xecute', 'x'), suffix=r'\b'), Keyword, ('#pop', 'l_xargument', 'argumentsp', 'postcond')),
            ],
        'l_xargument': L('xargument'),
        'xargument': [
            default(('#pop', 'postcond', 'expr'))
            ],
        }
