"""
    pygments.lexers.mumps
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for MUMPS

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
from pygments.lexer import ExtendedRegexLexer, words, bygroups, default, include
from pygments.token import Whitespace, Keyword, Operator, Number, Name, Punctuation, Comment, String

__all__ = ['MumpsLexer']

class MumpsLexer(ExtendedRegexLexer):
    """
    Lexer for ANSI-standard M[UMPS].

    """
    url = "https://en.wikipedia.org/wiki/MUMPS"
    name = 'MUMPS'
    aliases = ['mumps', 'm']
    # Filenames aren't meaningful in M, but some implementations allow file export/import of routines
    # For example, YottaDB would have the source file for "dmex" in "dmex.m"
    filenames = ['*.m', '*.mumps', '*.epc', '*.int']
    flags = re.IGNORECASE
    # Section numbers below are as per the 1995 standard available at `The Annotated M[UMPS] Standard <http://71.174.62.16/Demo/AnnoStd>`

    # 5 - Metalanguage description
    # 'L': list of one or more, separated by commas
    def L(state):
        return [default(('list_comma', state))]

    # Definitions of groups that we implement as regular expressions
    # 6.1 - Routine head 'routinehead'
    name_re = '[%A-Za-z][A-Za-z0-9]*'
    # 7.1.4.1 - String literal 'strlit'
    strlit_re = '"(""|[^"])*"'
    # 7.1.4.10 - Intrinsic special variables 'svn'
    svn_re = words(('$DEVICE', '$D', '$ECODE', '$EC', '$ESTACK', '$ES', '$ETRAP', '$ET', '$HOROLOG', '$H', '$IO', '$I', '$JOB', '$J', '$KEY', '$K', '$PRINCIPAL', '$P', '$QUIT', '$Q', '$STACK', '$ST', '$STORAGE', '$S', '$SYSTEM', '$SY', '$TEST', '$T', '$TLEVEL', '$TL', '$TRESTART', '$TR', '$X', '$Y'), suffix=r'\b(?!\()')

    def analyse_text(text):
        if re.search(r'^[%A-Za-z][A-Za-z0-9]* ;', text):
            return 0.3
        if re.search(r'^[%A-Za-z][A-Za-z0-9]*\n', text):
            return 0.1

    # Parsing states
    tokens = {
        'root': [
            ('^' + name_re + r'(?=\n)', Name.Namespace),
            default(('#pop', 'linebody', 'linestart'))
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
        'routinename': [
            (name_re, Name.Namespace, '#pop'),
        ],
        # 6.2 - Routine body 'routinebody'
        'linestart': [
            (r'(' + name_re + r')(\()(\))( +)', bygroups(Name.Function, Punctuation, Punctuation, Whitespace), '#pop'), # 6.2.2 formalline
            (r'(' + name_re + r')(\()', bygroups(Name.Function, Punctuation), ('#pop', 'ls', 'close_paren', 'l_name')),
            (r'(' + name_re + r')( +)', bygroups(Name.Label, Whitespace), ('#pop', 'li_chain')), #6.2.3 - Label
            (r' +', Whitespace, ('#pop', 'li_chain')),
        ],
        'li_chain': [
            (r'(\.)( +)', bygroups(Punctuation, Whitespace)),
            (r'\.', Punctuation),
            default('#pop')
        ],
        'l_name': [
            (r'(' + name_re + r')(,)', bygroups(Name.Variable, Punctuation)),
            (name_re, Name.Variable, '#pop'),
        ],
        # 6.2.4 - Line separator 'ls'
        'ls': [
            ( ' +', Whitespace, '#pop')
        ],
        # 6.2.5 - Line body 'linebody'
        'linebody': [
            (';.*', Comment, '#pop'),
            (' +', Whitespace),
            default('command')
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
            ( r'(' + name_re + r')(\()' , bygroups( Name.Variable, Punctuation), ('#pop', 'close_paren', 'l_expr')),
            ( name_re , Name.Variable, '#pop'),
            ('@', Operator, ('#pop', 'namind_subscripts', 'rexpratom'))
        ],
        # 7.1.2.4 - Global variable name gvn
        'gvn': [
            include('rgvn'),
            ('@', Operator, ('#pop', 'namind_subscripts', 'rexpratom'))
        ],
        'rgvn': [
            (r'(\^)(\()', bygroups(Name.Variable.Global, Punctuation), ('#pop', 'close_paren', 'l_expr')),
            (r'(\^)(\|)', bygroups(Name.Variable.Global, Punctuation), ('#pop', 'opt_subscripts', 'gname', 'vert_bar', 'expr')),
            (r'(\^' + name_re + r')(\()', bygroups( Name.Variable.Global, Punctuation), ('#pop', 'close_paren', 'l_expr')),
            (r'\^' + name_re, Name.Variable.Global, '#pop')
        ],
        'vert_bar': [
            (r'\|', Punctuation, '#pop'),
        ],
        'gname': [
            (name_re, Name.Variable.Global, '#pop'),
        ],
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
        # Name indirection
        'namind_subscripts': [
            (r'(@)(\()', bygroups(Operator, Punctuation), ('#pop', 'close_paren', 'l_expr')),
            default('#pop'),
        ],
        'rexpratom': [
            ( r'(' + name_re + r')(\()' , bygroups(Name.Variable, Punctuation), ('#pop', 'close_paren', 'l_expr')),
            ( name_re , Name.Variable, '#pop'),
            include('rgvn'),
            include('expritem')
        ],
        # 7.1.3 - Structured system variable ssvn
        'ssvn': [
            (r'\^\$[A-Z]+', Name.Variable.Magic, ('#pop', 'subscripts')),
        ],
        # 7.1.4 - Expression item 'expritem'
        'expritem': [
            ( strlit_re , String, '#pop'),
            # 7.1.4.2 - Numeric literals
            (r'[0-9]*\.[0-9]+E[+-]?[0-9]+', Number, '#pop'),
            (r'[0-9]*\.[0-9]+', Number, '#pop'),
            ('[0-9]+E[+-]?[0-9]+', Number, '#pop'),
            ('[0-9]+', Number, '#pop'),
            (r'\$\$', Punctuation, ('#pop', 'opt_actuallist', 'labelref_func')),
            (r'(\$)(&)', bygroups(Punctuation, Punctuation), ('#pop', 'opt_actuallist', 'labelref_func', 'opt_packagename')),
            (svn_re, Name.Variable.Magic, '#pop'),
            include('function'),
            ( "[-+']", Operator, ('#pop', 'expratom')),
            ( r'\(', Punctuation, ('#pop', 'close_paren', 'expr')),
        ],
        # 7.1.5 - Intrinsic function function
        'function': [
            (r'(\$ASCII|\$A)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr')),
            (r'(\$CHAR|\$C)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'l_expr')),
            (r'(\$DATA|\$D)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'glvn')),
            (r'(\$EXTRACT|\$E)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'opt_param_comma', 'expr')),
            (r'(\$FIND|\$F)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'comma', 'expr')),
            (r'(\$FNUMBER|\$FN)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'comma', 'expr')),
            (r'(\$GET|\$G)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'glvn')),
            (r'(\$JUSTIFY|\$J)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'comma', 'expr')),
            (r'(\$LENGTH|\$L)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr')),
            (r'(\$NAME|\$NA)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'glvn')),
            (r'(\$ORDER|\$O)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'glvn')),
            (r'(\$PIECE|\$P)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'opt_param_comma', 'expr', 'comma', 'expr')),
            (r'(\$QLENGTH|\$QL)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr')),
            (r'(\$QSUBSCRIPT|\$QS)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'comma', 'expr')),
            (r'(\$QUERY|\$Q)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'glvn')),
            (r'(\$RANDOM|\$R)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr')),
            (r'(\$REVERSE|\$RE)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr')),
            (r'(\$SELECT|\$S)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'l_selectatom')),
            (r'(\$STACK|\$ST)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr')),
            (r'(\$TEXT|\$T)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'textarg')),
            (r'(\$TRANSLATE|\$TR)(\()', bygroups(Name.Function, Punctuation), ('#pop', 'close_paren', 'expr', 'opt_param_comma', 'expr', 'comma', 'expr')),
            (r'(\$VIEW|$V)(\()(\))', bygroups(Name.Function, Punctuation, Punctuation), '#pop'),
        ],
        'opt_param_comma': [
            include('comma'),
            default('#pop:2')
        ],
        'l_selectatom': L('selectatom'),
        'selectatom': [
            default(('#pop', 'expr', 'colon', 'expr'))
        ],
        # 7.1.5.20 - $TEXT
        'textarg': [
            (r'\+', Operator, ('#pop', 'opt_routineref', 'expr')),
            include('entryref')
        ],
        # 7.2 - Expression tail 'exprtail'
        'exprtail': [
            ( r'\*\*|[-_+*/\\#]', Operator, ('#pop', 'expratom')), # 7.2.2 - Binaryop
            ( r'\]\]|[&!<=>\[\]]', Operator, ('#pop', 'expratom')), # 7.2.2 - Truthop
            ( r'(\?)(@)', bygroups(Operator, Operator), ('#pop', 'expratom')), # 7.2.3 - Pattern
            ( r'\?', Operator, ('#pop', 'more_patatoms', 'patatom')),
            ( r'(\')(\]\]|[&!<=>\[\]])', bygroups(Operator, Operator), ('#pop', 'expratom')),
            ( r'(\')(\?)(@)', bygroups(Operator, Operator, Operator), ('#pop', 'expratom')), # 7.2.3 - Pattern
            ( r'(\')(\?)', bygroups(Operator, Operator), ('#pop', 'more_patatoms', 'patatom')),
        ],
        'patatom': [
            ( r'([0-9]+)(\.)([0-9]*)', bygroups(Number, Punctuation, Number), ('#pop', 'patatom_choice')),
            ( r'(\.)([0-9]*)', bygroups(Punctuation, Number), ('#pop', 'patatom_choice')),
            ( '[0-9]+', Number, ('#pop', 'patatom_choice')),
        ],
	'l_patatom': L('patatom'),
        'more_patatoms': [
            ( r'([0-9]+)(\.)([0-9]*)', bygroups(Number, Punctuation, Number), 'patatom_choice'),
            ( r'(\.)([0-9]*)', bygroups(Punctuation, Number), 'patatom_choice'),
            ( '[0-9]+', Number, 'patatom_choice'),
            default('#pop')
        ],
        'patatom_choice': [
            (r'\(', Punctuation, ('#pop', 'close_paren', 'l_patatom')),
            ( strlit_re , String, '#pop'),
            ('[A-Xa-x]+', Name.Entity, '#pop'),
            ('Y[A-XZa-xz]*Y', Name.Entity, '#pop'),
            ('Z[A-Ya-y]*Z', Name.Entity, '#pop'),
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
            (r'\|', Punctuation, ('#pop', 'routinename', 'vert_bar', 'expr')),
            ( name_re, Name.Namespace, '#pop'),
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
            (r'\|', Punctuation, ('#pop', 'vert_bar', 'expr')),
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
            (r'(\.)(@)', bygroups(Punctuation, Operator), ('#pop', 'expratom')),
            (r'(\.)(' + name_re + r')', bygroups(Punctuation, Name.Variable), '#pop'),
            default(('#pop', 'expr')),
        ],
        # 8.2 - Command
        'command': [
            (words(('break', 'b'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
            (words(('close', 'c'), suffix=r'\b'), Keyword, ('#pop', 'l_closearg', 'argumentsp', 'postcond')),
            (r'do?\b', Keyword, ('#pop', 'l_doargument', 'optargsp', 'postcond')),
            (words(('else', 'e'), suffix=r'\b'), Keyword, ('#pop', 'noargsp')),
            (words(('for', 'f'), suffix=r'\b'), Keyword, ('#pop', 'for_argument', 'optargsp')),
            (words(('goto', 'g'), suffix=r'\b'), Keyword, ('#pop', 'l_gotoargument', 'argumentsp', 'postcond')),
            (r'halt\b', Keyword, ('#pop', 'noargsp')),
            (r'hang\b', Keyword, ('#pop', 'l_expr', 'argumentsp', 'postcond')),
            (r'h\b', Keyword, ('#pop', 'l_expr', 'optargsp', 'postcond')), # halt or hang, depending or argument
            (r'if?\b', Keyword, ('#pop', 'l_expr', 'optargsp')),
            (words(('job', 'j'), suffix=r'\b'), Keyword, ('#pop', 'l_jobargument', 'argumentsp', 'postcond')),
            (words(('kill', 'k'), suffix=r'\b'), Keyword, ('#pop', 'l_killargument', 'optargsp', 'postcond')),
            (words(('lock', 'l'), suffix=r'\b'), Keyword, ('#pop', 'l_lockargument', 'optargsp', 'postcond')),
            (words(('merge', 'm'), suffix=r'\b'), Keyword, ('#pop', 'l_mergeargument', 'argumentsp', 'postcond')),
            (words(('new', 'n'), suffix=r'\b'), Keyword, ('#pop', 'l_newargument', 'optargsp', 'postcond')),
            (words(('open', 'o'), suffix=r'\b'), Keyword, ('#pop', 'l_openargument', 'argumentsp', 'postcond')),
            (words(('quit', 'q'), suffix=r'\b'), Keyword, ('#pop', 'expr_or_indirect', 'optargsp', 'postcond')),
            (words(('read', 'r'), suffix=r'\b'), Keyword, ('#pop', 'l_readargument', 'argumentsp', 'postcond')),
            (words(('set', 's'), suffix=r'\b'), Keyword, ('#pop', 'l_setargument', 'argumentsp', 'postcond')),
            (words(('tcommit', 'tc'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
            (words(('trestart', 'tre'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
            (words(('trollback', 'tro'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
            (words(('tstart', 'ts'), suffix=r'\b'), Keyword, ('#pop', 'tstartargument', 'optargsp', 'postcond')),
            (words(('use', 'u'), suffix=r'\b'), Keyword, ('#pop', 'expr', 'colon_sep', 'deviceparameters', 'colon_sep', 'expr', 'argumentsp', 'postcond')),
            (words(('view', 'v'), suffix=r'\b'), Keyword, ('#pop', 'noargsp', 'postcond')),
            (words(('write', 'w'), suffix=r'\b'), Keyword, ('#pop', 'l_writeargument', 'argumentsp', 'postcond')),
            (words(('xecute', 'x'), suffix=r'\b'), Keyword, ('#pop', 'l_xargument', 'argumentsp', 'postcond')),
        ],
        # 8.2.2 - CLOSE
        'closearg': [
            ('@', Operator, ('#pop', 'expratom')),
            default(('#pop', 'deviceparameters', 'colon_sep', 'expr')),
        ],
        'deviceparameters': [
            (r'\(', Punctuation, ('#pop', 'close_paren', 'colon_expr_list', 'expr')),
            include('expr'),
        ],
        'colon_expr_list': [
            (':', Punctuation, 'expr'),
            default('#pop'),
        ],
        'colon_group': [
            include('colon'),
            (r'\)', Punctuation, '#pop:2'),
        ],
        'l_closearg': L('closearg'),
        # 8.2.3 - DO
        'doargument': [
            default(('#pop', 'postcond', 'opt_actuallist', 'entryref_or_externref')),
        ],
        'entryref_or_externref': [
            include('externref'),
            include('entryref'),
        ],
        'l_doargument': L('doargument'),
        'opt_actuallist': [
            include('actuallist'),
            default('#pop'),
        ],
        # 8.2.5 - FOR
        'for_argument': [
            default(('#pop', 'expr', 'colon_sep', 'expr', 'colon_sep', 'expr', 'equals', 'lvn')),
        ],
        'equals': [
            ('=', Operator, '#pop'),
        ],
        # 8.2.6 - GOTO
        'gotoargument': [
            default(('#pop', 'postcond', 'entryref')),
        ],
        'l_gotoargument': L('gotoargument'),
        # 8.2.10 - JOB
        'jobargument': [
            default(('#pop', 'jobparameters', 'colon_sep', 'opt_actuallist', 'entryref_or_externref')),
        ],
        'l_jobargument': L('jobargument'),
        'jobparameters': [
            default(('#pop', 'timeout', 'processparameters'))
        ],
        'processparameters': [
            (r'\(', Punctuation, ('#pop', 'close_paren', 'colon_expr_list', 'expr')),
            include('expr'),
        ],
        # 8.2.11 - KILL
        'killargument': [
            (r'\(', Punctuation, ('#pop', 'close_paren', 'l_lname')),
            ('@', Operator, ('#pop', 'expratom')),
            include('glvn'),
        ],
        'l_killargument': L('killargument'),
        'lname': [
            ('@', Operator, ('#pop', 'expratom')),
            (name_re, Name.Variable, '#pop'),
        ],
        'l_lname': L('lname'),
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
            (r'(\^' + name_re + r')(\()', bygroups(Name, Punctuation), ('#pop', 'close_paren', 'l_expr')),
            (r'\^' + name_re , Name, '#pop'),
            (r'(' + name_re + r')(\()', bygroups(Name, Punctuation), ('#pop', 'close_paren', 'l_expr')),
            (name_re , Name, '#pop'),
        ],
        'l_nref': L('nref'),
        # 8.2.13 - MERGE
        'mergeargument': [
            ('@', Operator, ('#pop', 'mergearg_post_indirect', 'expratom')),
            default(('#pop', 'glvn', 'equals', 'glvn'))
        ],
        'l_mergeargument': L('mergeargument'),
        'mergearg_post_indirect': [
            ('=', Operator, ('#pop', 'glvn')), # Indirected variable name, no subscripts
            ('@', Operator, ('#pop', 'glvn', 'equals', 'subscripts')), # indirected variable, with subscripts
            default('#pop')
        ],
        # 8.2.14 - NEW
        'newargument': [
            (r'\(', Punctuation, ('#pop', 'close_paren', 'l_lname')),
            (words(('$ETRAP', '$ET', '$ESTACK', '$ES'), suffix=r'\b'), Name.Variable.Magic, '#pop'),
            include('lname')
        ],
        'l_newargument': L('newargument'),
        # 8.2.15 - OPEN
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
            (r'\(', Punctuation, ('#pop', 'close_paren', 'l_expr')),
            default(('#pop', 'expr'))
        ],
        # 8.2.16 - QUIT
        'expr_or_indirect': [
            ('@', Operator, ('#pop', 'expratom')),
            include('expr')
        ],
        # 8.2.17 - READ
        'readargument': [
            include('format'),
            ( strlit_re , String, '#pop'),
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
        'opt_readcount': [
            ('#', Punctuation, ('#pop', 'expr')),
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
        'l_setargument': L('setargument'),
        'setdestination': [
            (r'\(', Punctuation, ('#pop', 'close_paren', 'l_setleft')),
            default(('#pop', 'setleft'))
        ],
        'setleft': [
            (words(('$DEVICE', '$D', '$KEY', '$K', '$X', '$Y'), suffix=r'\b'), Name.Variable.Magic, '#pop'),
            (words(('$PIECE', '$P'), suffix=r'\b'), Name.Function.Magic, ('#pop', 'close_paren', 'l_expr', 'comma', 'glvn', 'open_paren')),
            (words(('$EXTRACT', '$E'), suffix=r'\b'), Name.Function.Magic, ('#pop', 'close_paren', 'l_expr', 'comma', 'glvn', 'open_paren')),
            (words(('$ECODE', '$EC'), suffix=r'\b'), Name.Variable.Magic, '#pop'),
            (words(('$ETRAP', '$ET'), suffix=r'\b'), Name.Variable.Magic, '#pop'),
            include('glvn')
        ],
        'l_setleft': L('setleft'),
        # 8.2.22 - TSTART
        'tstartargument': [
            (r'(\*)(:)', bygroups(Keyword.Pseudo, Punctuation), ('#pop', 'transparameters')),
            (r'(\()(\))(:)', bygroups(Punctuation, Punctuation, Punctuation), ('#pop', 'transparameters')),
            (r'\*', Keyword.Pseudo, '#pop'),
            (r'(\()(\))', bygroups(Punctuation, Punctuation), '#pop'),
            (r'\(', Punctuation, ('#pop', 'opt_transparameters', 'close_paren', 'l_lname')),
            ('@', Operator, ('#pop', 'opt_transparameters', 'expratom')),
            (r':', Punctuation, ('#pop', 'transparameters')),
            default(('#pop', 'opt_transparameters', 'lname'))
        ],
        'opt_transparameters': [
            (':', Punctuation, ('#pop', 'transparameters')),
            default('#pop')
        ],
        'transparameters': [
            (r'\(', Punctuation, ('#pop', 'close_paren', 'colon_tsparam_list', 'tsparam')),
            include('tsparam'),
        ],
        'colon_tsparam_list': [
            (':', Punctuation, 'tsparam'), 
            default('#pop'),
        ],
        'tsparam': [
            (r'([A-Z]+)(=)', bygroups( Keyword, Operator ), ('#pop', 'expr')),
            (r'[A-Z]+', Keyword, '#pop')
        ],
        'restartargument': [
            (r'\*', Keyword.Pseudo, '#pop'),
            (r'(\()(\))', bygroups(Punctuation, Punctuation), '#pop'),
            (r'\(', Punctuation, ('#pop', 'close_paren', 'l_lname')),
            include('lname')
        ],
        # 8.2.25 - WRITE
        'l_writeargument': L('writeargument'),
        'writeargument': [
            include('format'),
            (r'\*', Keyword.Pseudo, ('#pop', 'expr')),
            include('expr'),
        ],
        # 8.2.26 - XECUTE
        'l_xargument': L('xargument'),
        'xargument': [
            default(('#pop', 'postcond', 'expr'))
        ],
    }
