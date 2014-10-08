# -*- coding: utf-8 -*-
"""
    pygments.lexers.int_fiction
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for interactive fiction languages.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, using, \
    this, default, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Error, Generic

__all__ = ['Inform6Lexer', 'Inform6TemplateLexer', 'Inform7Lexer']


class Inform6Lexer(RegexLexer):
    """
    For `Inform 6 <http://inform-fiction.org/>`_ source code.

    .. versionadded:: 2.0
    """

    name = 'Inform 6'
    aliases = ['inform6', 'i6']
    filenames = ['*.inf']

    flags = re.MULTILINE | re.DOTALL | re.UNICODE

    _name = r'[a-zA-Z_][a-zA-Z_0-9]*'

    # Inform 7 maps these four character classes to their ASCII
    # equivalents. To support Inform 6 inclusions within Inform 7,
    # Inform6Lexer maps them too.
    _dash = u'\\-\u2010-\u2014'
    _dquote = u'"\u201c\u201d'
    _squote = u"'\u2018\u2019"
    _newline = u'\\n\u0085\u2028\u2029'

    tokens = {
        'root': [
            (r'(\A(!%%[^%s]*[%s])+)?' % (_newline, _newline), Comment.Preproc,
             'directive')
        ],
        '_whitespace': [
            (r'\s+', Text),
            (r'![^%s]*' % _newline, Comment.Single)
        ],
        'default': [
            include('_whitespace'),
            (r'\[', Punctuation, 'many-values'),  # Array initialization
            (r':|(?=;)', Punctuation, '#pop'),
            (r'<', Punctuation),  # Second angle bracket in an action statement
            default(('expression', '_expression'))
        ],

        # Expressions
        '_expression': [
            include('_whitespace'),
            (r'(?=sp\b)', Text, '#pop'),
            (r'(?=[%s%s$0-9#a-zA-Z_])' % (_dquote, _squote), Text,
             ('#pop', 'value')),
            (r'\+\+|[%s]{1,2}(?!>)|~~?' % _dash, Operator),
            (r'(?=[()\[%s,?@{:;])' % _dash, Text, '#pop')
        ],
        'expression': [
            include('_whitespace'),
            (r'\(', Punctuation, ('expression', '_expression')),
            (r'\)', Punctuation, '#pop'),
            (r'\[', Punctuation, ('#pop', 'statements', 'locals')),
            (r'>(?=(\s+|(![^%s]*))*[>;])' % _newline, Punctuation),
            (r'\+\+|[%s]{2}(?!>)' % _dash, Operator),
            (r',', Punctuation, '_expression'),
            (r'&&?|\|\|?|[=~><]?=|[%s]{1,2}>?|\.\.?[&#]?|::|[<>+*/%%]' % _dash,
             Operator, '_expression'),
            (r'(has|hasnt|in|notin|ofclass|or|provides)\b', Operator.Word,
             '_expression'),
            (r'sp\b', Name),
            (r'\?~?', Name.Label, 'label?'),
            (r'[@{]', Error),
            default('#pop')
        ],
        '_assembly-expression': [
            (r'\(', Punctuation, ('#push', '_expression')),
            (r'[\[\]]', Punctuation),
            (r'[%s]>' % _dash, Punctuation, '_expression'),
            (r'sp\b', Keyword.Pseudo),
            (r';', Punctuation, '#pop:3'),
            include('expression')
        ],
        '_for-expression': [
            (r'\)', Punctuation, '#pop:2'),
            (r':', Punctuation, '#pop'),
            include('expression')
        ],
        '_keyword-expression': [
            (r'(from|near|to)\b', Keyword, '_expression'),
            include('expression')
        ],
        '_list-expression': [
            (r',', Punctuation, '#pop'),
            include('expression')
        ],
        '_object-expression': [
            (r'has\b', Keyword.Declaration, '#pop'),
            include('_list-expression')
        ],

        # Values
        'value': [
            include('_whitespace'),
            # Strings
            (r'[%s][^@][%s]' % (_squote, _squote), String.Char, '#pop'),
            (r'([%s])(@{[0-9a-fA-F]{1,4}})([%s])' % (_squote, _squote),
             bygroups(String.Char, String.Escape, String.Char), '#pop'),
            (r'([%s])(@..)([%s])' % (_squote, _squote),
             bygroups(String.Char, String.Escape, String.Char), '#pop'),
            (r'[%s]' % _squote, String.Single, ('#pop', 'dictionary-word')),
            (r'[%s]' % _dquote, String.Double, ('#pop', 'string')),
            # Numbers
            (r'\$[+%s][0-9]*\.?[0-9]*([eE][+%s]?[0-9]+)?' % (_dash, _dash),
             Number.Float, '#pop'),
            (r'\$[0-9a-fA-F]+', Number.Hex, '#pop'),
            (r'\$\$[01]+', Number.Bin, '#pop'),
            (r'[0-9]+', Number.Integer, '#pop'),
            # Values prefixed by hashes
            (r'(##|#a\$)(%s)' % _name, bygroups(Operator, Name), '#pop'),
            (r'(#g\$)(%s)' % _name,
             bygroups(Operator, Name.Variable.Global), '#pop'),
            (r'#[nw]\$', Operator, ('#pop', 'obsolete-dictionary-word')),
            (r'(#r\$)(%s)' % _name, bygroups(Operator, Name.Function), '#pop'),
            (r'#', Name.Builtin, ('#pop', 'system-constant')),
            # System functions
            (words((
                'child', 'children', 'elder', 'eldest', 'glk', 'indirect', 'metaclass',
                'parent', 'random', 'sibling', 'younger', 'youngest'), suffix=r'\b'),
             Name.Builtin, '#pop'),
            # Metaclasses
            (r'(?i)(Class|Object|Routine|String)\b', Name.Builtin, '#pop'),
            # Veneer routines
            (words((
                'Box__Routine', 'CA__Pr', 'CDefArt', 'CInDefArt', 'Cl__Ms',
                'Copy__Primitive', 'CP__Tab', 'DA__Pr', 'DB__Pr', 'DefArt', 'Dynam__String',
                'EnglishNumber', 'Glk__Wrap', 'IA__Pr', 'IB__Pr', 'InDefArt', 'Main__',
                'Meta__class', 'OB__Move', 'OB__Remove', 'OC__Cl', 'OP__Pr', 'Print__Addr',
                'Print__PName', 'PrintShortName', 'RA__Pr', 'RA__Sc', 'RL__Pr', 'R_Process',
                'RT__ChG', 'RT__ChGt', 'RT__ChLDB', 'RT__ChLDW', 'RT__ChPR', 'RT__ChPrintA',
                'RT__ChPrintC', 'RT__ChPrintO', 'RT__ChPrintS', 'RT__ChPS', 'RT__ChR',
                'RT__ChSTB', 'RT__ChSTW', 'RT__ChT', 'RT__Err', 'RT__TrPS', 'RV__Pr',
                'Symb__Tab', 'Unsigned__Compare', 'WV__Pr', 'Z__Region'),
                prefix='(?i)', suffix=r'\b'),
             Name.Builtin, '#pop'),
            # Other built-in symbols
            (words((
                'call', 'copy', 'create', 'DEBUG', 'destroy', 'DICT_CHAR_SIZE',
                'DICT_ENTRY_BYTES', 'DICT_IS_UNICODE', 'DICT_WORD_SIZE', 'false',
                'FLOAT_INFINITY', 'FLOAT_NAN', 'FLOAT_NINFINITY', 'GOBJFIELD_CHAIN',
                'GOBJFIELD_CHILD', 'GOBJFIELD_NAME', 'GOBJFIELD_PARENT',
                'GOBJFIELD_PROPTAB', 'GOBJFIELD_SIBLING', 'GOBJ_EXT_START',
                'GOBJ_TOTAL_LENGTH', 'Grammar__Version', 'INDIV_PROP_START', 'INFIX',
                'infix__watching', 'MODULE_MODE', 'name', 'nothing', 'NUM_ATTR_BYTES', 'print',
                'print_to_array', 'recreate', 'remaining', 'self', 'sender', 'STRICT_MODE',
                'sw__var', 'sys__glob0', 'sys__glob1', 'sys__glob2', 'sys_statusline_flag',
                'TARGET_GLULX', 'TARGET_ZCODE', 'temp__global2', 'temp__global3',
                'temp__global4', 'temp_global', 'true', 'USE_MODULES', 'WORDSIZE'),
                prefix='(?i)', suffix=r'\b'),
             Name.Builtin, '#pop'),
            # Other values
            (_name, Name, '#pop')
        ],
        # Strings
        'dictionary-word': [
            (r'[~^]+', String.Escape),
            (r'[^~^\\@({%s]+' % _squote, String.Single),
            (r'[({]', String.Single),
            (r'@{[0-9a-fA-F]{,4}}', String.Escape),
            (r'@..', String.Escape),
            (r'[%s]' % _squote, String.Single, '#pop')
        ],
        'string': [
            (r'[~^]+', String.Escape),
            (r'[^~^\\@({%s]+' % _dquote, String.Double),
            (r'[({]', String.Double),
            (r'\\', String.Escape),
            (r'@(\\\s*[%s]\s*)*@((\\\s*[%s]\s*)*[0-9])*' %
             (_newline, _newline), String.Escape),
            (r'@(\\\s*[%s]\s*)*{((\\\s*[%s]\s*)*[0-9a-fA-F]){,4}'
             r'(\\\s*[%s]\s*)*}' % (_newline, _newline, _newline),
             String.Escape),
            (r'@(\\\s*[%s]\s*)*.(\\\s*[%s]\s*)*.' % (_newline, _newline),
             String.Escape),
            (r'[%s]' % _dquote, String.Double, '#pop')
        ],
        'plain-string': [
            (r'[^~^\\({\[\]%s]+' % _dquote, String.Double),
            (r'[~^({\[\]]', String.Double),
            (r'\\', String.Escape),
            (r'[%s]' % _dquote, String.Double, '#pop')
        ],
        # Names
        '_constant': [
            include('_whitespace'),
            (_name, Name.Constant, '#pop'),
            include('value')
        ],
        '_global': [
            include('_whitespace'),
            (_name, Name.Variable.Global, '#pop'),
            include('value')
        ],
        'label?': [
            include('_whitespace'),
            (r'(%s)?' % _name, Name.Label, '#pop')
        ],
        'variable?': [
            include('_whitespace'),
            (r'(%s)?' % _name, Name.Variable, '#pop')
        ],
        # Values after hashes
        'obsolete-dictionary-word': [
            (r'\S[a-zA-Z_0-9]*', String.Other, '#pop')
        ],
        'system-constant': [
            include('_whitespace'),
            (_name, Name.Builtin, '#pop')
        ],

        # Directives
        'directive': [
            include('_whitespace'),
            (r'#', Punctuation),
            (r';', Punctuation, '#pop'),
            (r'\[', Punctuation,
             ('default', 'statements', 'locals', 'routine-name?')),
            (words((
                'abbreviate', 'endif', 'dictionary', 'ifdef', 'iffalse', 'ifndef', 'ifnot',
                'iftrue', 'ifv3', 'ifv5', 'release', 'serial', 'switches', 'system_file',
                'version'), prefix='(?i)', suffix=r'\b'),
             Keyword, 'default'),
            (r'(?i)(array|global)\b', Keyword,
             ('default', 'directive-keyword?', '_global')),
            (r'(?i)attribute\b', Keyword, ('default', 'alias?', '_constant')),
            (r'(?i)class\b', Keyword,
             ('object-body', 'duplicates', 'class-name')),
            (r'(?i)(constant|default)\b', Keyword,
             ('default', 'expression', '_constant')),
            (r'(?i)(end\b)(.*)', bygroups(Keyword, Text)),
            (r'(?i)(extend|verb)\b', Keyword, 'grammar'),
            (r'(?i)fake_action\b', Keyword, ('default', '_constant')),
            (r'(?i)import\b', Keyword, 'manifest'),
            (r'(?i)(include|link)\b', Keyword,
             ('default', 'before-plain-string')),
            (r'(?i)(lowstring|undef)\b', Keyword, ('default', '_constant')),
            (r'(?i)message\b', Keyword, ('default', 'diagnostic')),
            (r'(?i)(nearby|object)\b', Keyword,
             ('object-body', '_object-head')),
            (r'(?i)property\b', Keyword,
             ('default', 'alias?', '_constant', 'property-keyword*')),
            (r'(?i)replace\b', Keyword,
             ('default', 'routine-name?', 'routine-name?')),
            (r'(?i)statusline\b', Keyword, ('default', 'directive-keyword?')),
            (r'(?i)stub\b', Keyword, ('default', 'routine-name?')),
            (r'(?i)trace\b', Keyword,
             ('default', 'trace-keyword?', 'trace-keyword?')),
            (r'(?i)zcharacter\b', Keyword,
             ('default', 'directive-keyword?', 'directive-keyword?')),
            (_name, Name.Class, ('object-body', '_object-head'))
        ],
        # [, Replace, Stub
        'routine-name?': [
            include('_whitespace'),
            (r'(%s)?' % _name, Name.Function, '#pop')
        ],
        'locals': [
            include('_whitespace'),
            (r';', Punctuation, '#pop'),
            (r'\*', Punctuation),
            (_name, Name.Variable)
        ],
        # Array
        'many-values': [
            include('_whitespace'),
            (r';', Punctuation),
            (r'\]', Punctuation, '#pop'),
            (r':', Error),
            default(('expression', '_expression'))
        ],
        # Attribute, Property
        'alias?': [
            include('_whitespace'),
            (r'alias\b', Keyword, ('#pop', '_constant')),
            default('#pop')
        ],
        # Class, Object, Nearby
        'class-name': [
            include('_whitespace'),
            (r'(?=[,;]|(class|has|private|with)\b)', Text, '#pop'),
            (_name, Name.Class, '#pop')
        ],
        'duplicates': [
            include('_whitespace'),
            (r'\(', Punctuation, ('#pop', 'expression', '_expression')),
            default('#pop')
        ],
        '_object-head': [
            (r'[%s]>' % _dash, Punctuation),
            (r'(class|has|private|with)\b', Keyword.Declaration, '#pop'),
            include('_global')
        ],
        'object-body': [
            include('_whitespace'),
            (r';', Punctuation, '#pop:2'),
            (r',', Punctuation),
            (r'class\b', Keyword.Declaration, 'class-segment'),
            (r'(has|private|with)\b', Keyword.Declaration),
            (r':', Error),
            default(('_object-expression', '_expression'))
        ],
        'class-segment': [
            include('_whitespace'),
            (r'(?=[,;]|(class|has|private|with)\b)', Text, '#pop'),
            (_name, Name.Class),
            default('value')
        ],
        # Extend, Verb
        'grammar': [
            include('_whitespace'),
            (r'=', Punctuation, ('#pop', 'default')),
            (r'\*', Punctuation, ('#pop', 'grammar-line')),
            default('_directive-keyword')
        ],
        'grammar-line': [
            include('_whitespace'),
            (r';', Punctuation, '#pop'),
            (r'[/*]', Punctuation),
            (r'[%s]>' % _dash, Punctuation, 'value'),
            (r'(noun|scope)\b', Keyword, '=routine'),
            default('_directive-keyword')
        ],
        '=routine': [
            include('_whitespace'),
            (r'=', Punctuation, 'routine-name?'),
            default('#pop')
        ],
        # Import
        'manifest': [
            include('_whitespace'),
            (r';', Punctuation, '#pop'),
            (r',', Punctuation),
            (r'(?i)(global\b)?', Keyword, '_global')
        ],
        # Include, Link, Message
        'diagnostic': [
            include('_whitespace'),
            (r'[%s]' % _dquote, String.Double, ('#pop', 'message-string')),
            default(('#pop', 'before-plain-string', 'directive-keyword?'))
        ],
        'before-plain-string': [
            include('_whitespace'),
            (r'[%s]' % _dquote, String.Double, ('#pop', 'plain-string'))
        ],
        'message-string': [
            (r'[~^]+', String.Escape),
            include('plain-string')
        ],

        # Keywords used in directives
        '_directive-keyword!': [
            include('_whitespace'),
            (words((
                'additive', 'alias', 'buffer', 'class', 'creature', 'data', 'error', 'fatalerror',
                'first', 'has', 'held', 'initial', 'initstr', 'last', 'long', 'meta', 'multi',
                'multiexcept', 'multiheld', 'multiinside', 'noun', 'number', 'only', 'private',
                'replace', 'reverse', 'scope', 'score', 'special', 'string', 'table', 'terminating',
                'time', 'topic', 'warning', 'with'), suffix=r'\b'),
             Keyword, '#pop'),
            (r'[%s]{1,2}>|[+=]' % _dash, Punctuation, '#pop')
        ],
        '_directive-keyword': [
            include('_directive-keyword!'),
            include('value')
        ],
        'directive-keyword?': [
            include('_directive-keyword!'),
            default('#pop')
        ],
        'property-keyword*': [
            include('_whitespace'),
            (r'(additive|long)\b', Keyword),
            default('#pop')
        ],
        'trace-keyword?': [
            include('_whitespace'),
            (words((
                'assembly', 'dictionary', 'expressions', 'lines', 'linker',
                'objects', 'off', 'on', 'symbols', 'tokens', 'verbs'), suffix=r'\b'),
             Keyword, '#pop'),
            default('#pop')
        ],

        # Statements
        'statements': [
            include('_whitespace'),
            (r'\]', Punctuation, '#pop'),
            (r'[;{}]', Punctuation),
            (words((
                'box', 'break', 'continue', 'default', 'give', 'inversion',
                'new_line', 'quit', 'read', 'remove', 'return', 'rfalse', 'rtrue',
                'spaces', 'string', 'until'), suffix=r'\b'),
             Keyword, 'default'),
            (r'(do|else)\b', Keyword),
            (r'(font|style)\b', Keyword,
             ('default', 'miscellaneous-keyword?')),
            (r'for\b', Keyword, ('for', '(?')),
            (r'(if|switch|while)', Keyword,
             ('expression', '_expression', '(?')),
            (r'(jump|save|restore)\b', Keyword, ('default', 'label?')),
            (r'objectloop\b', Keyword,
             ('_keyword-expression', 'variable?', '(?')),
            (r'print(_ret)?\b|(?=[%s])' % _dquote, Keyword, 'print-list'),
            (r'\.', Name.Label, 'label?'),
            (r'@', Keyword, 'opcode'),
            (r'#(?![agrnw]\$|#)', Punctuation, 'directive'),
            (r'<', Punctuation, 'default'),
            (r'(move\b)?', Keyword,
             ('default', '_keyword-expression', '_expression'))
        ],
        'miscellaneous-keyword?': [
            include('_whitespace'),
            (r'(bold|fixed|from|near|off|on|reverse|roman|to|underline)\b',
             Keyword, '#pop'),
            (r'(a|A|an|address|char|name|number|object|property|string|the|'
             r'The)\b(?=(\s+|(![^%s]*))*\))' % _newline, Keyword.Pseudo,
             '#pop'),
            (r'%s(?=(\s+|(![^%s]*))*\))' % (_name, _newline), Name.Function,
             '#pop'),
            default('#pop')
        ],
        '(?': [
            include('_whitespace'),
            (r'\(?', Punctuation, '#pop')
        ],
        'for': [
            include('_whitespace'),
            (r';?', Punctuation, ('_for-expression', '_expression'))
        ],
        'print-list': [
            include('_whitespace'),
            (r';', Punctuation, '#pop'),
            (r':', Error),
            default(('_list-expression', '_expression', '_list-expression', 'form'))
        ],
        'form': [
            include('_whitespace'),
            (r'\(', Punctuation, ('#pop', 'miscellaneous-keyword?')),
            default('#pop')
        ],

        # Assembly
        'opcode': [
            include('_whitespace'),
            (r'[%s]' % _dquote, String.Double, ('operands', 'plain-string')),
            (_name, Keyword, 'operands')
        ],
        'operands': [
            (r':', Error),
            default(('_assembly-expression', '_expression'))
        ]
    }

    def get_tokens_unprocessed(self, text):
        # 'in' is either a keyword or an operator.
        # If the token two tokens after 'in' is ')', 'in' is a keyword:
        #   objectloop(a in b)
        # Otherwise, it is an operator:
        #   objectloop(a in b && true)
        objectloop_queue = []
        objectloop_token_count = -1
        previous_token = None
        for index, token, value in RegexLexer.get_tokens_unprocessed(self,
                                                                     text):
            if previous_token is Name.Variable and value == 'in':
                objectloop_queue = [[index, token, value]]
                objectloop_token_count = 2
            elif objectloop_token_count > 0:
                if token not in Comment and token not in Text:
                    objectloop_token_count -= 1
                objectloop_queue.append((index, token, value))
            else:
                if objectloop_token_count == 0:
                    if objectloop_queue[-1][2] == ')':
                        objectloop_queue[0][1] = Keyword
                    while objectloop_queue:
                        yield objectloop_queue.pop(0)
                    objectloop_token_count = -1
                yield index, token, value
            if token not in Comment and token not in Text:
                previous_token = token
        while objectloop_queue:
            yield objectloop_queue.pop(0)


class Inform7Lexer(RegexLexer):
    """
    For `Inform 7 <http://inform7.com/>`_ source code.

    .. versionadded:: 2.0
    """

    name = 'Inform 7'
    aliases = ['inform7', 'i7']
    filenames = ['*.ni', '*.i7x']

    flags = re.MULTILINE | re.DOTALL | re.UNICODE

    _dash = Inform6Lexer._dash
    _dquote = Inform6Lexer._dquote
    _newline = Inform6Lexer._newline
    _start = r'\A|(?<=[%s])' % _newline

    # There are three variants of Inform 7, differing in how to
    # interpret at signs and braces in I6T. In top-level inclusions, at
    # signs in the first column are inweb syntax. In phrase definitions
    # and use options, tokens in braces are treated as I7. Use options
    # also interpret "{N}".
    tokens = {}
    token_variants = ['+i6t-not-inline', '+i6t-inline', '+i6t-use-option']

    for level in token_variants:
        tokens[level] = {
            '+i6-root': list(Inform6Lexer.tokens['root']),
            '+i6t-root': [  # For Inform6TemplateLexer
                (r'[^%s]*' % Inform6Lexer._newline, Comment.Preproc,
                 ('directive', '+p'))
            ],
            'root': [
                (r'(\|?\s)+', Text),
                (r'\[', Comment.Multiline, '+comment'),
                (r'[%s]' % _dquote, Generic.Heading,
                 ('+main', '+titling', '+titling-string')),
                default(('+main', '+heading?'))
            ],
            '+titling-string': [
                (r'[^%s]+' % _dquote, Generic.Heading),
                (r'[%s]' % _dquote, Generic.Heading, '#pop')
            ],
            '+titling': [
                (r'\[', Comment.Multiline, '+comment'),
                (r'[^%s.;:|%s]+' % (_dquote, _newline), Generic.Heading),
                (r'[%s]' % _dquote, Generic.Heading, '+titling-string'),
                (r'[%s]{2}|(?<=[\s%s])\|[\s%s]' % (_newline, _dquote, _dquote),
                 Text, ('#pop', '+heading?')),
                (r'[.;:]|(?<=[\s%s])\|' % _dquote, Text, '#pop'),
                (r'[|%s]' % _newline, Generic.Heading)
            ],
            '+main': [
                (r'(?i)[^%s:a\[(|%s]+' % (_dquote, _newline), Text),
                (r'[%s]' % _dquote, String.Double, '+text'),
                (r':', Text, '+phrase-definition'),
                (r'(?i)\bas\b', Text, '+use-option'),
                (r'\[', Comment.Multiline, '+comment'),
                (r'(\([%s])(.*?)([%s]\))' % (_dash, _dash),
                 bygroups(Punctuation,
                          using(this, state=('+i6-root', 'directive'),
                                i6t='+i6t-not-inline'), Punctuation)),
                (r'(%s|(?<=[\s;:.%s]))\|\s|[%s]{2,}' %
                 (_start, _dquote, _newline), Text, '+heading?'),
                (r'(?i)[a(|%s]' % _newline, Text)
            ],
            '+phrase-definition': [
                (r'\s+', Text),
                (r'\[', Comment.Multiline, '+comment'),
                (r'(\([%s])(.*?)([%s]\))' % (_dash, _dash),
                 bygroups(Punctuation,
                          using(this, state=('+i6-root', 'directive',
                                             'default', 'statements'),
                                i6t='+i6t-inline'), Punctuation), '#pop'),
                default('#pop')
            ],
            '+use-option': [
                (r'\s+', Text),
                (r'\[', Comment.Multiline, '+comment'),
                (r'(\([%s])(.*?)([%s]\))' % (_dash, _dash),
                 bygroups(Punctuation,
                          using(this, state=('+i6-root', 'directive'),
                                i6t='+i6t-use-option'), Punctuation), '#pop'),
                default('#pop')
            ],
            '+comment': [
                (r'[^\[\]]+', Comment.Multiline),
                (r'\[', Comment.Multiline, '#push'),
                (r'\]', Comment.Multiline, '#pop')
            ],
            '+text': [
                (r'[^\[%s]+' % _dquote, String.Double),
                (r'\[.*?\]', String.Interpol),
                (r'[%s]' % _dquote, String.Double, '#pop')
            ],
            '+heading?': [
                (r'(\|?\s)+', Text),
                (r'\[', Comment.Multiline, '+comment'),
                (r'[%s]{4}\s+' % _dash, Text, '+documentation-heading'),
                (r'[%s]{1,3}' % _dash, Text),
                (r'(?i)(volume|book|part|chapter|section)\b[^%s]*' % _newline,
                 Generic.Heading, '#pop'),
                default('#pop')
            ],
            '+documentation-heading': [
                (r'\s+', Text),
                (r'\[', Comment.Multiline, '+comment'),
                (r'(?i)documentation\s+', Text, '+documentation-heading2'),
                default('#pop')
            ],
            '+documentation-heading2': [
                (r'\s+', Text),
                (r'\[', Comment.Multiline, '+comment'),
                (r'[%s]{4}\s' % _dash, Text, '+documentation'),
                default('#pop:2')
            ],
            '+documentation': [
                (r'(?i)(%s)\s*(chapter|example)\s*:[^%s]*' %
                 (_start, _newline), Generic.Heading),
                (r'(?i)(%s)\s*section\s*:[^%s]*' % (_start, _newline),
                 Generic.Subheading),
                (r'((%s)\t.*?[%s])+' % (_start, _newline),
                 using(this, state='+main')),
                (r'[^%s\[]+|[%s\[]' % (_newline, _newline), Text),
                (r'\[', Comment.Multiline, '+comment'),
            ],
            '+i6t-not-inline': [
                (r'(%s)@c( .*?)?([%s]|\Z)' % (_start, _newline),
                 Comment.Preproc),
                (r'(%s)@([%s]+|Purpose:)[^%s]*' % (_start, _dash, _newline),
                 Comment.Preproc),
                (r'(%s)@p( .*?)?([%s]|\Z)' % (_start, _newline),
                 Generic.Heading, '+p')
            ],
            '+i6t-use-option': [
                include('+i6t-not-inline'),
                (r'({)(N)(})', bygroups(Punctuation, Text, Punctuation))
            ],
            '+i6t-inline': [
                (r'({)(\S[^}]*)?(})',
                 bygroups(Punctuation, using(this, state='+main'),
                          Punctuation))
            ],
            '+i6t': [
                (r'({[%s])(![^}]*)(}?)' % _dash,
                 bygroups(Punctuation, Comment.Single, Punctuation)),
                (r'({[%s])(lines)(:)([^}]*)(}?)' % _dash,
                 bygroups(Punctuation, Keyword, Punctuation, Text,
                          Punctuation), '+lines'),
                (r'({[%s])([^:}]*)(:?)([^}]*)(}?)' % _dash,
                 bygroups(Punctuation, Keyword, Punctuation, Text,
                          Punctuation)),
                (r'(\(\+)(.*?)(\+\)|\Z)',
                 bygroups(Punctuation, using(this, state='+main'),
                          Punctuation))
            ],
            '+p': [
                (r'[^@]+', Comment.Preproc),
                (r'(%s)@c( .*?)?([%s]|\Z)' % (_start, _newline),
                 Comment.Preproc, '#pop'),
                (r'(%s)@([%s]|Purpose:)' % (_start, _dash), Comment.Preproc),
                (r'(%s)@p( .*?)?([%s]|\Z)' % (_start, _newline),
                 Generic.Heading),
                (r'@', Comment.Preproc)
            ],
            '+lines': [
                (r'(%s)@c( .*?)?([%s]|\Z)' % (_start, _newline),
                 Comment.Preproc),
                (r'(%s)@([%s]|Purpose:)[^%s]*' % (_start, _dash, _newline),
                 Comment.Preproc),
                (r'(%s)@p( .*?)?([%s]|\Z)' % (_start, _newline),
                 Generic.Heading, '+p'),
                (r'(%s)@[a-zA-Z_0-9]*[ %s]' % (_start, _newline), Keyword),
                (r'![^%s]*' % _newline, Comment.Single),
                (r'({)([%s]endlines)(})' % _dash,
                 bygroups(Punctuation, Keyword, Punctuation), '#pop'),
                (r'[^@!{]+?([%s]|\Z)|.' % _newline, Text)
            ]
        }
        # Inform 7 can include snippets of Inform 6 template language,
        # so all of Inform6Lexer's states are copied here, with
        # modifications to account for template syntax. Inform7Lexer's
        # own states begin with '+' to avoid name conflicts. Some of
        # Inform6Lexer's states begin with '_': these are not modified.
        # They deal with template syntax either by including modified
        # states, or by matching r'' then pushing to modified states.
        for token in Inform6Lexer.tokens:
            if token == 'root':
                continue
            tokens[level][token] = list(Inform6Lexer.tokens[token])
            if not token.startswith('_'):
                tokens[level][token][:0] = [include('+i6t'), include(level)]

    def __init__(self, **options):
        level = options.get('i6t', '+i6t-not-inline')
        if level not in self._all_tokens:
            self._tokens = self.__class__.process_tokendef(level)
        else:
            self._tokens = self._all_tokens[level]
        RegexLexer.__init__(self, **options)


class Inform6TemplateLexer(Inform7Lexer):
    """
    For `Inform 6 template
    <http://inform7.com/sources/src/i6template/Woven/index.html>`_ code.

    .. versionadded:: 2.0
    """

    name = 'Inform 6 template'
    aliases = ['i6t']
    filenames = ['*.i6t']

    def get_tokens_unprocessed(self, text, stack=('+i6t-root',)):
        return Inform7Lexer.get_tokens_unprocessed(self, text, stack)
