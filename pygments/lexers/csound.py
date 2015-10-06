# -*- coding: utf-8 -*-

import re

from pygments.lexer import RegexLexer, bygroups, default, include, using, words
from pygments.token import Comment, Keyword, Name, Number, Operator, Punctuation, String, Text, Whitespace

# The CsoundDocumentLexer casuses a Pygments test to fail.
__all__ = ['CsoundScoreLexer', 'CsoundOrchestraLexer']#, 'CsoundDocumentLexer']

newline = (r'((?:;|//).*)*(\n)', bygroups(Comment.Single, Text))


class CsoundLexer(RegexLexer):
    # Subclasses must define a 'single-line string' state.
    tokens = {
        'whitespace': [
            (r'[ \t]+', Text),
            (r'\\\n', Text),
            (r'/[*](.|\n)*?[*]/', Comment.Multiline)
        ],
        
        'macro call': [
            (r'(\$\w+\.?)(\()', bygroups(Comment.Preproc, Punctuation), 'function macro call'),
            (r'\$\w+(\.|\b)', Comment.Preproc)
        ],
        'function macro call': [
            (r"((?:\\['\)]|[^'\)])+)(')", bygroups(Comment.Preproc, Punctuation)),
            (r"([^'\)]+)(\))", bygroups(Comment.Preproc, Punctuation), '#pop')
        ],
        
        'whitespace or macro call': [
            include('whitespace'),
            include('macro call')
        ],
        
        'preprocessor directives': [
            (r'#(e(nd(if)?|lse)|ifn?def|undef)\b|##', Comment.Preproc),
            (r'#include\b', Comment.Preproc, 'include'),
            (r'#[ \t]*define\b', Comment.Preproc, 'macro name'),
            (r'@+[ \t]*\d*', Comment.Preproc)
        ],

        'include': [
            include('whitespace'),
            (r'"', String, 'single-line string')
        ],

        'macro name': [
            include('whitespace'),
            (r'(\w+)(\()', bygroups(Comment.Preproc, Text), 'function macro argument list'),
            (r'\w+', Comment.Preproc, 'object macro definition after name')
        ],
        'object macro definition after name': [
            include('whitespace'),
            (r'#', Punctuation, 'object macro replacement text')
        ],
        'object macro replacement text': [
            (r'(\\#|[^#])+', Comment.Preproc),
            (r'#', Punctuation, '#pop:3')
        ],
        'function macro argument list': [
            (r"(\w+)(['#])", bygroups(Comment.Preproc, Punctuation)),
            (r'(\w+)(\))', bygroups(Comment.Preproc, Punctuation), 'function macro definition after name')
        ],
        'function macro definition after name': [
            (r'[ \t]+', Text),
            (r'#', Punctuation, 'function macro replacement text')
        ],
        'function macro replacement text': [
            (r'(\\#|[^#])+', Comment.Preproc),
            (r'#', Punctuation, '#pop:4')
        ]
    }


class CsoundScoreLexer(CsoundLexer):
    """
    For `Csound <http://csound.github.io>`_ scores.
    """

    name = 'Csound Score'
    filenames = ['*.sco']

    tokens = {
        'partial statement': [
            include('preprocessor directives'),
            (r'\d+e[+-]?\d+|(\d+\.\d*|\d*\.\d+)(e[+-]?\d+)?', Number.Float),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+', Number.Integer),
            (r'"', String, 'single-line string'),
            (r'[+\-*/%^!=<>|&#~.]', Operator),
            (r'[]()[]', Punctuation),
            (r'\w+', Comment.Preproc)
        ],

        'statement': [
            include('whitespace or macro call'),
            newline + ('#pop',),
            include('partial statement')
        ],
        
        'root': [
            newline,
            include('whitespace or macro call'),
            (r'[{}]', Punctuation, 'statement'),
            (r'[abefimq-tv-z]|[nN][pP]?', Keyword, 'statement')
        ],

        'single-line string': [
            (r'"', String, '#pop'),
            (r'[^\\"]+', String)
        ]
    }


from pygments.lexers._csound_builtins import OPCODES
from pygments.lexers.python import PythonLexer
from pygments.lexers.scripting import LuaLexer

class CsoundOrchestraLexer(CsoundLexer):
    """
    For `Csound <http://csound.github.io>`_ orchestras.
    """

    name = 'Csound Orchestra'
    filenames = ['*.orc']

    user_defined_opcodes = set()
    
    def opcode_name_callback(lexer, match):
        opcode = match.group(0)
        lexer.user_defined_opcodes.add(opcode)
        yield match.start(), Name.Function, opcode

    def name_callback(lexer, match):
        name = match.group(0)
        if re.match('p\d+$', name) or name in OPCODES:
            yield match.start(), Name.Builtin, name
        elif name in lexer.user_defined_opcodes:
            yield match.start(), Name.Function, name
        else:
            nameMatch = re.search(r'^(g?[aikSw])(\w+)', name)
            if nameMatch:
                yield nameMatch.start(1), Keyword.Type, nameMatch.group(1)
                yield nameMatch.start(2), Name, nameMatch.group(2)
            else:
                yield match.start(), Name, name

    tokens = {
        'label': [
            (r'\b(\w+)(:)', bygroups(Name.Label, Punctuation))
        ],

        'partial expression': [
            include('preprocessor directives'),
            (r'\b(0dbfs|k(r|smps)|nchnls(_i)?|sr)\b', Name.Variable.Global),
            (r'\d+e[+-]?\d+|(\d+\.\d*|\d*\.\d+)(e[+-]?\d+)?', Number.Float),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+', Number.Integer),
            (r'"', String, 'single-line string'),
            (r'{{', String, 'multi-line string'),
            (r'[+\-*/%^!=&|<>#~¬]', Operator),
            (r'[](),?:[]', Punctuation),
            (words((
                # Keywords
                'do', 'else', 'elseif', 'endif', 'enduntil', 'fi', 'if', 'ithen', 'kthen', 'od', 'then', 'until', 'while',
                # Opcodes that act as control structures
                'return', 'timout'
                ), prefix=r'\b', suffix=r'\b'), Keyword),
            (words(('goto', 'igoto', 'kgoto', 'rigoto', 'tigoto'), prefix=r'\b', suffix=r'\b'), Keyword, 'goto label'),
            (words(('cggoto', 'cigoto', 'cingoto', 'ckgoto', 'cngoto'), prefix=r'\b', suffix=r'\b'), Keyword, ('goto label', 'goto expression')),
            (words(('loop_ge', 'loop_gt', 'loop_le', 'loop_lt'), prefix=r'\b', suffix=r'\b'), Keyword, ('goto label', 'goto expression', 'goto expression', 'goto expression')),
            (r'\bscoreline(_i)?\b', Name.Builtin, 'scoreline opcode'),
            (r'\bpyl?run[it]?\b', Name.Builtin, 'python opcode'),
            (r'\blua_(exec|opdef)\b', Name.Builtin, 'lua opcode'),
            (r'\b[a-zA-Z_]\w*\b', name_callback)
        ],

        'expression': [
            include('whitespace or macro call'),
            newline + ('#pop',),
            include('partial expression')
        ],

        'root': [
            newline,
            include('whitespace or macro call'),
            (r'\binstr\b', Keyword, ('instrument block', 'instrument name list')),
            (r'\bopcode\b', Keyword, ('opcode block', 'opcode parameter list', 'opcode types', 'opcode types', 'opcode name')),
            include('label'),
            default('expression')
        ],

        'instrument name list': [
            include('whitespace or macro call'),
            (r'\d+|\+?[a-zA-Z_]\w*', Name.Function),
            (r',', Punctuation),
            newline + ('#pop',)
        ],
        'instrument block': [
            newline,
            include('whitespace or macro call'),
            (r'\bendin\b', Keyword, '#pop'),
            include('label'),
            default('expression')
        ],

        'opcode name': [
            include('whitespace or macro call'),
            (r'[a-zA-Z_]\w*', opcode_name_callback, '#pop')
        ],
        'opcode types': [
            include('whitespace or macro call'),
            (r'0|[]afijkKoOpPStV[]+', Keyword.Type, '#pop'),
            (r',', Punctuation)
        ],
        'opcode parameter list': [
            include('whitespace or macro call'),
            newline + ('#pop',)
        ],
        'opcode block': [
            newline,
            include('whitespace or macro call'),
            (r'\bendop\b', Keyword, '#pop'),
            include('label'),
            default('expression')
        ],

        'goto label': [
            include('whitespace or macro call'),
            (r'\w+', Name.Label, '#pop'),
            default('#pop')
        ],
        'goto expression': [
            include('whitespace or macro call'),
            (r',', Punctuation, '#pop'),
            include('partial expression')
        ],
        
        'single-line string': [
            include('macro call'),
            (r'"', String, '#pop'),
            # From https://github.com/csound/csound/blob/develop/Opcodes/fout.c#L1405
            (r'%\d*(\.\d+)?[cdhilouxX]', String.Interpol),
            (r'%[!%nNrRtT]|[~^]|\\([\\aAbBnNrRtT"]|[0-7]{1,3})', String.Escape),
            (r'[^\\"~$%\^\n]+', String),
            (r'[\\"~$%\^\n]', String)
        ],
        'multi-line string': [
            (r'}}', String, '#pop'),
            (r'[^\}]+|\}(?!\})', String)
        ],

        'scoreline opcode': [
            include('whitespace or macro call'),
            (r'{{', String, 'scoreline'),
            default('#pop')
        ],
        'scoreline': [
            (r'}}', String, '#pop'),
            (r'([^\}]+)|\}(?!\})', using(CsoundScoreLexer))
        ],

        'python opcode': [
            include('whitespace or macro call'),
            (r'{{', String, 'python'),
            default('#pop')
        ],
        'python': [
            (r'}}', String, '#pop'),
            (r'([^\}]+)|\}(?!\})', using(PythonLexer))
        ],

        'lua opcode': [
            include('whitespace or macro call'),
            (r'"', String, 'single-line string'),
            (r'{{', String, 'lua'),
            (r',', Punctuation),
            default('#pop')
        ],
        'lua': [
            (r'}}', String, '#pop'),
            (r'([^\}]+)|\}(?!\})', using(LuaLexer))
        ]
    }


# Below is a lexer for Csound documents, but it causes a Pygments test to fail.

# import copy
# from pygments.lexers.html import HtmlLexer, XmlLexer
# 
# class CsoundDocumentLexer(XmlLexer):
#     """
#     For `Csound <http://csound.github.io>`_ documents.
#     """
# 
#     name = 'Csound Document'
#     aliases = ['csound']
#     filenames = ['*.csd']
# 
#     tokens = copy.deepcopy(XmlLexer.tokens)
#     for i, item in enumerate(tokens['root']):
#         if len(item) > 2 and item[2] == 'tag':
#             (tokens['root']).insert(i, (r'(<)(\s*)(CsInstruments)(\s*)', bygroups(Name.Tag, Text, Name.Tag, Text), ('orchestra content', 'tag')))
#             (tokens['root']).insert(i, (r'(<)(\s*)(CsScore)(\s*)', bygroups(Name.Tag, Text, Name.Tag, Text), ('score content', 'tag')))
#             (tokens['root']).insert(i, (r'(<)(\s*)(html)(\s*)', bygroups(Name.Tag, Text, Name.Tag, Text), ('HTML', 'tag')))
#             break
# 
#     tokens['orchestra content'] = [
#         (r'(<)(\s*)(/)(\s*)(CsInstruments)(\s*)(>)', bygroups(Name.Tag, Text, Name.Tag, Text, Name.Tag, Text, Name.Tag), '#pop'),
#         (r'.+?(?=<\s*/\s*CsInstruments\s*>)', using(CsoundOrchestraLexer))
#     ]
#     tokens['score content'] = [
#         (r'(<)(\s*)(/)(\s*)(CsScore)(\s*)(>)', bygroups(Name.Tag, Text, Name.Tag, Text, Name.Tag, Text, Name.Tag), '#pop'),
#         (r'.+?(?=<\s*/\s*CsScore\s*>)', using(CsoundScoreLexer))
#     ]
#     tokens['HTML'] = [
#         (r'(<)(\s*)(/)(\s*)(html)(\s*)(>)', bygroups(Name.Tag, Text, Name.Tag, Text, Name.Tag, Text, Name.Tag), '#pop'),
#         (r'.+?(?=<\s*/\s*html\s*>)', using(HtmlLexer))
#     ]
