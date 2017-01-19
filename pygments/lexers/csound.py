# -*- coding: utf-8 -*-
"""
    pygments.lexers.csound
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Csound languages.

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, default, include, using, words
from pygments.token import Comment, Keyword, Name, Number, Operator, Punctuation, \
    String, Text
from pygments.lexers._csound_builtins import OPCODES, DEPRECATED_OPCODES
from pygments.lexers.html import HtmlLexer
from pygments.lexers.python import PythonLexer
from pygments.lexers.scripting import LuaLexer

__all__ = ['CsoundScoreLexer', 'CsoundOrchestraLexer', 'CsoundDocumentLexer']

newline = (r'((?:(?:;|//).*)*)(\n)', bygroups(Comment.Single, Text))


class CsoundLexer(RegexLexer):
    tokens = {
        'whitespace': [
            (r'[ \t]+', Text),
            (r'\\\n', Text),
            (r'/[*](.|\n)*?[*]/', Comment.Multiline)
        ],

        'macro use': [
            (r'(\$[A-Z_a-z]\w*\.?)(\()', bygroups(Comment.Preproc, Punctuation),
             'function macro use'),
            (r'\$[A-Z_a-z]\w*(\.|\b)', Comment.Preproc)
        ],
        'function macro use': [
            (r"((?:\\['\)]|[^'\)])+)(')", bygroups(Comment.Preproc, Punctuation)),
            (r"([^'\)]+)(\))", bygroups(Comment.Preproc, Punctuation), '#pop')
        ],

        'whitespace or macro use': [
            include('whitespace'),
            include('macro use')
        ],

        'preprocessor directives': [
            (r'#(e(nd(if)?|lse)|ifn?def|undef)\b|##', Comment.Preproc),
            (r'#include', Comment.Preproc, 'include'),
            (r'#[ \t]*define', Comment.Preproc, 'macro name'),
            (r'@+[ \t]*\d*', Comment.Preproc)
        ],

        'include': [
            include('whitespace'),
            (r'([^ \t]).*?\1', String, '#pop')
        ],

        'macro name': [
            include('whitespace'),
            (r'([A-Z_a-z]\w*)(\()', bygroups(Comment.Preproc, Punctuation),
             'function macro parameter list'),
            (r'[A-Z_a-z]\w*', Comment.Preproc, 'object macro definition after name')
        ],
        'object macro definition after name': [
            include('whitespace'),
            (r'#', Punctuation, 'object macro replacement text')
        ],
        'object macro replacement text': [
            (r'(\\#|[^#])+', Comment.Preproc),
            (r'#', Punctuation, '#pop:3')
        ],
        'function macro parameter list': [
            include('whitespace'),
            (r"([A-Z_a-z]\w*)(['#])", bygroups(Comment.Preproc, Punctuation)),
            (r'([A-Z_a-z]\w*)(\))', bygroups(Comment.Preproc, Punctuation),
             'function macro definition after parameter list')
        ],
        'function macro definition after parameter list': [
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
    For `Csound <https://csound.github.io>`_ scores.

    .. versionadded:: 2.1
    """

    name = 'Csound Score'
    aliases = ['csound-score', 'csound-sco']
    filenames = ['*.sco']

    tokens = {
        'partial statement': [
            include('preprocessor directives'),
            (r'\d+[Ee][+-]?\d+|(\d+\.\d*|\d*\.\d+)([Ee][+-]?\d+)?', Number.Float),
            (r'0[Xx][0-9A-Fa-f]+', Number.Hex),
            (r'\d+', Number.Integer),
            (r'"', String, 'single-line string'),
            (r'[+\-*/%^!=<>|&#~.]', Operator),
            (r'[]()[]', Punctuation),
            (r'\w+', Comment.Preproc)
        ],

        'statement': [
            include('whitespace or macro use'),
            newline + ('#pop',),
            include('partial statement')
        ],

        'root': [
            newline,
            include('whitespace or macro use'),
            (r'[{}]', Punctuation, 'statement'),
            (r'[abefimq-tv-z]|[nN][pP]?', Keyword, 'statement')
        ],

        'single-line string': [
            (r'"', String, '#pop'),
            (r'[^\\"]+', String)
        ]
    }


class CsoundOrchestraLexer(CsoundLexer):
    """
    For `Csound <https://csound.github.io>`_ orchestras.

    .. versionadded:: 2.1
    """

    name = 'Csound Orchestra'
    aliases = ['csound', 'csound-orc']
    filenames = ['*.orc', '*.udo']

    user_defined_opcodes = set()

    def opcode_name_callback(lexer, match):
        opcode = match.group(0)
        lexer.user_defined_opcodes.add(opcode)
        yield match.start(), Name.Function, opcode

    def name_callback(lexer, match):
        name = match.group(1)
        if name in OPCODES or name in DEPRECATED_OPCODES:
            yield match.start(), Name.Builtin, name
            if match.group(2):
                yield match.start(2), Punctuation, match.group(2)
                yield match.start(3), Keyword.Type, match.group(3)
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
            (r'^([ \t]*)(\w+)(:)', bygroups(Text, Name.Label, Punctuation))
        ],

        'partial expression': [
            include('preprocessor directives'),
            (r'\b(0dbfs|A4|k(r|smps)|nchnls(_i)?|sr)\b', Name.Variable.Global),
            (r'\d+[Ee][+-]?\d+|(\d+\.\d*|\d*\.\d+)([Ee][+-]?\d+)?', Number.Float),
            (r'0[Xx][0-9A-Fa-f]+', Number.Hex),
            (r'\d+', Number.Integer),
            (r'"', String, 'single-line string'),
            (r'\{\{', String, 'multi-line string'),
            (r'[+\-*/%^!=&|<>#~¬]', Operator),
            (r'[](),?:[]', Punctuation),
            (words((
                # Keywords
                'do', 'else', 'elseif', 'endif', 'enduntil', 'fi', 'if', 'ithen', 'kthen',
                'od', 'then', 'until', 'while',
                # Opcodes that act as control structures
                'return', 'rireturn'
                ), prefix=r'\b', suffix=r'\b'), Keyword),
            (words(('goto', 'igoto', 'kgoto', 'reinit', 'rigoto', 'tigoto'),
                   prefix=r'\b', suffix=r'\b'), Keyword, 'goto label'),
            (words(('cggoto', 'cigoto', 'cingoto', 'ckgoto', 'cngoto', 'cnkgoto'),
                   prefix=r'\b', suffix=r'\b'), Keyword,
             ('goto label', 'goto expression')),
            (r'\btimout\b', Keyword,
             ('goto label', 'goto expression', 'goto expression')),
            (words(('loop_ge', 'loop_gt', 'loop_le', 'loop_lt'),
                   prefix=r'\b', suffix=r'\b'), Keyword,
             ('goto label', 'goto expression', 'goto expression', 'goto expression')),
            (r'\bscoreline(_i)?\b', Name.Builtin, 'scoreline opcode'),
            (r'\bprintk?s\b', Name.Builtin, 'prints opcode'),
            (r'\bpyl?run[it]?\b', Name.Builtin, 'python opcode'),
            (r'\blua_(exec|opdef)\b', Name.Builtin, 'lua opcode'),
            (r'\bp\d+\b', Name.Builtin),
            (r'\b([A-Z_a-z]\w*)(?:(:)([A-Za-z]))?\b', name_callback)
        ],

        'expression': [
            include('whitespace or macro use'),
            newline + ('#pop',),
            include('partial expression')
        ],

        'root': [
            newline,
            include('whitespace or macro use'),
            (r'\binstr\b', Keyword, ('instrument block', 'instrument name list')),
            (r'\bopcode\b', Keyword, ('opcode block', 'opcode parameter list',
                                      'opcode types', 'opcode types', 'opcode name')),
            include('label'),
            default('expression')
        ],

        'instrument name list': [
            include('whitespace or macro use'),
            (r'\d+|[A-Z_a-z]\w*', Name.Function),
            (r'[+,]', Punctuation),
            newline + ('#pop',)
        ],
        'instrument block': [
            newline,
            include('whitespace or macro use'),
            (r'\bendin\b', Keyword, '#pop'),
            include('label'),
            default('expression')
        ],

        'opcode name': [
            include('whitespace or macro use'),
            (r'[A-Z_a-z]\w*', opcode_name_callback, '#pop')
        ],
        'opcode types': [
            include('whitespace or macro use'),
            (r'0|[]afijkKoOpPStV[]+', Keyword.Type, '#pop'),
            (r',', Punctuation)
        ],
        'opcode parameter list': [
            include('whitespace or macro use'),
            newline + ('#pop',)
        ],
        'opcode block': [
            newline,
            include('whitespace or macro use'),
            (r'\bendop\b', Keyword, '#pop'),
            include('label'),
            default('expression')
        ],

        'goto label': [
            include('whitespace or macro use'),
            (r'\w+', Name.Label, '#pop'),
            default('#pop')
        ],
        'goto expression': [
            include('whitespace or macro use'),
            (r',', Punctuation, '#pop'),
            include('partial expression')
        ],

        'single-line string': [
            include('macro use'),
            (r'"', String, '#pop'),
            # https://github.com/csound/csound/search?q=unquote_string+path%3AEngine+filename%3Acsound_orc_compile.c
            (r'\\([\\abnrt"]|[0-7]{1,3})', String.Escape),
            (r'[^\\"$\n]+', String),
            (r'[\\"$\n]', String)
        ],
        'multi-line string': [
            (r'\}\}', String, '#pop'),
            (r'[^}]+|\}(?!\})', String)
        ],

        'scoreline opcode': [
            include('whitespace or macro use'),
            (r'\{\{', String, 'scoreline'),
            default('#pop')
        ],
        'scoreline': [
            (r'\}\}', String, '#pop'),
            (r'([^}]+)|\}(?!\})', using(CsoundScoreLexer))
        ],

        'prints opcode': [
            include('whitespace or macro use'),
            (r'"', String, 'prints'),
            default('#pop')
        ],
        'prints': [
            include('macro use'),
            (r'"', String, '#pop'),
            # https://github.com/csound/csound/search?q=printksset_+path%3AOOps+filename%3Augrw1.c
            (r'%\d*(\.\d+)?[cdfhilouxX]', String.Interpol),
            (r'%[!%nNrRtT]|[~^]|\\([\\aAbBnNrRtT"]|[0-7]{1,3})', String.Escape),
            (r'[^\\"~$%\^\n]+', String),
            (r'[\\"~$%\^\n]', String)
        ],

        'python opcode': [
            include('whitespace or macro use'),
            (r'\{\{', String, 'python'),
            default('#pop')
        ],
        'python': [
            (r'\}\}', String, '#pop'),
            (r'([^}]+)|\}(?!\})', using(PythonLexer))
        ],

        'lua opcode': [
            include('whitespace or macro use'),
            (r'"', String, 'single-line string'),
            (r'\{\{', String, 'lua'),
            (r',', Punctuation),
            default('#pop')
        ],
        'lua': [
            (r'\}\}', String, '#pop'),
            (r'([^}]+)|\}(?!\})', using(LuaLexer))
        ]
    }


class CsoundDocumentLexer(RegexLexer):
    """
    For `Csound <https://csound.github.io>`_ documents.

    .. versionadded:: 2.1
    """

    name = 'Csound Document'
    aliases = ['csound-document', 'csound-csd']
    filenames = ['*.csd']

    # These tokens are based on those in XmlLexer in pygments/lexers/html.py. Making
    # CsoundDocumentLexer a subclass of XmlLexer rather than RegexLexer may seem like a
    # better idea, since Csound Document files look like XML files. However, Csound
    # Documents can contain Csound comments (preceded by //, for example) before and
    # after the root element, unescaped bitwise AND & and less than < operators, etc. In
    # other words, while Csound Document files look like XML files, they may not actually
    # be XML files.
    tokens = {
        'root': [
            newline,
            (r'/[*](.|\n)*?[*]/', Comment.Multiline),
            (r'[^<&;/]+', Text),
            (r'<\s*CsInstruments', Name.Tag, ('orchestra', 'tag')),
            (r'<\s*CsScore', Name.Tag, ('score', 'tag')),
            (r'<\s*[hH][tT][mM][lL]', Name.Tag, ('HTML', 'tag')),
            (r'<\s*[\w:.-]+', Name.Tag, 'tag'),
            (r'<\s*/\s*[\w:.-]+\s*>', Name.Tag)
        ],
        'orchestra': [
            (r'<\s*/\s*CsInstruments\s*>', Name.Tag, '#pop'),
            (r'(.|\n)+?(?=<\s*/\s*CsInstruments\s*>)', using(CsoundOrchestraLexer))
        ],
        'score': [
            (r'<\s*/\s*CsScore\s*>', Name.Tag, '#pop'),
            (r'(.|\n)+?(?=<\s*/\s*CsScore\s*>)', using(CsoundScoreLexer))
        ],
        'HTML': [
            (r'<\s*/\s*[hH][tT][mM][lL]\s*>', Name.Tag, '#pop'),
            (r'(.|\n)+?(?=<\s*/\s*[hH][tT][mM][lL]\s*>)', using(HtmlLexer))
        ],
        'tag': [
            (r'\s+', Text),
            (r'[\w.:-]+\s*=', Name.Attribute, 'attr'),
            (r'/?\s*>', Name.Tag, '#pop')
        ],
        'attr': [
            (r'\s+', Text),
            (r'".*?"', String, '#pop'),
            (r"'.*?'", String, '#pop'),
            (r'[^\s>]+', String, '#pop')
        ]
    }
