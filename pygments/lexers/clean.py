# -*- coding: utf-8 -*-
"""
    pygments.lexers.make
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for Makefiles and similar.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import ExtendedRegexLexer, bygroups, words, include
from pygments.token import *

__all__ = ['CleanLexer']

class CleanLexer(ExtendedRegexLexer):
    """
    Lexer for the general purpose, state-of-the-art, pure and lazy functional
    programming language Clean (http://clean.cs.ru.nl/Clean).

    .. versionadded: 2.1
    """
    name = 'CleanLexer'
    aliases = ['Clean', 'clean']
    filenames = ['*.icl', '*.dcl']

    def __init__(self, *args, **kwargs):
        super(CleanLexer, self).__init__(*args, **kwargs)
        self.stored_indent = 0

    def check_class_not_import(lexer, match, ctx):
        if match.group(0) == 'import':
            yield match.start(), Keyword.Namespace, match.group(0)
            ctx.stack = ctx.stack[:-1] + ['fromimportfunc']
        else:
            yield match.start(), Name.Class, match.group(0)
        ctx.pos = match.end()

    def check_instance_class(lexer, match, ctx):
        if match.group(0) == 'instance' or match.group(0) == 'class':
            yield match.start(), Keyword, match.group(0)
        else:
            yield match.start(), Name.Function, match.group(0)
            ctx.stack = ctx.stack + ['fromimportfunctype']
        ctx.pos = match.end()

    def store_indent(lexer, match, ctx):
        # Tabs are four spaces: 
        # https://svn.cs.ru.nl/repos/clean-platform/trunk/doc/STANDARDS.txt
        lexer.stored_indent = len(match.group(0).replace('\t','    '))
        ctx.pos = match.end()
        yield match.start(), Text, match.group(0)

    def check_indent1(lexer, match, ctx):
        indent = len(match.group(0)) - 1
        if indent > lexer.stored_indent:
            yield match.start(), Whitespace, match.group(0)
            ctx.pos = match.start() + indent + 1
        else:
            lexer.stored_indent = 0
            ctx.pos = match.start()
            ctx.stack = ctx.stack[:-1]
            yield match.start(), Whitespace, match.group(0)[1:]

    def check_indent2(lexer, match, ctx):
        indent = len(match.group(0)) - 1
        if indent > lexer.stored_indent:
            yield match.start(), Whitespace, match.group(0)
            ctx.pos = match.start() + indent + 1
        else:
            lexer.stored_indent = 0
            ctx.pos = match.start()
            ctx.stack = ctx.stack[:-2]
            yield match.start(), Whitespace, match.group(0)[1:]
            if match.group(0) == '\n\n':
                ctx.pos = ctx.pos + 1

    def check_indent3(lexer, match, ctx):
        indent = len(match.group(0)) - 1
        if indent > lexer.stored_indent:
            yield match.start(), Whitespace, match.group(0)
            ctx.pos = match.start() + indent + 1
        else:
            lexer.stored_indent = 0
            ctx.pos = match.start()
            ctx.stack = ctx.stack[:-3]
            yield match.start(), Whitespace, match.group(0)[1:]
            if match.group(0) == '\n\n':
                ctx.pos = ctx.pos + 1

    def skip(lexer, match, ctx):
        ctx.stack = ctx.stack[:-1]
        ctx.pos = match.end()
        yield match.start(), Comment, match.group(0)

    tokens = {
        'common': [
            (r';', Punctuation, '#pop'),
            (r'//', Comment, 'singlecomment')
        ],
        'root': [
            # Comments
            (r'//.*\n', Comment.Single),
            (r'(?s)/\*\*.*?\*/', Comment.Special),
            (r'(?s)/\*.*?\*/', Comment.Multi),

            # Modules, imports, etc.
            (r'\b((?:implementation|definition|system)\s+)?(module)(\s+)([\w`]+)', 
                bygroups(Keyword.Namespace, Keyword.Namespace, Text, Name.Class)),
            (r'(?<=\n)import(?=\s)', Keyword.Namespace, 'import'),
            (r'(?<=\n)from(?=\s)', Keyword.Namespace, 'fromimport'),

            # Keywords
            # We cannot use (?s)^|(?<=\s) as prefix, so need to repeat this
            (words(('class','instance','where','with','let','let!','with','in',
                'case','of','infix','infixr','infixl','generic','derive',
                'otherwise', 'code', 'inline'), 
                prefix=r'(?<=\s)', suffix=r'(?=\s)'), Keyword),
            (words(('class','instance','where','with','let','let!','with','in',
                'case','of','infix','infixr','infixl','generic','derive',
                'otherwise', 'code', 'inline'), 
                prefix=r'(?s)^', suffix=r'(?=\s)'), Keyword),

            # Function definitions
            (r'(?=\{\|)', Whitespace, 'genericfunction'),
            (r'(?<=\n)(\s*)([\w`\$\(\)=\-<>~*\^\|\+&%]+)(\s+[\w])*(\s*)(::)',
                bygroups(store_indent, Name.Function, Keyword.Type, Whitespace, Punctuation),
                'functiondefargs'),

            # Type definitions
            (r'(?<=\n)([ \t]*)(::)', bygroups(store_indent, Punctuation), 'typedef'),
            (r'^([ \t]*)(::)', bygroups(store_indent, Punctuation), 'typedef'),

            # Literals
            (r'\'\\?.(?<!\\)\'', String.Char),
            (r'\'\\\d+\'', String.Char),
            (r'\'\\\\\'', String.Char), # (special case for '\\')
            (r'[\+\-~]?\s*?\d+\.\d+(E[+-~]?\d+)?\b', Number.Float),
            (r'[\+\-~]?\s*?0[0-7]\b', Number.Oct),
            (r'[\+\-~]?\s*?0x[0-9a-fA-F]\b', Number.Hex),
            (r'[\+\-~]?\s*?\d+\b', Number.Integer),
            (r'"', String.Double, 'doubleqstring'),
            (words(('True', 'False'), prefix=r'(?<=\s)', suffix=r'(?=\s)'), Literal),

            # Everything else is some name
            (r'([\w`\$%]+\.?)*[\w`\$%]+', Name),

            # Punctuation
            (r'[{}()\[\],:;\.#]', Punctuation),
            (r'[\+\-=!<>\|&~*\^/]', Operator),
            (r'\\\\', Operator),

            # Lambda expressions
            (r'\\.*?(->|\.|=)', Name.Function),

            # Whitespace
            (r'\s', Whitespace),

            include('common')
        ],
        'fromimport': [
            include('common'),
            (r'([\w`]+)', check_class_not_import),
            (r'\n', Whitespace, '#pop'),
            (r'\s', Whitespace)
        ],
        'fromimportfunc': [
            include('common'),
            (r'([\w`\$\(\)=\-<>~*\^\|\+&%]+)', check_instance_class),
            (r',', Punctuation),
            (r'\n', Whitespace, '#pop'),
            (r'\s', Whitespace)
        ],
        'fromimportfunctype': [
            include('common'),
            (r'[{(\[]', Punctuation, 'combtype'),
            (r',', Punctuation, '#pop'),
            (r':;\.#]', Punctuation),
            (r'\n', Whitespace, '#pop:2'),
            (r'\s', Whitespace),
            (r'.', Keyword.Type)
        ],
        'combtype': [
            include('common'),
            (r'[})\]]', Punctuation, '#pop'),
            (r'[{(\[]', Punctuation, '#pop'),
            (r',:;\.#]', Punctuation),
            (r'\s', Whitespace),
            (r'.', Keyword.Type)
        ],
        'import': [
            include('common'),
            (words(('from', 'import', 'as', 'qualified'), 
                prefix='(?<=\s)', suffix='(?=\s)'), Keyword.Namespace),
            (r'[\w`]+', Name.Class),
            (r'\n', Whitespace, '#pop'),
            (r',', Punctuation),
            (r'\s', Whitespace)
        ],
        'singlecomment': [
            (r'(.)(?=\n)', skip),
            (r'.', Comment)
        ],
        'doubleqstring': [
            (r'[^\\\'"]+', String.Double),
            (r'"', String.Double, '#pop'),
            (r'\\.|\'', String.Double)
        ],
        'typedef': [
            include('common'),
            (r'[\w`]+', Keyword.Type),
            (r'[:=\|\(\),\[\]\{\}\!\*]', Punctuation),
            (r'->', Punctuation),
            (r'\n(?=[^\s\|])', Whitespace, '#pop'),
            (r'\s', Whitespace),
            (r'.', Keyword.Type)
        ],
        'genericfunction': [
            include('common'),
            (r'\{\|', Punctuation),
            (r'\|\}', Punctuation, '#pop'),
            (r',', Punctuation),
            (r'->', Punctuation),
            (r'(\s+of\s+)(\{)', bygroups(Keyword, Punctuation), 'genericftypes'),
            (r'\s', Whitespace),
            (r'[\w`]+', Keyword.Type),
            (r'[\*\(\)]', Punctuation)
        ],
        'genericftypes': [
            include('common'),
            (r'[\w`]+', Keyword.Type),
            (r',', Punctuation),
            (r'\s', Whitespace),
            (r'\}', Punctuation, '#pop')
        ],
        'functiondefargs': [
            include('common'),
            (r'\n(\s*)', check_indent1),
            (r'[!{}()\[\],:;\.#]', Punctuation),
            (r'->', Punctuation, 'functiondefres'),
            (r'^(?=\S)', Whitespace, '#pop'),
            (r'\S', Keyword.Type),
            (r'\s', Whitespace)
        ],
        'functiondefres': [
            include('common'),
            (r'\n(\s*)', check_indent2),
            (r'^(?=\S)', Whitespace, '#pop:2'),
            (r'[!{}()\[\],:;\.#]', Punctuation),
            (r'\|', Punctuation, 'functiondefclasses'),
            (r'\S', Keyword.Type),
            (r'\s', Whitespace)
        ],
        'functiondefclasses': [
            include('common'),
            (r'\n(\s*)', check_indent3),
            (r'^(?=\S)', Whitespace, '#pop:3'),
            (r'[,&]', Punctuation),
            (r'[\w`\$\(\)=\-<>~*\^\|\+&%]', Name.Function, 'functionname'),
            (r'\s', Whitespace)
        ],
        'functionname': [
            include('common'),
            (r'[\w`\$\(\)=\-<>~*\^\|\+&%]+', Name.Function),
            (r'(?=\{\|)', Punctuation, 'genericfunction'),
            (r'', Text, '#pop')
        ]
    }

