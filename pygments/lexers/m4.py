# -*- coding: utf-8 -*-
"""
    pygments.lexers.m4
    ~~~~~~~~~~~~~~~~~~

    Lexers for M4

    :copyright: Copyright 2013 Fabrice Salvaire.
    :license: BSD, see LICENSE for details.
"""

import re
from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import (
    Text, Comment, Keyword, Name, String, Punctuation,
)

__all__ = ['M4Lexer']


class M4Lexer(RegexLexer):
    """ For M4 source code. """
    name = 'm4'
    aliases = ['m4']
    filenames = ['*.m4']
    mimetypes = ['text']
    flags = re.MULTILINE

    gnu_m4_macros = (
        '__file__',
        '__gnu__',
        '__line__',
        '__os2__',
        '__program__',
        '__unix__',
        '__windows__',
        'argn',
        'array',
        'array_set',
        'builtin',
        'capitalize',
        'changecom',
        'changequote',
        'changeword',
        'cleardivert',
        'cond',
        'copy',
        'curry',
        'debugfile',
        'debugmode',
        'decr',
        'define',
        'define_blind',
        'defn',
        'divert',
        'divnum',
        'dnl',
        'downcase',
        'dquote',
        'dquote_elt',
        'dumpdef',
        'errprint',
        'esyscmd',
        'eval',
        'example',
        'exch',
        'fatal_error',
        'foreach',
        'foreachq',
        'forloop',
        'format',
        'ifdef',
        'ifelse',
        'ifelse',
        'ifelse',
        'include',
        'incr',
        'index',
        'indir',
        'join',
        'joinall',
        'len',
        'm4exit',
        'm4wrap',
        'maketemp',
        'mkstemp',
        'nargs',
        'os2',
        'patsubst',
        'popdef',
        'pushdef',
        'quote',
        'regexp',
        'rename',
        'reverse',
        'shift',
        'sinclude',
        'stack_foreach',
        'stack_foreach_lifo',
        'stack_foreach_sep',
        'stack_foreach_sep_lifo',
        'substr',
        'syscmd',
        'sysval',
        'traceoff',
        'traceon',
        'translit',
        'undefine',
        'undivert',
        'unix',
        'upcase',
        'windows',
        )

    tokens = {
        'whitespace': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'#.*', Comment.Single),
            ],
        'text': [
            ('[^#\n]+', Text),
            ],
        'statements': [
            ('(' + '|'.join(gnu_m4_macros) + r')(\()',
             bygroups(Name.Builtin, Punctuation),
             'macro'),
            (r'([a-zA-Z_][a-zA-Z0-9_]*)(\()',
             bygroups(Name, Punctuation),
             'macro'),
            (r'`', String, 'string'),
            ],
        'root': [
            include('whitespace'),
            include('statements'),
            include('text'),
            ],
        'macro': [
            include('statements'),
            (',', Punctuation),
            (r'(\))(dnl)(\n)', bygroups(Punctuation, Keyword, Text), '#pop'),
            (r'\)', Punctuation, '#pop'),
            (r'[^\),`]+', Text),
        ],
        'string': [
            (r"[^`']", String),
            (r"`", String, '#push'),
            (r"'", String, '#pop'),
        ],
    }
