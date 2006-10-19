# -*- coding: utf-8 -*-
"""
    pygments.lexers.text
    ~~~~~~~~~~~~~~~~~~~

    Lexers for non-source code file types: Diff, Makefiles, Ini configs etc.

    :copyright: 2006 by Armin Ronacher, Georg Brandl.
    :license: GNU LGPL, see LICENSE for more details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import \
    Text, Comment, Keyword, Name, String, Generic, Operator, Number


__all__ = ['IniLexer', 'MakefileLexer', 'DiffLexer', 'IrcLogsLexer',
           'TexLexer']


class IniLexer(RegexLexer):
    name = 'INI'
    aliases = ['ini', 'cfg']
    filenames = ['*.ini', '*.cfg']

    tokens = {
        'root': [
            (r'\s+', Text),
            (r';.*?$', Comment),
            (r'\[.*?\]$', Keyword),
            (r'(.*?)(\s*)(=)(\s*)(.*?)$',
             bygroups(Name.Attribute, Text, Operator, Text, String))
        ]
    }


class MakefileLexer(RegexLexer):
    name = 'Makefile'
    aliases = ['make', 'makefile', 'mf']
    filenames = ['*.mak', 'Makefile', 'makefile']

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'#.*?\n', Comment),
            (r'(cmdswitches|error|message|include|if|ifdef|ifndef|else|'
             r'else\s*if|else\s*ifdef|else\s*ifndef|endif|undef)\b', Keyword),
            (r'([a-zA-Z_][a-zA-Z0-9_]*)(\s*)(=)(\s*)',
             bygroups(Name.Variable, Text, Operator, Text), 'var'),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r"'(\\\\|\\'|[^'])*'", String.Single),
            (r'([^\n:]+)(:)([ \t]*)', bygroups(Name.Function, Operator, Text),
             'block-header')
        ],
        'var': [
            (r'\\\n', String),
            (r'\n', Text, '#pop'),
            (r'\\', String),
            (r'[^\\\n]+', String),
        ],
        'block-header': [
            (r'[^,\n]', String),
            (r',', Text),
            (r'\n[\t ]+', Text, 'block'),
            (r'\n', Text, '#pop')
        ],
        'block': [
            (r'#.*?(?=\n)', Comment),
            (r'\n[\t ]+', Text),
            (r'[^\n$]+', String),
            (r'\$[A-Za-z0-9_]+', String.Interpol),
            (r'\$\(.*?\)', String.Interpol),
            (r'\$', String),
            (r'\n', Text, '#pop:2'),
        ]
    }


class DiffLexer(RegexLexer):
    name = 'Diff'
    aliases = ['diff']
    filenames = ['*.diff', '*.patch']

    tokens = {
        'root': [
            (r' .*\n', Text),
            (r'\+.*\n', Generic.Inserted),
            (r'-.*\n', Generic.Deleted),
            (r'!.*\n', Generic.Strong),
            (r'@.*\n', Generic.Subheading),
            (r'Index.*\n', Generic.Heading),
            (r'=.*\n', Generic.Heading),
            (r'.*\n', Text),
        ]
    }


class IrcLogsLexer(RegexLexer):
    name = 'IRC logs'
    aliases = ['irc']

    flags = re.VERBOSE | re.MULTILINE
    timestamp = r"""
        ( (?: \[|\()?                  # Opening bracket or paren for the timestamp
            (?:                        # Timestamp
                (?: (?:\d{1,4} [-/]?)+ # Date as - or /-separated groups of digits
                 [T ])?                # Date/time separator: T or space
                (?: \d?\d [:.]?)+      # Time as :/.-separated groups of 1 or 2 digits
            )
          (?: \]|\))?\s+ )?            # Closing bracket or paren for the timestamp
    """
    tokens = {
        'root': [
            # normal msgs
            ("^" + timestamp + r"""
                (\s*<.*?>\s+)          # Nick """,
             bygroups(Comment.Preproc, Name.Tag), 'msg'),
            # /me msgs
            ("^" + timestamp + r"""
                (\s*[*]\s+)            # Star
                ([^\s]+\s+.*?\n)       # Nick + rest of message """,
             bygroups(Comment.Preproc, Keyword, Generic.Inserted)),
            # join/part msgs
            ("^" + timestamp + r"""
                (\s*(?:[*]{3}|-!-)\s*) # Star(s)
                ([^\s]+\s+)            # Nick + Space
                (.*?\n)                # Rest of message """,
             bygroups(Comment.Preproc, Keyword, String, Comment)),
            (r"^.*?\n", Text),
        ],
        'msg': [
            (r"[^\s]+:", Name.Attribute),  # Prefix
            (r".*?\n", Text, '#pop'),
        ],
    }


class TexLexer(RegexLexer):
    name = 'TeX'
    aliases = ['tex', 'latex']
    filenames = ['*.tex', '*.aux', '*.toc']

    tokens = {
        'general': [
            (r'%.*?\n', Comment),
            (r'[{}]', Name.Builtin),
            (r'[&_^]', Name.Builtin),
        ],
        'root': [
            (r'\\\[', String.Backtick, 'displaymath'),
            (r'\\\(', String, 'inlinemath'),
            (r'\$\$', String.Backtick, 'displaymath'),
            (r'\$', String, 'inlinemath'),
            (r'\\([a-zA-Z]+|.)', Keyword, 'command'),
            include('general'),
            (r'[^\\$%&_^{}]+', Text),
        ],
        'math': [
            (r'\\([a-zA-Z]+|.)', Name.Variable),
            include('general'),
            (r'[0-9]+', Number),
            (r'[-=!+*/()\[\]]', Operator),
            (r'[^=!+*/()\[\]\\$%&_^{}0-9-]+', Name.Builtin),
        ],
        'inlinemath': [
            (r'\\\)', String, '#pop'),
            (r'\$', String, '#pop'),
            include('math'),
        ],
        'displaymath': [
            (r'\\\]', String, '#pop'),
            (r'\$\$', String, '#pop'),
            (r'\$', Name.Builtin),
            include('math'),
        ],
        'command': [
            (r'\[.*?\]', Name.Attribute),
            (r'\*', Keyword),
            (r'', Text, '#pop'),
        ],
    }
