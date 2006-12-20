# -*- coding: utf-8 -*-
"""
    pygments.lexers.text
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for non-source code file types: Diff, Makefiles, Ini configs etc.

    :copyright: 2006 by Armin Ronacher, Georg Brandl,
                Tim Hatch <tim@timhatch.com>,
                Ronny Pfannschmidt.
    :license: BSD, see LICENSE for more details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Punctuation, \
    Text, Comment, Keyword, Name, String, Generic, Operator, Number


__all__ = ['IniLexer', 'MakefileLexer', 'DiffLexer', 'IrcLogsLexer',
           'TexLexer', 'GroffLexer', 'ApacheConfLexer', 'BBCodeLexer']


class IniLexer(RegexLexer):
    name = 'INI'
    aliases = ['ini', 'cfg']
    filenames = ['*.ini', '*.cfg']

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'[;#].*?$', Comment),
            (r'\[.*?\]$', Keyword),
            (r'(.*?)(\s*)(=)(\s*)(.*?)$',
             bygroups(Name.Attribute, Text, Operator, Text, String))
        ]
    }

    def analyse_text(text):
        npos = text.find('\n')
        if npos < 3:
            return False
        return text[0] == '[' and text[npos-1] == ']'


class MakefileLexer(RegexLexer):
    name = 'Makefile'
    aliases = ['make', 'makefile', 'mf']
    filenames = ['*.mak', 'Makefile', 'makefile']
    mimetypes = ['text/x-makefile']

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'#.*?\n', Comment),
            (r'(cmdswitches|error|message|include|if|ifdef|ifndef|else|'
             r'else\s*if|else\s*ifdef|else\s*ifndef|endif|undef)\b', Keyword),
            (r'([a-zA-Z_][a-zA-Z0-9_]*)(\s*)([?:+]?=)(\s*)',
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
            (r',', Punctuation),
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
    mimetypes = ['text/x-diff', 'text/x-patch']

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

    def analyse_text(text):
        if text[:7] == 'Index: ':
            return True
        if text[:5] == 'diff ':
            return True
        if text[:4] == '--- ':
            return 0.9


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


class BBCodeLexer(RegexLexer):
    name = 'BBCode'
    aliases = ['bbcode']
    
    tokens = {
        'root' : [
            (r'[\s\w]+', Text),
            (r'(\[)(/?[^\]\n\r=]+)(\])',
             bygroups(Keyword, Keyword.Pseudo, Keyword)),
            (r'(\[)([^\]\n\r=]+)(=)([^\]\n\r]+)(\])',
             bygroups(Keyword, Keyword.Pseudo, Operator, String, Keyword)),
        ],
    }


class TexLexer(RegexLexer):
    name = 'TeX'
    aliases = ['tex', 'latex']
    filenames = ['*.tex', '*.aux', '*.toc']
    mimetypes = ['text/x-tex', 'text/x-latex']

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

    def analyse_text(text):
        for start in ("\\documentclass", "\\input", "\\documentstyle",
                      "\\relax"):
            if text[:len(start)] == start:
                return True


class GroffLexer(RegexLexer):
    """
    Lexer for the roff format, supporting groff extensions.  Mainly useful
    for highlighting manpages.
    """
    name = 'Groff'
    aliases = ['groff', 'nroff', 'man']
    filenames = ['*.[1234567]', '*.man']
    mimetypes = ['application/x-troff', 'text/troff']

    tokens = {
        'root': [
            (r'(?i)(\.)(\w+)', bygroups(Text, Keyword), 'request'),
            (r'\.', Punctuation, 'request'),
            # Regular characters, slurp till we find a backslash or newline
            (r'[^\\\n]*', Text, 'textline'),
        ],
        'textline': [
            include('escapes'),
            (r'[^\\\n]+', Text),
            (r'\n', Text, '#pop'),
        ],
        'escapes': [
            # groff has many ways to write escapes.
            (r'\\"[^\n]*', Comment),
            (r'\\[fn]\w', String.Escape),
            (r'\\\(..', String.Escape),
            (r'\\.\[.*\]', String.Escape),
            (r'\\.', String.Escape),
            (r'\\\n', Text, 'request'),
        ],
        'request': [
            (r'\n', Text, '#pop'),
            include('escapes'),
            (r'"[^\n"]+"', String.Double),
            (r'\d+', Number),
            (r'\S+', String),
            (r'\s+', Text),
        ],
    }

    def analyse_text(text):
        if text[0] != '.':
            return False
        if text[:3] == '.\\"':
            return True
        if text[:4] == '.TH ':
            return True
        if text[1:3].isalnum() and text[3].isspace():
            return 0.9


class ApacheConfLexer(RegexLexer):
    """
    Lex Apache configuration like files.
    """
    name = 'ApacheConf'
    aliases = ['apacheconf', 'aconf', 'apache']
    filenames = ['.htaccess', 'apache.conf', 'apache2.conf']
    flags = re.MULTILINE | re.IGNORECASE

    tokens = {
        'root': [
            (r'^(\s*)(#.*?)$', bygroups(Text, Comment)),
            (r'\s+', Text),
            (r'(<[^\s>]+)(?:(\s+)(.*?))?(>)',
             bygroups(Name.Tag, Text, String, Name.Tag)),
            (r'([a-zA-Z][a-zA-Z0-9]*)(\s+)',
             bygroups(Name.Builtin, Text), 'value')
        ],
        'value': [
            (r'$', Text, '#pop'),
            (r'\s', Text), # XXX: \s+ does not work with examplefile
            (r'\d+', Number),
            (r'/([a-zA-Z0-9][a-zA-Z0-9_./-]+)', String.Other),
            (r'(on|off|none|any|all|double|email|dns|min|minimal|'
             r'os|productonly|full|emerg|alert|crit|error|warn|'
             r'notice|info|debug|registry|script|inetd|standalone|'
             r'user|group)\b', Keyword),
            (r'"([^"\\]*(?:\\.[^"\\]*)*)"', String.Double),
            (r'[^\s"]+', Text)
        ]
    }
