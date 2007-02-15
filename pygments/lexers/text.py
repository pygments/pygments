# -*- coding: utf-8 -*-
"""
    pygments.lexers.text
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for non-source code file types.

    :copyright: 2006-2007 by Armin Ronacher, Georg Brandl,
                Tim Hatch <tim@timhatch.com>,
                Ronny Pfannschmidt,
                Dennis Kaarsemaker.
    :license: BSD, see LICENSE for more details.
"""

import re
try:
    set
except NameError:
    from sets import Set as set

from pygments.lexer import RegexLexer, bygroups, include, using, this
from pygments.token import Punctuation, \
    Text, Comment, Keyword, Name, String, Generic, Operator, Number


__all__ = ['IniLexer', 'SourcesListLexer', 'MakefileLexer', 'DiffLexer',
           'IrcLogsLexer', 'TexLexer', 'GroffLexer', 'ApacheConfLexer',
           'BBCodeLexer', 'MoinWikiLexer', 'RstLexer']


class IniLexer(RegexLexer):
    """
    Lexer for configuration files in INI style.
    """

    name = 'INI'
    aliases = ['ini', 'cfg']
    filenames = ['*.ini', '*.cfg']
    mimetypes = ['text/x-ini']

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


class SourcesListLexer(RegexLexer):
    """
    Lexer that highlights debian sources.list files.

    *New in Pygments 0.7.*
    """

    name = 'Debian Sourcelist'
    aliases = ['sourceslist', 'sources.list']
    filenames = ['sources.list']
    mimetype = ['application/x-debian-sourceslist']

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'#.*?$', Comment),
            (r'^(deb(?:-src)?)(\s+)',
             bygroups(Keyword, Text), 'distribution')
        ],
        'distribution': [
            (r'#.*?$', Comment, '#pop'),
            (r'\$\(ARCH\)', Name.Variable),
            (r'[^\s$[]+', String),
            (r'\[', String.Other, 'escaped-distribution'),
            (r'\$', String),
            (r'\s+', Text, 'components')
        ],
        'escaped-distribution': [
            (r'\]', String.Other, '#pop'),
            (r'\$\(ARCH\)', Name.Variable),
            (r'[^\]$]+', String.Other),
            (r'\$', String.Other)
        ],
        'components': [
            (r'#.*?$', Comment, '#pop:2'),
            (r'$', Text, '#pop:2'),
            (r'\s+', Text),
            (r'\S+', Keyword.Pseudo),
        ]
    }

    def analyse_text(text):
        for line in text.split('\n'):
            line = line.strip()
            if not (line.startswith('#') or line.startswith('deb ') or
                    line.startswith('deb-src ') or not line):
                return False
        return True


class MakefileLexer(RegexLexer):
    """
    Lexer for Makefiles.
    """

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
    """
    Lexer for unified or context-style diffs or patches.
    """

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
    """
    Lexer for IRC logs in **irssi** or **xchat** style.
    """

    name = 'IRC logs'
    aliases = ['irc']
    mimetypes = ['text/x-irclog']

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
    """
    A lexer that highlights BBCode(-like) syntax.

    *New in Pygments 0.6.*
    """

    name = 'BBCode'
    aliases = ['bbcode']
    mimetypes = ['text/x-bbcode']

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
    """
    Lexer for the TeX and LaTeX typesetting languages.
    """

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
    Lexer for the (g)roff typesetting language, supporting groff
    extensions. Mainly useful for highlighting manpage sources.

    *New in Pygments 0.6.*
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
    Lexer for configuration files following the Apache config file
    format.

    *New in Pygments 0.6.*
    """

    name = 'ApacheConf'
    aliases = ['apacheconf', 'aconf', 'apache']
    filenames = ['.htaccess', 'apache.conf', 'apache2.conf']
    mimetypes = ['text/x-apacheconf']
    flags = re.MULTILINE | re.IGNORECASE

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'(#.*?)$', Comment),
            (r'(<[^\s>]+)(?:(\s+)(.*?))?(>)',
             bygroups(Name.Tag, Text, String, Name.Tag)),
            (r'([a-zA-Z][a-zA-Z0-9]*)(\s+)',
             bygroups(Name.Builtin, Text), 'value')
        ],
        'value': [
            (r'$', Text, '#pop'),
            (r'[^\S\n]+', Text),
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


class MoinWikiLexer(RegexLexer):
    """
    For MoinMoin (and Trac) Wiki markup.

    *New in Pygments 0.7.*
    """

    name = 'MoinMoin/Trac Wiki markup'
    aliases = ['trac-wiki', 'moin']
    filenames = []
    mimetypes = ['text/x-trac-wiki']
    flags = re.MULTILINE | re.IGNORECASE

    tokens = {
        'root': [
            (r'^#.*$', Comment),
            (r'(!)(\S+)', bygroups(Keyword, Text)), # Ignore-next
            # Titles
            (r'^(=+)([^=]+)(=+)(\s*#.+)?$',
             bygroups(Generic.Heading, using(this), Generic.Heading, String)),
            # Literal code blocks, with optional shebang
            (r'({{{)(\n#!.+)?', bygroups(Name.Builtin, Name.Namespace), 'codeblock'),
            (r'(\'\'\'?|\|\||`|__|~~|\^|,,|::)', Comment), # Formatting
            # Lists
            (r'^( +)([.*-])( )', bygroups(Text, Name.Builtin, Text)),
            (r'^( +)([a-zivx]{1,5}\.)( )', bygroups(Text, Name.Builtin, Text)),
            # Other Formatting
            (r'\[\[\w+.*?\]\]', Keyword), # Macro
            (r'(\[[^\s\]]+)(\s+[^\]]+?)?(\])',
             bygroups(Keyword, String, Keyword)), # Link
            (r'^----+$', Keyword), # Horizontal rules
            (r'[^\n\'\[{!_~^,|]+', Text),
            (r'\n', Text),
            (r'.', Text),
        ],
        'codeblock': [
            (r'}}}', Name.Builtin, '#pop'),
            # these blocks are allowed to be nested in Trac, but not MoinMoin
            (r'{{{', Text, '#push'),
            (r'[^{}]+', Comment.Preproc), # slurp boring text
            (r'.', Comment.Preproc), # allow loose { or }
        ],
    }


class RstLexer(RegexLexer):
    """
    For `reStructuredText <http://docutils.sf.net/rst.html>`_ markup.

    *New in Pygments 0.7.*
    """
    name = 'reStructuredText'
    aliases = ['rst', 'restructuredtext']
    filenames = ['*.rst', '*.rest']
    mimetypes = ["text/x-rst"]
    flags = re.MULTILINE

    tokens = {
        'root': [
            # Heading with overline
            (r'^(=+|-+|`+|:+|\.+|\'+|"+|~+|\^+|_+|\*+|\++|#+)(\n)(.+)(\n)(\1)(\n)',
             bygroups(Generic.Heading, Text, using(this, state='inline'),
             Text, Generic.Heading, Text)),
            # Plain heading
            (r'^(\S.*)(\n)(=+|-+|`+|:+|\.+|\'+|"+|~+|\^+|_+|\*+|\++|#+)(\n)',
             bygroups(Generic.Heading, Text, Generic.Heading, Text)),

            # Bulleted lists
            (r'^(\s*)([-*+])( .+\n(?:\1  .+\n)*)',
             bygroups(Text, Keyword, using(this, state='inline'))),
            # Numbered lists
            (r'^(\s*)([0-9#ivxlcmIVXLCM]+\.)( .+\n(?:\1  .+\n)*)',
             bygroups(Text, Keyword, using(this, state='inline'))),
            (r'^(\s*)(\(?[0-9#ivxlcmIVXLCM]+\))( .+\n(?:\1  .+\n)*)',
             bygroups(Text, Keyword, using(this, state='inline'))),
            # Numbered, but keep words at BOL from becoming lists
            (r'^(\s*)([A-Z]+\.)( .+\n(?:\1  .+\n)+)',
             bygroups(Text, Keyword, using(this, state='inline'))),
            (r'^(\s*)(\(?[A-Za-z]+\))( .+\n(?:\1  .+\n)+)',
             bygroups(Text, Keyword, using(this, state='inline'))),
            # Introducing a section
            (r'^( *\.\.)(\s*)(\w+)(::)(?:(\s*)(.+))?(\n(?:(?: +.*|)\n)+)$',
             bygroups(Punctuation, Text, Number, Punctuation, Text, Number, Text)),
            # A reference target
            (r'^( *\.\.)(\s*)(\w+:)(.*?)$',
             bygroups(Punctuation, Text, Name.Tag, using(this, state='inline'))),
            # A footnote target
            (r'^( *\.\.)(\s*)(\[.+\])(.*?)$',
             bygroups(Punctuation, Text, Name.Tag, using(this, state='inline'))),
            # Comments
            (r'^ *\.\..*(\n( +.*\n|\n)+)?', Comment.Preproc),
            # Definition list
            (r'^([^ ].*(?<!::)\n)((?:(?: +.*)\n)+)',
             bygroups(using(this, state='inline'), using(this, state='inline'))),
            # Code blocks
            (r'(::)(\n)((?:(?: +.*|)\n)+)',
             bygroups(String.Escape, Text, String)),
            include('inline'),
        ],
        'inline': [
            (r'``.+?``', String), # code
            # Phrase reference
            (r'(``?)(.+?)(\1__?)',
             bygroups(Punctuation, using(this), Punctuation)),
            (r'`.+?`', Name),
            (r'\*\*.+?\*\*', String), # Strong emphasis
            (r'\*.+?\*', Number), # Emphasis
            (r'\[.*?\]_', String), # Footnote or citation
            (r'<.+?>', Name.Tag),
            (r'[^\n\[*`:]+', Text),
            (r'.', Text),
        ],
    }
