# -*- coding: utf-8 -*-
"""
    pygments.lexers.text
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for non-source code file types.

    :copyright: 2006-2007 by Armin Ronacher, Georg Brandl,
                Tim Hatch <tim@timhatch.com>,
                Ronny Pfannschmidt,
                Dennis Kaarsemaker,
                Kumar Appaiah <akumar@ee.iitm.ac.in>,
                Varun Hiremath <varunhiremath@gmail.com>,
                Jeremy Thurgood,
                Max Battcher.
    :license: BSD, see LICENSE for more details.
"""

import re
try:
    set
except NameError:
    from sets import Set as set
from bisect import bisect

from pygments.lexer import RegexLexer, bygroups, include, using, this, \
     do_insertions
from pygments.token import Punctuation, \
    Text, Comment, Keyword, Name, String, Generic, Operator, Number, \
    Whitespace, Literal
from pygments.util import get_bool_opt


__all__ = ['IniLexer', 'SourcesListLexer', 'MakefileLexer', 'DiffLexer',
           'IrcLogsLexer', 'TexLexer', 'GroffLexer', 'ApacheConfLexer',
           'BBCodeLexer', 'MoinWikiLexer', 'RstLexer', 'VimLexer',
           'GettextLexer', 'SquidConfLexer', 'DebianControlLexer',
           'DarcsPatchLexer']


class IniLexer(RegexLexer):
    """
    Lexer for configuration files in INI style.
    """

    name = 'INI'
    aliases = ['ini', 'cfg']
    filenames = ['*.ini', '*.cfg', '*.properties']
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
            (r'(cmdswitches|error|export|message|include|if|ifdef|ifndef|else|'
             r'else\s*if|else\s*ifdef|else\s*ifndef|endif|undef)\b', Keyword),
            # assignment
            (r'([a-zA-Z_][a-zA-Z0-9_]*)(\s*)([?:+]?=)([ \t]*)',
             bygroups(Name.Variable, Text, Operator, Text), 'var'),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r"'(\\\\|\\'|[^'])*'", String.Single),
            # targets
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
            (r'[^,\\\n#]+', Number),
            (r',', Punctuation),
            (r'#.*?\n', Comment),
            # line continuation
            (r'\\\n', Text),
            (r'\\', Text),
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


class DarcsPatchLexer(RegexLexer):
    """
    DarcsPatchLexer is a lexer for the various versions of the darcs patch
    format.  Examples of this format are derived by commands such as
    ``darcs annotate --patch`` and ``darcs send``.

    *New in Pygments 1.0.*
    """
    name = 'Darcs Patch'
    aliases = ['dpatch']
    filenames = ['*.dpatch', '*.darcspatch']

    tokens = {
        'root': [
            (r'<', Operator),
            (r'>', Operator),
            (r'{', Operator, 'patch'),
            (r'(\[)((?:TAG )?)(.*)(\n)(.*)(\*\*)(\d+)(\s?)', bygroups(Operator, Keyword, Name, Text,
                Name, Operator, Literal.Date, Text), 'comment'),
            (r'New patches:', Generic.Heading),
            (r'Context:', Generic.Heading),
            (r'Patch bundle hash:', Generic.Heading),
            (r'\s+|\w+', Text),
        ],
        'comment': [
            (r' .*\n', Comment),
            (r'\]', Operator, "#pop"),
        ],
        'patch': [
            (r'}', Operator, "#pop"),
            (r'(\w+)(.*\n)', bygroups(Keyword, Text)),
            (r'\+.*\n', Generic.Inserted),
            (r'-.*\n', Generic.Deleted),
            (r'.*\n', Text),
        ],
    }


class IrcLogsLexer(RegexLexer):
    """
    Lexer for IRC logs in *irssi*, *xchat* or *weechat* style.
    """

    name = 'IRC logs'
    aliases = ['irc']
    filenames = ['*.weechatlog']
    mimetypes = ['text/x-irclog']

    flags = re.VERBOSE | re.MULTILINE
    timestamp = r"""
        (
          # irssi / xchat and others
          (?: \[|\()?                  # Opening bracket or paren for the timestamp
            (?:                        # Timestamp
                (?: (?:\d{1,4} [-/]?)+ # Date as - or /-separated groups of digits
                 [T ])?                # Date/time separator: T or space
                (?: \d?\d [:.]?)+      # Time as :/.-separated groups of 1 or 2 digits
            )
          (?: \]|\))?\s+               # Closing bracket or paren for the timestamp
        |
          # weechat
          \d{4}\s\w{3}\s\d{2}\s        # Date
          \d{2}:\d{2}:\d{2}\s+         # Time + Whitespace
        )?
    """
    tokens = {
        'root': [
                # log start/end
            (r'^\*\*\*\*(.*)\*\*\*\*$', Comment),
            # hack
            ("^" + timestamp + r'(\s*<.*>\s*)$', bygroups(Comment.Preproc, Name.Tag)),
            # normal msgs
            ("^" + timestamp + r"""
                (\s*<.*?>\s*)          # Nick """,
             bygroups(Comment.Preproc, Name.Tag), 'msg'),
            # /me msgs
            ("^" + timestamp + r"""
                (\s*[*]\s+)            # Star
                ([^\s]+\s+.*?\n)       # Nick + rest of message """,
             bygroups(Comment.Preproc, Keyword, Generic.Inserted)),
            # join/part msgs
            ("^" + timestamp + r"""
                (\s*(?:\*{3}|<?-[!@=P]?->?)\s*)  # Star(s) or symbols
                ([^\s]+\s+)                     # Nick + Space
                (.*?\n)                         # Rest of message """,
             bygroups(Comment.Preproc, Keyword, String, Comment)),
            (r"^.*?\n", Text),
        ],
        'msg': [
            (r"[^\s]+:(?!//)", Name.Attribute),  # Prefix
            (r".*\n", Text, '#pop'),
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

    Additional options accepted:

    `handlecodeblocks`
        Highlight the contents of ``.. sourcecode:: langauge`` and
        ``.. code:: language`` directives with a lexer for the given
        language (default: ``True``). *New in Pygments 0.8.*
    """
    name = 'reStructuredText'
    aliases = ['rst', 'rest', 'restructuredtext']
    filenames = ['*.rst', '*.rest']
    mimetypes = ["text/x-rst"]
    flags = re.MULTILINE

    def _handle_sourcecode(self, match):
        from pygments.lexers import get_lexer_by_name
        from pygments.util import ClassNotFound

        # section header
        yield match.start(1), Punctuation, match.group(1)
        yield match.start(2), Text, match.group(2)
        yield match.start(3), Operator.Word, match.group(3)
        yield match.start(4), Punctuation, match.group(4)
        yield match.start(5), Text, match.group(5)
        yield match.start(6), Keyword, match.group(6)
        yield match.start(7), Text, match.group(7)

        # lookup lexer if wanted and existing
        lexer = None
        if self.handlecodeblocks:
            try:
                lexer = get_lexer_by_name(match.group(6).strip())
            except ClassNotFound:
                pass
        indention = match.group(8)
        indention_size = len(indention)
        code = (indention + match.group(9) + match.group(10) + match.group(11))

        # no lexer for this language. handle it like it was a code block
        if lexer is None:
            yield match.start(8), String, code
            return

        # highlight the lines with the lexer.
        ins = []
        codelines = code.splitlines(True)
        code = ''
        for line in codelines:
            if len(line) > indention_size:
                ins.append((len(code), [(0, Text, line[:indention_size])]))
                code += line[indention_size:]
            else:
                code += line
        for item in do_insertions(ins, lexer.get_tokens_unprocessed(code)):
            yield item

    tokens = {
        'root': [
            # Heading with overline
            (r'^(=+|-+|`+|:+|\.+|\'+|"+|~+|\^+|_+|\*+|\++|#+)([ \t]*\n)(.+)(\n)(\1)(\n)',
             bygroups(Generic.Heading, Text, Generic.Heading,
                      Text, Generic.Heading, Text)),
            # Plain heading
            (r'^(\S.*)(\n)(={3,}|-{3,}|`{3,}|:{3,}|\.{3,}|\'{3,}|"{3,}|'
             r'~{3,}|\^{3,}|_{3,}|\*{3,}|\+{3,}|#{3,})(\n)',
             bygroups(Generic.Heading, Text, Generic.Heading, Text)),
            # Bulleted lists
            (r'^(\s*)([-*+])( .+\n(?:\1  .+\n)*)',
             bygroups(Text, Number, using(this, state='inline'))),
            # Numbered lists
            (r'^(\s*)([0-9#ivxlcmIVXLCM]+\.)( .+\n(?:\1  .+\n)*)',
             bygroups(Text, Number, using(this, state='inline'))),
            (r'^(\s*)(\(?[0-9#ivxlcmIVXLCM]+\))( .+\n(?:\1  .+\n)*)',
             bygroups(Text, Number, using(this, state='inline'))),
            # Numbered, but keep words at BOL from becoming lists
            (r'^(\s*)([A-Z]+\.)( .+\n(?:\1  .+\n)+)',
             bygroups(Text, Number, using(this, state='inline'))),
            (r'^(\s*)(\(?[A-Za-z]+\))( .+\n(?:\1  .+\n)+)',
             bygroups(Text, Number, using(this, state='inline'))),
            # Sourcecode directives
            (r'^( *\.\.)(\s*)((?:source)?code)(::)([ \t]*)([^\n]+)'
             r'(\n[ \t]*\n)([ \t]+)(.*)(\n)((?:(?:\8.*|)\n)+)',
             _handle_sourcecode),
            # A directive
            (r'^( *\.\.)(\s*)(\w+)(::)(?:([ \t]*)(.+))?',
             bygroups(Punctuation, Text, Operator.Word, Punctuation, Text, Keyword)),
            # A reference target
            (r'^( *\.\.)(\s*)([\w\t ]+:)(.*?)$',
             bygroups(Punctuation, Text, Name.Tag, using(this, state='inline'))),
            # A footnote target
            (r'^( *\.\.)(\s*)(\[.+\])(.*?)$',
             bygroups(Punctuation, Text, Name.Tag, using(this, state='inline'))),
            # Comments
            (r'^ *\.\..*(\n( +.*\n|\n)+)?', Comment.Preproc),
            # Field list
            (r'^( *)(:.*?:)([ \t]+)(.*?)$', bygroups(Text, Name.Class, Text,
                                                     Name.Function)),
            # Definition list
            (r'^([^ ].*(?<!::)\n)((?:(?: +.*)\n)+)',
             bygroups(using(this, state='inline'), using(this, state='inline'))),
            # Code blocks
            (r'(::)(\n[ \t]*\n)([ \t]+)(.*)(\n)((?:(?:\3.*|)\n)+)',
             bygroups(String.Escape, Text, String, String, Text, String)),
            include('inline'),
        ],
        'inline': [
            (r'\\.', Text), # escape
            (r'``', String, 'literal'), # code
            (r'(`)(.+?)(`__?)',
             bygroups(Punctuation, using(this), Punctuation)), # reference
            (r'(`.+?`)(:[a-zA-Z0-9-]+?:)?',
             bygroups(Name.Variable, Name.Attribute)), # role
            (r'(:[a-zA-Z0-9-]+?:)(`.+?`)',
             bygroups(Name.Attribute, Name.Variable)), # user-defined role
            (r'\*\*.+?\*\*', Generic.Strong), # Strong emphasis
            (r'\*.+?\*', Generic.Emph), # Emphasis
            (r'\[.*?\]_', String), # Footnote or citation
            (r'<.+?>', Name.Tag), # Hyperlink
            (r'[^\\\n\[*`:]+', Text),
            (r'.', Text),
        ],
        'literal': [
            (r'[^`\\]+', String),
            (r'\\.', String),
            (r'``', String, '#pop'),
            (r'[`\\]', String),
        ]
    }

    def __init__(self, **options):
        self.handlecodeblocks = get_bool_opt(options, 'handlecodeblocks', True)
        RegexLexer.__init__(self, **options)


class VimLexer(RegexLexer):
    """
    Lexer for VimL script files.

    *New in Pygments 0.8.*
    """
    name = 'VimL'
    aliases = ['vim']
    filenames = ['*.vim', '.vimrc']
    mimetypes = ['text/x-vim']
    flags = re.MULTILINE

    tokens = {
        'root': [
            # Who decided that doublequote was a good comment character??
            (r'^\s*".*', Comment),
            (r'(?<=\s)"[^\-:.%#=*].*', Comment),

            (r'[ \t]+', Text),
            # TODO: regexes can have other delims
            (r'/(\\\\|\\/|[^\n/])*/', String.Regex),
            (r'"(\\\\|\\"|[^\n"])*"', String.Double),
            (r"'(\\\\|\\'|[^\n'])*'", String.Single),
            (r'-?\d+', Number),
            (r'^:', Punctuation),
            (r'[()<>+=!|,~-]', Punctuation), # Inexact list.  Looks decent.
            (r'\b(let|if|else|endif|elseif|fun|function|endfunction)\b',
             Keyword),
            (r'\b\w+\b', Name.Other), # These are postprocessed below
            (r'.', Text),
        ],
    }
    def __init__(self, **options):
        from pygments.lexers._vimbuiltins import command, option, auto
        self._cmd = command
        self._opt = option
        self._aut = auto

        RegexLexer.__init__(self, **options)

    def is_in(self, w, mapping):
        r"""
        It's kind of difficult to decide if something might be a keyword
        in VimL because it allows you to abbreviate them.  In fact,
        'ab[breviate]' is a good example.  :ab, :abbre, or :abbreviate are
        valid ways to call it so rather than making really awful regexps
        like::

            \bab(?:b(?:r(?:e(?:v(?:i(?:a(?:t(?:e)?)?)?)?)?)?)?)?\b

        we match `\b\w+\b` and then call is_in() on those tokens.  See
        `scripts/get_vimkw.py` for how the lists are extracted.
        """
        p = bisect(mapping, (w,))
        if p > 0:
            if mapping[p-1][0] == w[:len(mapping[p-1][0])] and \
               mapping[p-1][1][:len(w)] == w: return True
        if p < len(mapping):
            return mapping[p][0] == w[:len(mapping[p][0])] and \
                   mapping[p][1][:len(w)] == w
        return False

    def get_tokens_unprocessed(self, text):
        # TODO: builtins are only subsequent tokens on lines
        #       and 'keywords' only happen at the beginning except
        #       for :au ones
        for index, token, value in \
            RegexLexer.get_tokens_unprocessed(self, text):
            if token is Name.Other:
                if self.is_in(value, self._cmd):
                    yield index, Keyword, value
                elif self.is_in(value, self._opt) or \
                     self.is_in(value, self._aut):
                    yield index, Name.Builtin, value
                else:
                    yield index, Text, value
            else:
                yield index, token, value


class GettextLexer(RegexLexer):
    """
    Lexer for Gettext catalog files.

    *New in Pygments 0.9.*
    """
    name = 'Gettext Catalog'
    aliases = ['pot', 'po']
    filenames = ['*.pot', '*.po']
    mimetypes = ['application/x-gettext', 'text/x-gettext', 'text/gettext']

    tokens = {
        'root': [
            (r'^#,\s.*?$', Keyword.Type),
            (r'^#:\s.*?$', Keyword.Declaration),
            #(r'^#$', Comment),
            (r'^(#|#\.\s|#\|\s|#~\s|#\s).*$', Comment.Single),
            (r'^(")([\w-]*:)(.*")$',
             bygroups(String, Name.Property, String)),
            (r'^".*"$', String),
            (r'^(msgid|msgid_plural|msgstr)(\s+)(".*")$',
             bygroups(Name.Variable, Text, String)),
            (r'^(msgstr\[)(\d)(\])(\s+)(".*")$',
             bygroups(Name.Variable, Number.Integer, Name.Variable, Text, String)),
        ]
    }

class SquidConfLexer(RegexLexer):
    """
    Lexer for `squid <http://www.squid-cache.org/>`_ configuration files.

    *New in Pygments 0.9.*
    """

    name = 'SquidConf'
    aliases = ['squidconf', 'squid.conf', 'squid']
    filenames = ['squid.conf']
    mimetypes = ['text/x-squidconf']
    flags = re.IGNORECASE

    keywords = [ "acl", "always_direct", "announce_host",
                 "announce_period", "announce_port", "announce_to",
                 "anonymize_headers", "append_domain", "as_whois_server",
                 "auth_param_basic", "authenticate_children",
                 "authenticate_program", "authenticate_ttl", "broken_posts",
                 "buffered_logs", "cache_access_log", "cache_announce",
                 "cache_dir", "cache_dns_program", "cache_effective_group",
                 "cache_effective_user", "cache_host", "cache_host_acl",
                 "cache_host_domain", "cache_log", "cache_mem",
                 "cache_mem_high", "cache_mem_low", "cache_mgr",
                 "cachemgr_passwd", "cache_peer", "cache_peer_access",
                 "cahce_replacement_policy", "cache_stoplist",
                 "cache_stoplist_pattern", "cache_store_log", "cache_swap",
                 "cache_swap_high", "cache_swap_log", "cache_swap_low",
                 "client_db", "client_lifetime", "client_netmask",
                 "connect_timeout", "coredump_dir", "dead_peer_timeout",
                 "debug_options", "delay_access", "delay_class",
                 "delay_initial_bucket_level", "delay_parameters",
                 "delay_pools", "deny_info", "dns_children", "dns_defnames",
                 "dns_nameservers", "dns_testnames", "emulate_httpd_log",
                 "err_html_text", "fake_user_agent", "firewall_ip",
                 "forwarded_for", "forward_snmpd_port", "fqdncache_size",
                 "ftpget_options", "ftpget_program", "ftp_list_width",
                 "ftp_passive", "ftp_user", "half_closed_clients",
                 "header_access", "header_replace", "hierarchy_stoplist",
                 "high_response_time_warning", "high_page_fault_warning",
                 "htcp_port", "http_access", "http_anonymizer", "httpd_accel",
                 "httpd_accel_host", "httpd_accel_port",
                 "httpd_accel_uses_host_header", "httpd_accel_with_proxy",
                 "http_port", "http_reply_access", "icp_access",
                 "icp_hit_stale", "icp_port", "icp_query_timeout",
                 "ident_lookup", "ident_lookup_access", "ident_timeout",
                 "incoming_http_average", "incoming_icp_average",
                 "inside_firewall", "ipcache_high", "ipcache_low",
                 "ipcache_size", "local_domain", "local_ip", "logfile_rotate",
                 "log_fqdn", "log_icp_queries", "log_mime_hdrs",
                 "maximum_object_size", "maximum_single_addr_tries",
                 "mcast_groups", "mcast_icp_query_timeout", "mcast_miss_addr",
                 "mcast_miss_encode_key", "mcast_miss_port", "memory_pools",
                 "memory_pools_limit", "memory_replacement_policy",
                 "mime_table", "min_http_poll_cnt", "min_icp_poll_cnt",
                 "minimum_direct_hops", "minimum_object_size",
                 "minimum_retry_timeout", "miss_access", "negative_dns_ttl",
                 "negative_ttl", "neighbor_timeout", "neighbor_type_domain",
                 "netdb_high", "netdb_low", "netdb_ping_period",
                 "netdb_ping_rate", "never_direct", "no_cache",
                 "passthrough_proxy", "pconn_timeout", "pid_filename",
                 "pinger_program", "positive_dns_ttl", "prefer_direct",
                 "proxy_auth", "proxy_auth_realm", "query_icmp", "quick_abort",
                 "quick_abort", "quick_abort_max", "quick_abort_min",
                 "quick_abort_pct", "range_offset_limit", "read_timeout",
                 "redirect_children", "redirect_program",
                 "redirect_rewrites_host_header", "reference_age",
                 "reference_age", "refresh_pattern", "reload_into_ims",
                 "request_body_max_size", "request_size", "request_timeout",
                 "shutdown_lifetime", "single_parent_bypass",
                 "siteselect_timeout", "snmp_access", "snmp_incoming_address",
                 "snmp_port", "source_ping", "ssl_proxy",
                 "store_avg_object_size", "store_objects_per_bucket",
                 "strip_query_terms", "swap_level1_dirs", "swap_level2_dirs",
                 "tcp_incoming_address", "tcp_outgoing_address",
                 "tcp_recv_bufsize", "test_reachability", "udp_hit_obj",
                 "udp_hit_obj_size", "udp_incoming_address",
                 "udp_outgoing_address", "unique_hostname", "unlinkd_program",
                 "uri_whitespace", "useragent_log", "visible_hostname",
                 "wais_relay", "wais_relay_host", "wais_relay_port",
                 ]

    opts = [ "proxy-only", "weight", "ttl", "no-query", "default",
             "round-robin", "multicast-responder", "on", "off", "all",
             "deny", "allow", "via", "parent", "no-digest", "heap", "lru",
             "realm", "children", "credentialsttl", "none", "disable",
             "offline_toggle", "diskd", "q1", "q2",
             ]

    actions = [ "shutdown", "info", "parameter", "server_list",
                "client_list", r'squid\.conf',
                ]

    actions_stats = [ "objects", "vm_objects", "utilization",
                      "ipcache", "fqdncache", "dns", "redirector", "io",
                      "reply_headers", "filedescriptors", "netdb",
                      ]

    actions_log = [ "status", "enable", "disable", "clear"]

    acls = [ "url_regex", "urlpath_regex", "referer_regex", "port",
             "proto", "req_mime_type", "rep_mime_type", "method",
             "browser", "user", "src", "dst", "time", "dstdomain", "ident",
             "snmp_community",
             ]

    ip_re = r'\b(?:\d{1,3}\.){3}\d{1,3}\b'

    def makelistre(list):
        return r'\b(?:'+'|'.join(list)+r')\b'

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'#', Comment, 'comment'),
            (makelistre(keywords), Keyword),
            (makelistre(opts), Name.Constant),
            # Actions
            (makelistre(actions), String),
            (r'stats/'+makelistre(actions), String),
            (r'log/'+makelistre(actions)+r'=', String),
            (makelistre(acls), Keyword),
            (ip_re+r'(?:/(?:'+ip_re+r')|\d+)?', Number),
            (r'\b\d+\b', Number),
            (r'\S+', Text),
        ],
        'comment': [
            (r'\s*TAG:.*', String.Escape, '#pop'),
            (r'.*', Comment, '#pop'),
        ],
    }


class DebianControlLexer(RegexLexer):
    """
    Lexer for Debian ``control`` files and ``apt-cache show <pkg>`` outputs.

    *New in Pygments 0.9.*
    """
    name = 'Debian Control file'
    aliases = ['control']
    filenames = ['control']

    tokens = {
        'root': [
            (r'^(Description)', Keyword, 'description'),
            (r'^(Maintainer)(:\s*)', bygroups(Keyword, Text), 'maintainer'),
            (r'^((Build-)?Depends)', Keyword, 'depends'),
            (r'^((?:Python-)?Version)(:\s*)([^\s]+)$',
             bygroups(Keyword, Text, Number)),
            (r'^((?:Installed-)?Size)(:\s*)([^\s]+)$',
             bygroups(Keyword, Text, Number)),
            (r'^(MD5Sum|SHA1|SHA256)(:\s*)([^\s]+)$',
             bygroups(Keyword, Text, Number)),
            (r'^([a-zA-Z\-0-9\.]*?)(:\s*)(.*?)$',
             bygroups(Keyword, Whitespace, String)),
        ],
        'maintainer': [
            (r'<[^>]+>', Generic.Strong),
            (r'<[^>]+>$', Generic.Strong, '#pop'),
            (r',\n?', Text),
            (r'.', Text),
        ],
        'description': [
            (r'(.*)(Homepage)(: )([^\s]+)', bygroups(Text, String, Name, Name.Class)),
            (r':.*\n', Generic.Strong),
            (r' .*\n', Text),
            ('', Text, '#pop'),
        ],
        'depends': [
            (r':\s*', Text),
            (r'(\$)(\{)(\w+\s*:\s*\w+)', bygroups(Operator, Text, Name.Entity)),
            (r'\(', Text, 'depend_vers'),
            (r',', Text),
            (r'\|', Operator),
            (r'[\s]+', Text),
            (r'[}\)]\s*$', Text, '#pop'),
            (r'[}]', Text),
            (r'[^,]$', Name.Function, '#pop'),
            (r'([\+\.a-zA-Z0-9-][\s\n]*)', Name.Function),
        ],
        'depend_vers': [
            (r'\),', Text, '#pop'),
            (r'\)[^,]', Text, '#pop:2'),
            (r'([><=]+)(\s*)([^\)]+)', bygroups(Operator, Text, Number))
        ]
    }
