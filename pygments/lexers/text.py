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
from bisect import bisect

from pygments.lexer import RegexLexer, bygroups, include, using, this, \
     do_insertions
from pygments.token import Punctuation, \
    Text, Comment, Keyword, Name, String, Generic, Operator, Number, \
    Whitespace
from pygments.util import get_bool_opt


__all__ = ['IniLexer', 'SourcesListLexer', 'MakefileLexer', 'DiffLexer',
           'IrcLogsLexer', 'TexLexer', 'GroffLexer', 'ApacheConfLexer',
           'BBCodeLexer', 'MoinWikiLexer', 'RstLexer', 'VimLexer',
           'GettextLexer', 'WeechatLogLexer']


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


class WeechatLogLexer(RegexLexer):
	"""
	Lexer for weechat log files.
	
	*New in Pygments 0.9.*
	"""
	name = 'Weechat Log'
	aliases = ['weechatlog']
	filenames = ['*.weechatlog']
	mimetypes = ['text/x-weechatlog']
	
	tokens = {
		'root' : [
			# date
			(r'(\d{4} \w{3} \d{2} \d{2}:\d{2}:\d{2})(\s+)',
				bygroups(Comment.Preproc, Whitespace)),
			# operators
			(r'(-=-|<--|-->|-@-|-P-)(.*)', bygroups(Operator, Text)),
			# messages
			(r'(<)(\w+)(>)(.*)', bygroups(Operator, Name, Operator, Text)),
			# log start/end
			(r'(\*\*\*\*[\w\s]+)(\d{4} \w{3} \d{2} \d{2}:\d{2}:\d{2}\s+)(\*\*\*\*)',
				Comment)
		]
	}
