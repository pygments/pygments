"""
    pygments.lexers.markup
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for non-HTML markup languages.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexers.html import XmlLexer
from pygments.lexers.javascript import JavascriptLexer
from pygments.lexers.css import CssLexer
from pygments.lexers.lilypond import LilyPondLexer
from pygments.lexers.data import JsonLexer

from pygments.lexer import RegexLexer, DelegatingLexer, include, bygroups, \
    using, this, do_insertions, default, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Generic, Other, Whitespace
from pygments.util import get_bool_opt, ClassNotFound

__all__ = ['BBCodeLexer', 'MoinWikiLexer', 'RstLexer', 'TexLexer', 'GroffLexer',
           'MozPreprocHashLexer', 'MozPreprocPercentLexer',
           'MozPreprocXulLexer', 'MozPreprocJavascriptLexer',
           'MozPreprocCssLexer', 'MarkdownLexer', 'TiddlyWiki5Lexer', 'WikitextLexer']


class BBCodeLexer(RegexLexer):
    """
    A lexer that highlights BBCode(-like) syntax.

    .. versionadded:: 0.6
    """

    name = 'BBCode'
    aliases = ['bbcode']
    mimetypes = ['text/x-bbcode']

    tokens = {
        'root': [
            (r'[^[]+', Text),
            # tag/end tag begin
            (r'\[/?\w+', Keyword, 'tag'),
            # stray bracket
            (r'\[', Text),
        ],
        'tag': [
            (r'\s+', Text),
            # attribute with value
            (r'(\w+)(=)("?[^\s"\]]+"?)',
             bygroups(Name.Attribute, Operator, String)),
            # tag argument (a la [color=green])
            (r'(=)("?[^\s"\]]+"?)',
             bygroups(Operator, String)),
            # tag end
            (r'\]', Keyword, '#pop'),
        ],
    }


class MoinWikiLexer(RegexLexer):
    """
    For MoinMoin (and Trac) Wiki markup.

    .. versionadded:: 0.7
    """

    name = 'MoinMoin/Trac Wiki markup'
    aliases = ['trac-wiki', 'moin']
    filenames = []
    mimetypes = ['text/x-trac-wiki']
    flags = re.MULTILINE | re.IGNORECASE

    tokens = {
        'root': [
            (r'^#.*$', Comment),
            (r'(!)(\S+)', bygroups(Keyword, Text)),  # Ignore-next
            # Titles
            (r'^(=+)([^=]+)(=+)(\s*#.+)?$',
             bygroups(Generic.Heading, using(this), Generic.Heading, String)),
            # Literal code blocks, with optional shebang
            (r'(\{\{\{)(\n#!.+)?', bygroups(Name.Builtin, Name.Namespace), 'codeblock'),
            (r'(\'\'\'?|\|\||`|__|~~|\^|,,|::)', Comment),  # Formatting
            # Lists
            (r'^( +)([.*-])( )', bygroups(Text, Name.Builtin, Text)),
            (r'^( +)([a-z]{1,5}\.)( )', bygroups(Text, Name.Builtin, Text)),
            # Other Formatting
            (r'\[\[\w+.*?\]\]', Keyword),  # Macro
            (r'(\[[^\s\]]+)(\s+[^\]]+?)?(\])',
             bygroups(Keyword, String, Keyword)),  # Link
            (r'^----+$', Keyword),  # Horizontal rules
            (r'[^\n\'\[{!_~^,|]+', Text),
            (r'\n', Text),
            (r'.', Text),
        ],
        'codeblock': [
            (r'\}\}\}', Name.Builtin, '#pop'),
            # these blocks are allowed to be nested in Trac, but not MoinMoin
            (r'\{\{\{', Text, '#push'),
            (r'[^{}]+', Comment.Preproc),  # slurp boring text
            (r'.', Comment.Preproc),  # allow loose { or }
        ],
    }


class RstLexer(RegexLexer):
    """
    For reStructuredText markup.

    .. versionadded:: 0.7

    Additional options accepted:

    `handlecodeblocks`
        Highlight the contents of ``.. sourcecode:: language``,
        ``.. code:: language`` and ``.. code-block:: language``
        directives with a lexer for the given language (default:
        ``True``).

        .. versionadded:: 0.8
    """
    name = 'reStructuredText'
    url = 'https://docutils.sourceforge.io/rst.html'
    aliases = ['restructuredtext', 'rst', 'rest']
    filenames = ['*.rst', '*.rest']
    mimetypes = ["text/x-rst", "text/prs.fallenstein.rst"]
    flags = re.MULTILINE

    def _handle_sourcecode(self, match):
        from pygments.lexers import get_lexer_by_name

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
        yield from do_insertions(ins, lexer.get_tokens_unprocessed(code))

    # from docutils.parsers.rst.states
    closers = '\'")]}>\u2019\u201d\xbb!?'
    unicode_delimiters = '\u2010\u2011\u2012\u2013\u2014\u00a0'
    end_string_suffix = (r'((?=$)|(?=[-/:.,; \n\x00%s%s]))'
                         % (re.escape(unicode_delimiters),
                            re.escape(closers)))

    tokens = {
        'root': [
            # Heading with overline
            (r'^(=+|-+|`+|:+|\.+|\'+|"+|~+|\^+|_+|\*+|\++|#+)([ \t]*\n)'
             r'(.+)(\n)(\1)(\n)',
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
            # Line blocks
            (r'^(\s*)(\|)( .+\n(?:\|  .+\n)*)',
             bygroups(Text, Operator, using(this, state='inline'))),
            # Sourcecode directives
            (r'^( *\.\.)(\s*)((?:source)?code(?:-block)?)(::)([ \t]*)([^\n]+)'
             r'(\n[ \t]*\n)([ \t]+)(.*)(\n)((?:(?:\8.*)?\n)+)',
             _handle_sourcecode),
            # A directive
            (r'^( *\.\.)(\s*)([\w:-]+?)(::)(?:([ \t]*)(.*))',
             bygroups(Punctuation, Text, Operator.Word, Punctuation, Text,
                      using(this, state='inline'))),
            # A reference target
            (r'^( *\.\.)(\s*)(_(?:[^:\\]|\\.)+:)(.*?)$',
             bygroups(Punctuation, Text, Name.Tag, using(this, state='inline'))),
            # A footnote/citation target
            (r'^( *\.\.)(\s*)(\[.+\])(.*?)$',
             bygroups(Punctuation, Text, Name.Tag, using(this, state='inline'))),
            # A substitution def
            (r'^( *\.\.)(\s*)(\|.+\|)(\s*)([\w:-]+?)(::)(?:([ \t]*)(.*))',
             bygroups(Punctuation, Text, Name.Tag, Text, Operator.Word,
                      Punctuation, Text, using(this, state='inline'))),
            # Comments
            (r'^ *\.\..*(\n( +.*\n|\n)+)?', Comment.Preproc),
            # Field list marker
            (r'^( *)(:(?:\\\\|\\:|[^:\n])+:(?=\s))([ \t]*)',
             bygroups(Text, Name.Class, Text)),
            # Definition list
            (r'^(\S.*(?<!::)\n)((?:(?: +.*)\n)+)',
             bygroups(using(this, state='inline'), using(this, state='inline'))),
            # Code blocks
            (r'(::)(\n[ \t]*\n)([ \t]+)(.*)(\n)((?:(?:\3.*)?\n)+)',
             bygroups(String.Escape, Text, String, String, Text, String)),
            include('inline'),
        ],
        'inline': [
            (r'\\.', Text),  # escape
            (r'``', String, 'literal'),  # code
            (r'(`.+?)(<.+?>)(`__?)',  # reference with inline target
             bygroups(String, String.Interpol, String)),
            (r'`.+?`__?', String),  # reference
            (r'(`.+?`)(:[a-zA-Z0-9:-]+?:)?',
             bygroups(Name.Variable, Name.Attribute)),  # role
            (r'(:[a-zA-Z0-9:-]+?:)(`.+?`)',
             bygroups(Name.Attribute, Name.Variable)),  # role (content first)
            (r'\*\*.+?\*\*', Generic.Strong),  # Strong emphasis
            (r'\*.+?\*', Generic.Emph),  # Emphasis
            (r'\[.*?\]_', String),  # Footnote or citation
            (r'<.+?>', Name.Tag),   # Hyperlink
            (r'[^\\\n\[*`:]+', Text),
            (r'.', Text),
        ],
        'literal': [
            (r'[^`]+', String),
            (r'``' + end_string_suffix, String, '#pop'),
            (r'`', String),
        ]
    }

    def __init__(self, **options):
        self.handlecodeblocks = get_bool_opt(options, 'handlecodeblocks', True)
        RegexLexer.__init__(self, **options)

    def analyse_text(text):
        if text[:2] == '..' and text[2:3] != '.':
            return 0.3
        p1 = text.find("\n")
        p2 = text.find("\n", p1 + 1)
        if (p2 > -1 and              # has two lines
                p1 * 2 + 1 == p2 and     # they are the same length
                text[p1+1] in '-=' and   # the next line both starts and ends with
                text[p1+1] == text[p2-1]):  # ...a sufficiently high header
            return 0.5


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
            (r'\\$', Keyword),
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
            default('#pop'),
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

    .. versionadded:: 0.6
    """

    name = 'Groff'
    aliases = ['groff', 'nroff', 'man']
    filenames = ['*.[1-9]', '*.man', '*.1p', '*.3pm']
    mimetypes = ['application/x-troff', 'text/troff']

    tokens = {
        'root': [
            (r'(\.)(\w+)', bygroups(Text, Keyword), 'request'),
            (r'\.', Punctuation, 'request'),
            # Regular characters, slurp till we find a backslash or newline
            (r'[^\\\n]+', Text, 'textline'),
            default('textline'),
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
            (r'\\\(.{2}', String.Escape),
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
        if text[:1] != '.':
            return False
        if text[:3] == '.\\"':
            return True
        if text[:4] == '.TH ':
            return True
        if text[1:3].isalnum() and text[3].isspace():
            return 0.9


class MozPreprocHashLexer(RegexLexer):
    """
    Lexer for Mozilla Preprocessor files (with '#' as the marker).

    Other data is left untouched.

    .. versionadded:: 2.0
    """
    name = 'mozhashpreproc'
    aliases = [name]
    filenames = []
    mimetypes = []

    tokens = {
        'root': [
            (r'^#', Comment.Preproc, ('expr', 'exprstart')),
            (r'.+', Other),
        ],
        'exprstart': [
            (r'(literal)(.*)', bygroups(Comment.Preproc, Text), '#pop:2'),
            (words((
                'define', 'undef', 'if', 'ifdef', 'ifndef', 'else', 'elif',
                'elifdef', 'elifndef', 'endif', 'expand', 'filter', 'unfilter',
                'include', 'includesubst', 'error')),
             Comment.Preproc, '#pop'),
        ],
        'expr': [
            (words(('!', '!=', '==', '&&', '||')), Operator),
            (r'(defined)(\()', bygroups(Keyword, Punctuation)),
            (r'\)', Punctuation),
            (r'[0-9]+', Number.Decimal),
            (r'__\w+?__', Name.Variable),
            (r'@\w+?@', Name.Class),
            (r'\w+', Name),
            (r'\n', Text, '#pop'),
            (r'\s+', Text),
            (r'\S', Punctuation),
        ],
    }


class MozPreprocPercentLexer(MozPreprocHashLexer):
    """
    Lexer for Mozilla Preprocessor files (with '%' as the marker).

    Other data is left untouched.

    .. versionadded:: 2.0
    """
    name = 'mozpercentpreproc'
    aliases = [name]
    filenames = []
    mimetypes = []

    tokens = {
        'root': [
            (r'^%', Comment.Preproc, ('expr', 'exprstart')),
            (r'.+', Other),
        ],
    }


class MozPreprocXulLexer(DelegatingLexer):
    """
    Subclass of the `MozPreprocHashLexer` that highlights unlexed data with the
    `XmlLexer`.

    .. versionadded:: 2.0
    """
    name = "XUL+mozpreproc"
    aliases = ['xul+mozpreproc']
    filenames = ['*.xul.in']
    mimetypes = []

    def __init__(self, **options):
        super().__init__(XmlLexer, MozPreprocHashLexer, **options)


class MozPreprocJavascriptLexer(DelegatingLexer):
    """
    Subclass of the `MozPreprocHashLexer` that highlights unlexed data with the
    `JavascriptLexer`.

    .. versionadded:: 2.0
    """
    name = "Javascript+mozpreproc"
    aliases = ['javascript+mozpreproc']
    filenames = ['*.js.in']
    mimetypes = []

    def __init__(self, **options):
        super().__init__(JavascriptLexer, MozPreprocHashLexer, **options)


class MozPreprocCssLexer(DelegatingLexer):
    """
    Subclass of the `MozPreprocHashLexer` that highlights unlexed data with the
    `CssLexer`.

    .. versionadded:: 2.0
    """
    name = "CSS+mozpreproc"
    aliases = ['css+mozpreproc']
    filenames = ['*.css.in']
    mimetypes = []

    def __init__(self, **options):
        super().__init__(CssLexer, MozPreprocPercentLexer, **options)


class MarkdownLexer(RegexLexer):
    """
    For Markdown markup.

    .. versionadded:: 2.2
    """
    name = 'Markdown'
    url = 'https://daringfireball.net/projects/markdown/'
    aliases = ['markdown', 'md']
    filenames = ['*.md', '*.markdown']
    mimetypes = ["text/x-markdown"]
    flags = re.MULTILINE

    def _handle_codeblock(self, match):
        """
        match args: 1:backticks, 2:lang_name, 3:newline, 4:code, 5:backticks
        """
        from pygments.lexers import get_lexer_by_name

        # section header
        yield match.start(1), String.Backtick, match.group(1)
        yield match.start(2), String.Backtick, match.group(2)
        yield match.start(3), Text           , match.group(3)

        # lookup lexer if wanted and existing
        lexer = None
        if self.handlecodeblocks:
            try:
                lexer = get_lexer_by_name( match.group(2).strip() )
            except ClassNotFound:
                pass
        code = match.group(4)

        # no lexer for this language. handle it like it was a code block
        if lexer is None:
            yield match.start(4), String, code
        else:
            yield from do_insertions([], lexer.get_tokens_unprocessed(code))

        yield match.start(5), String.Backtick, match.group(5)

    tokens = {
        'root': [
            # heading with '#' prefix (atx-style)
            (r'(^#[^#].+)(\n)', bygroups(Generic.Heading, Text)),
            # subheading with '#' prefix (atx-style)
            (r'(^#{2,6}[^#].+)(\n)', bygroups(Generic.Subheading, Text)),
            # heading with '=' underlines (Setext-style)
            (r'^(.+)(\n)(=+)(\n)', bygroups(Generic.Heading, Text, Generic.Heading, Text)),
            # subheading with '-' underlines (Setext-style)
            (r'^(.+)(\n)(-+)(\n)', bygroups(Generic.Subheading, Text, Generic.Subheading, Text)),
            # task list
            (r'^(\s*)([*-] )(\[[ xX]\])( .+\n)',
            bygroups(Whitespace, Keyword, Keyword, using(this, state='inline'))),
            # bulleted list
            (r'^(\s*)([*-])(\s)(.+\n)',
            bygroups(Whitespace, Keyword, Whitespace, using(this, state='inline'))),
            # numbered list
            (r'^(\s*)([0-9]+\.)( .+\n)',
            bygroups(Whitespace, Keyword, using(this, state='inline'))),
            # quote
            (r'^(\s*>\s)(.+\n)', bygroups(Keyword, Generic.Emph)),
            # code block fenced by 3 backticks
            (r'^(\s*```\n[\w\W]*?^\s*```$\n)', String.Backtick),
            # code block with language
            (r'^(\s*```)(\w+)(\n)([\w\W]*?)(^\s*```$\n)', _handle_codeblock),

            include('inline'),
        ],
        'inline': [
            # escape
            (r'\\.', Text),
            # inline code
            (r'([^`]?)(`[^`\n]+`)', bygroups(Text, String.Backtick)),
            # warning: the following rules eat outer tags.
            # eg. **foo _bar_ baz** => foo and baz are not recognized as bold
            # bold fenced by '**'
            (r'([^\*]?)(\*\*[^* \n][^*\n]*\*\*)', bygroups(Text, Generic.Strong)),
            # bold fenced by '__'
            (r'([^_]?)(__[^_ \n][^_\n]*__)', bygroups(Text, Generic.Strong)),
            # italics fenced by '*'
            (r'([^\*]?)(\*[^* \n][^*\n]*\*)', bygroups(Text, Generic.Emph)),
            # italics fenced by '_'
            (r'([^_]?)(_[^_ \n][^_\n]*_)', bygroups(Text, Generic.Emph)),
            # strikethrough
            (r'([^~]?)(~~[^~ \n][^~\n]*~~)', bygroups(Text, Generic.Deleted)),
            # mentions and topics (twitter and github stuff)
            (r'[@#][\w/:]+', Name.Entity),
            # (image?) links eg: ![Image of Yaktocat](https://octodex.github.com/images/yaktocat.png)
            (r'(!?\[)([^]]+)(\])(\()([^)]+)(\))',
             bygroups(Text, Name.Tag, Text, Text, Name.Attribute, Text)),
            # reference-style links, e.g.:
            #   [an example][id]
            #   [id]: http://example.com/
            (r'(\[)([^]]+)(\])(\[)([^]]*)(\])',
             bygroups(Text, Name.Tag, Text, Text, Name.Label, Text)),
            (r'^(\s*\[)([^]]*)(\]:\s*)(.+)',
             bygroups(Text, Name.Label, Text, Name.Attribute)),

            # general text, must come last!
            (r'[^\\\s]+', Text),
            (r'.', Text),
        ],
    }

    def __init__(self, **options):
        self.handlecodeblocks = get_bool_opt(options, 'handlecodeblocks', True)
        RegexLexer.__init__(self, **options)


class TiddlyWiki5Lexer(RegexLexer):
    """
    For TiddlyWiki5 markup.

    .. versionadded:: 2.7
    """
    name = 'tiddler'
    url = 'https://tiddlywiki.com/#TiddlerFiles'
    aliases = ['tid']
    filenames = ['*.tid']
    mimetypes = ["text/vnd.tiddlywiki"]
    flags = re.MULTILINE

    def _handle_codeblock(self, match):
        """
        match args: 1:backticks, 2:lang_name, 3:newline, 4:code, 5:backticks
        """
        from pygments.lexers import get_lexer_by_name

        # section header
        yield match.start(1), String, match.group(1)
        yield match.start(2), String, match.group(2)
        yield match.start(3), Text,   match.group(3)

        # lookup lexer if wanted and existing
        lexer = None
        if self.handlecodeblocks:
            try:
                lexer = get_lexer_by_name(match.group(2).strip())
            except ClassNotFound:
                pass
        code = match.group(4)

        # no lexer for this language. handle it like it was a code block
        if lexer is None:
            yield match.start(4), String, code
            return

        yield from do_insertions([], lexer.get_tokens_unprocessed(code))

        yield match.start(5), String, match.group(5)

    def _handle_cssblock(self, match):
        """
        match args: 1:style tag 2:newline, 3:code, 4:closing style tag
        """
        from pygments.lexers import get_lexer_by_name

        # section header
        yield match.start(1), String, match.group(1)
        yield match.start(2), String, match.group(2)

        lexer = None
        if self.handlecodeblocks:
            try:
                lexer = get_lexer_by_name('css')
            except ClassNotFound:
                pass
        code = match.group(3)

        # no lexer for this language. handle it like it was a code block
        if lexer is None:
            yield match.start(3), String, code
            return

        yield from do_insertions([], lexer.get_tokens_unprocessed(code))

        yield match.start(4), String, match.group(4)

    tokens = {
        'root': [
            # title in metadata section
            (r'^(title)(:\s)(.+\n)', bygroups(Keyword, Text, Generic.Heading)),
            # headings
            (r'^(!)([^!].+\n)', bygroups(Generic.Heading, Text)),
            (r'^(!{2,6})(.+\n)', bygroups(Generic.Subheading, Text)),
            # bulleted or numbered lists or single-line block quotes
            # (can be mixed)
            (r'^(\s*)([*#>]+)(\s*)(.+\n)',
             bygroups(Text, Keyword, Text, using(this, state='inline'))),
            # multi-line block quotes
            (r'^(<<<.*\n)([\w\W]*?)(^<<<.*$)', bygroups(String, Text, String)),
            # table header
            (r'^(\|.*?\|h)$', bygroups(Generic.Strong)),
            # table footer or caption
            (r'^(\|.*?\|[cf])$', bygroups(Generic.Emph)),
            # table class
            (r'^(\|.*?\|k)$', bygroups(Name.Tag)),
            # definitions
            (r'^(;.*)$', bygroups(Generic.Strong)),
            # text block
            (r'^(```\n)([\w\W]*?)(^```$)', bygroups(String, Text, String)),
            # code block with language
            (r'^(```)(\w+)(\n)([\w\W]*?)(^```$)', _handle_codeblock),
            # CSS style block
            (r'^(<style>)(\n)([\w\W]*?)(^</style>$)', _handle_cssblock),

            include('keywords'),
            include('inline'),
        ],
        'keywords': [
            (words((
                '\\define', '\\end', 'caption', 'created', 'modified', 'tags',
                'title', 'type'), prefix=r'^', suffix=r'\b'),
             Keyword),
        ],
        'inline': [
            # escape
            (r'\\.', Text),
            # created or modified date
            (r'\d{17}', Number.Integer),
            # italics
            (r'(\s)(//[^/]+//)((?=\W|\n))',
             bygroups(Text, Generic.Emph, Text)),
            # superscript
            (r'(\s)(\^\^[^\^]+\^\^)', bygroups(Text, Generic.Emph)),
            # subscript
            (r'(\s)(,,[^,]+,,)', bygroups(Text, Generic.Emph)),
            # underscore
            (r'(\s)(__[^_]+__)', bygroups(Text, Generic.Strong)),
            # bold
            (r"(\s)(''[^']+'')((?=\W|\n))",
             bygroups(Text, Generic.Strong, Text)),
            # strikethrough
            (r'(\s)(~~[^~]+~~)((?=\W|\n))',
             bygroups(Text, Generic.Deleted, Text)),
            # TiddlyWiki variables
            (r'<<[^>]+>>', Name.Tag),
            (r'\$\$[^$]+\$\$', Name.Tag),
            (r'\$\([^)]+\)\$', Name.Tag),
            # TiddlyWiki style or class
            (r'^@@.*$', Name.Tag),
            # HTML tags
            (r'</?[^>]+>', Name.Tag),
            # inline code
            (r'`[^`]+`', String.Backtick),
            # HTML escaped symbols
            (r'&\S*?;', String.Regex),
            # Wiki links
            (r'(\[{2})([^]\|]+)(\]{2})', bygroups(Text, Name.Tag, Text)),
            # External links
            (r'(\[{2})([^]\|]+)(\|)([^]\|]+)(\]{2})',
            bygroups(Text, Name.Tag, Text, Name.Attribute, Text)),
            # Transclusion
            (r'(\{{2})([^}]+)(\}{2})', bygroups(Text, Name.Tag, Text)),
            # URLs
            (r'(\b.?.?tps?://[^\s"]+)', bygroups(Name.Attribute)),

            # general text, must come last!
            (r'[\w]+', Text),
            (r'.', Text)
        ],
    }

    def __init__(self, **options):
        self.handlecodeblocks = get_bool_opt(options, 'handlecodeblocks', True)
        RegexLexer.__init__(self, **options)


class WikitextLexer(RegexLexer):
    """
    For MediaWiki Wikitext.

    Parsing Wikitext is tricky, and results vary between different MediaWiki installations,
    so we only highlight common syntaxes (built-in or from popular extensions),
    and also assume templates produce no unbalanced syntaxes.
    """
    name = 'Wikitext'
    url = 'https://www.mediawiki.org/wiki/Wikitext'
    aliases = ['wikitext', 'mediawiki']
    filenames = []
    mimetypes = ['text/x-wiki']
    flags = re.MULTILINE | re.IGNORECASE

    def text_rules(token):
        return [
            (r'\w+', token),
            (r'[\r\t\f\v ]+', token),
            (r'(?s).', token),
        ]

    def handle_syntaxhighlight(self, match):
        from pygments.lexers import get_lexer_by_name

        yield match.start(1), Punctuation, match.group(1)
        yield match.start(2), Name.Tag, match.group(2)
        yield match.start(3), Whitespace, match.group(3)
        yield from self.get_tokens_unprocessed(match.group(4), stack=['root', 'attr'])
        yield match.start(5), Punctuation, match.group(5)

        lexer = None
        code = match.group(6)
        lang_match = re.findall(r'\blang=("|\'|)(\w+)(\1)', match.group(4))

        if len(lang_match) >= 1:
            # Pick the last match in case of multiple matches
            lang = lang_match[-1][1]
            try:
                lexer = get_lexer_by_name(lang)
            except ClassNotFound:
                pass

        if lexer is None:
            yield match.start(6), Text, code
        else:
            yield from lexer.get_tokens_unprocessed(code)

        yield match.start(7), Punctuation, match.group(7)
        yield match.start(8), Name.Tag, match.group(8)
        yield match.start(9), Text, match.group(9)
        yield match.start(10), Punctuation, match.group(10)

    def handle_score(self, match):
        yield match.start(1), Punctuation, match.group(1)
        yield match.start(2), Name.Tag, match.group(2)
        yield match.start(3), Whitespace, match.group(3)
        yield from self.get_tokens_unprocessed(match.group(4), stack=['root', 'attr'])
        yield match.start(5), Punctuation, match.group(5)

        lang_match = re.findall(r'\blang=("|\'|)(\w+)(\1)', match.group(4))
        score = match.group(6)
        # Pick the last match in case of multiple matches
        lang = lang_match[-1][1] if len(lang_match) >= 1 else 'lilypond'

        if lang == 'lilypond':  # Case sensitive
            yield from LilyPondLexer().get_tokens_unprocessed(score)
        else:  # ABC
            # FIXME: Use ABC lexer in the future
            yield match.start(6), Text, score

        yield match.start(7), Punctuation, match.group(7)
        yield match.start(8), Name.Tag, match.group(8)
        yield match.start(9), Text, match.group(9)
        yield match.start(10), Punctuation, match.group(10)

    title_char = r' %!"$&\'()*, \-./0-9:; =?@A-Z\\\^ _`a-z~+\u0080-\uFFFF'
    nbsp_char = r'(?:\t|&nbsp;|&\#0*160;|&\#[Xx]0*[Aa]0;|[ \xA0\u1680\u2000-\u200A\u202F\u205F\u3000])'
    link_address = r'(?:[0-9.]+|\[[0-9a-f:.]+\]|[^\0- "<>\[\]\x7F\xA0\u1680\u2000-\u200A\u202F\u205F\u3000\uFFFD])'
    link_char_class = r'[^\0- "<>\[\]\x7F\xA0\u1680\u2000-\u200A\u202F\u205F\u3000\uFFFD]'
    double_slashes_i = {
        '__FORCETOC__', '__NOCONTENTCONVERT__', '__NOCC__', '__NOEDITSECTION__', '__NOGALLERY__',
        '__NOTITLECONVERT__', '__NOTC__', '__NOTOC__', '__TOC__'
    }
    double_slashes = {
        '__EXPECTUNUSEDCATEGORY__',  '__HIDDENCAT__', '__INDEX__',  '__NEWSECTIONLINK__',
        '__NOINDEX__',  '__NONEWSECTIONLINK__',  '__STATICREDIRECT__', '__NOGLOBAL__',
        '__DISAMBIG__', '__EXPECTED_UNCONNECTED_PAGE__',
    }
    protocols = {
        'bitcoin:', 'ftp://', 'ftps://', 'geo:', 'git://', 'gopher://', 'http://', 'https://',
        'irc://', 'ircs://', 'magnet:', 'mailto:', 'mms://', 'news:', 'nntp://', 'redis://',
        'sftp://', 'sip:', 'sips:', 'sms:', 'ssh://', 'svn://', 'tel:', 'telnet://', 'urn:',
        'worldwind://', 'xmpp:', '//'
    }
    non_relative_protocols = protocols - {'//'}
    html_tags = {
        'abbr', 'b', 'bdi', 'bdo', 'big', 'blockquote', 'br', 'caption', 'center', 'cite', 'code',
        'data', 'dd', 'del', 'dfn', 'div', 'dl', 'dt', 'em', 'font', 'h1', 'h2', 'h3', 'h4', 'h5',
        'h6', 'hr', 'i', 'ins', 'kbd', 'li', 'link', 'mark', 'meta', 'ol', 'p', 'q', 'rb', 'rp',
        'rt', 'rtc', 'ruby', 's', 'samp', 'small', 'span', 'strike', 'strong', 'sub', 'sup',
        'table', 'td', 'th', 'time', 'tr', 'tt', 'u', 'ul', 'var', 'wbr'
    }
    parser_tags = {
        'graph', 'charinsert', 'rss', 'chem', 'categorytree', 'nowiki', 'inputbox', 'math',
        'hiero', 'score', 'pre', 'ref', 'translate', 'imagemap', 'templatestyles', 'languages',
        'noinclude', 'mapframe', 'section', 'poem', 'syntaxhighlight', 'includeonly', 'tvar',
        'onlyinclude', 'templatedata', 'langconvert', 'timeline', 'dynamicpagelist', 'gallery',
        'maplink', 'ce', 'references'
    }
    variant_langs = {
        # ZhConverter.php
        'zh', 'zh-hans', 'zh-hant', 'zh-cn', 'zh-hk', 'zh-mo', 'zh-my', 'zh-sg', 'zh-tw',
        # UnConverter.php
        'uz', 'uz-latn', 'uz-cyrl',
        # TlyConverter.php
        'tly', 'tly-cyrl',
        # TgConverter.php
        'tg', 'tg-latn',
        # SrConverter.php
        'sr', 'sr-ec', 'sr-el',
        # ShiConverter.php
        'shi', 'shi-tfng', 'shi-latn',
        # ShConverter.php
        'sh-latn', 'sh-cyrl',
        # KuConverter.php
        'ku', 'ku-arab', 'ku-latn',
        # KkConverter.php
        'kk', 'kk-cyrl', 'kk-latn', 'kk-arab', 'kk-kz', 'kk-tr', 'kk-cn',
        # IuConverter.php
        'iu', 'ike-cans', 'ike-latn',
        # GanConverter.php
        'gan', 'gan-hans', 'gan-hant',
        # EnConverter.php
        'en', 'en-x-piglatin',
        # CrhConverter.php
        'crh', 'crh-cyrl', 'crh-latn',
        # BanConverter.php
        'ban', 'ban-bali', 'ban-x-dharma', 'ban-x-palmleaf', 'ban-x-pku',
    }
    magic_vars_i = {
        'ARTICLEPATH', 'PAGEID', 'SCRIPTPATH', 'SERVER', 'SERVERNAME', 'STYLEPATH'
    }
    magic_vars = {
        '!', '=', 'BASEPAGENAME', 'BASEPAGENAMEE', 'CASCADINGSOURCES', 'CONTENTLANGUAGE',
        'CONTENTLANG', 'CURRENTDAY', 'CURRENTDAY2', 'CURRENTDAYNAME', 'CURRENTDOW', 'CURRENTHOUR',
        'CURRENTMONTH', 'CURRENTMONTH2', 'CURRENTMONTH1', 'CURRENTMONTHABBREV', 'CURRENTMONTHNAME',
        'CURRENTMONTHNAMEGEN', 'CURRENTTIME', 'CURRENTTIMESTAMP', 'CURRENTVERSION', 'CURRENTWEEK',
        'CURRENTYEAR', 'DIRECTIONMARK', 'DIRMARK', 'FULLPAGENAME', 'FULLPAGENAMEE', 'LOCALDAY',
        'LOCALDAY2', 'LOCALDAYNAME', 'LOCALDOW', 'LOCALHOUR', 'LOCALMONTH', 'LOCALMONTH2',
        'LOCALMONTH1', 'LOCALMONTHABBREV', 'LOCALMONTHNAME', 'LOCALMONTHNAMEGEN', 'LOCALTIME',
        'LOCALTIMESTAMP', 'LOCALWEEK', 'LOCALYEAR', 'NAMESPACE', 'NAMESPACEE', 'NAMESPACENUMBER',
        'NUMBEROFACTIVEUSERS', 'NUMBEROFADMINS', 'NUMBEROFARTICLES', 'NUMBEROFEDITS',
        'NUMBEROFFILES', 'NUMBEROFPAGES', 'NUMBEROFUSERS', 'PAGELANGUAGE', 'PAGENAME', 'PAGENAMEE',
        'REVISIONDAY', 'REVISIONDAY2', 'REVISIONID', 'REVISIONMONTH', 'REVISIONMONTH1',
        'REVISIONSIZE', 'REVISIONTIMESTAMP', 'REVISIONUSER', 'REVISIONYEAR', 'ROOTPAGENAME',
        'ROOTPAGENAMEE', 'SITENAME', 'SUBJECTPAGENAME', 'ARTICLEPAGENAME', 'SUBJECTPAGENAMEE',
        'ARTICLEPAGENAMEE', 'SUBJECTSPACE', 'ARTICLESPACE', 'SUBJECTSPACEE', 'ARTICLESPACEE',
        'SUBPAGENAME', 'SUBPAGENAMEE', 'TALKPAGENAME', 'TALKPAGENAMEE', 'TALKSPACE', 'TALKSPACEE'
    }
    parser_functions_i = {
        'ANCHORENCODE', 'BIDI', 'CANONICALURL', 'CANONICALURLE', 'FILEPATH', 'FORMATNUM',
        'FULLURL', 'FULLURLE', 'GENDER', 'GRAMMAR', '\#LANGUAGE', 'LC', 'LCFIRST', 'LOCALURL',
        'LOCALURLE', 'NS', 'NSE', 'PADLEFT', 'PADRIGHT', 'PAGEID', 'PLURAL', 'UC', 'UCFIRST',
        'URLENCODE'
    }
    parser_functions = {
        'BASEPAGENAME', 'BASEPAGENAMEE', 'CASCADINGSOURCES', 'DEFAULTSORT', 'DEFAULTSORTKEY',
        'DEFAULTCATEGORYSORT', 'FULLPAGENAME', 'FULLPAGENAMEE', 'NAMESPACE', 'NAMESPACEE',
        'NAMESPACENUMBER', 'NUMBERINGROUP', 'NUMINGROUP', 'NUMBEROFACTIVEUSERS', 'NUMBEROFADMINS',
        'NUMBEROFARTICLES', 'NUMBEROFEDITS', 'NUMBEROFFILES', 'NUMBEROFPAGES', 'NUMBEROFUSERS',
        'PAGENAME', 'PAGENAMEE', 'PAGESINCATEGORY', 'PAGESINCAT', 'PAGESIZE', 'PROTECTIONEXPIRY',
        'PROTECTIONLEVEL', 'REVISIONDAY', 'REVISIONDAY2', 'REVISIONID', 'REVISIONMONTH',
        'REVISIONMONTH1', 'REVISIONTIMESTAMP', 'REVISIONUSER', 'REVISIONYEAR', 'ROOTPAGENAME',
        'ROOTPAGENAMEE', 'SUBJECTPAGENAME', 'ARTICLEPAGENAME', 'SUBJECTPAGENAMEE',
        'ARTICLEPAGENAMEE', 'SUBJECTSPACE', 'ARTICLESPACE', 'SUBJECTSPACEE', 'ARTICLESPACEE',
        'SUBPAGENAME', 'SUBPAGENAMEE', 'TALKPAGENAME', 'TALKPAGENAMEE', 'TALKSPACE', 'TALKSPACEE',
        'INT', 'DISPLAYTITLE', 'PAGESINNAMESPACE', 'PAGESINNS'
    }

    tokens = {
        'root': [
            # Redirects
            (r"""(?x)
                (\A\s*?)
                (\#REDIRECT:?) # may contain a colon
                (\s+)
                (\[\[)
                    ([{}]+?)
                    (?: (\#)([^#]*?) )?
                    (?: (\|)([^\n]*) )?
                (\]\])
                (\s*?$\n)
            """.format(title_char),
             bygroups(Whitespace, Keyword, Whitespace, Punctuation, Name.Tag, Punctuation, Name.Label,
                      Punctuation, Text, Punctuation, Whitespace)),
            # Subheadings
            (r'^(={2,6})([ \t]*)(.+?)([ \t]*)(\1)(\s*$\n)',
             bygroups(Generic.Subheading, Whitespace, Generic.Subheading, Whitespace, Generic.Subheading, Whitespace)),
            # Headings
            (r'^(=)([ \t]*)(.+?)([ \t]*)(=)(\s*$\n)',
             bygroups(Generic.Heading, Whitespace, Generic.Heading, Whitespace, Generic.Heading, Whitespace)),
            # Double-slashed magic words
            (words(double_slashes_i), Name.Function.Magic),
            (words(double_slashes, prefix='(?-i:', suffix=')'), Name.Function.Magic),
            # Raw URLs
            (r'\b(?:{}){}{}*'.format('|'.join(protocols),
             link_address, link_char_class), Name.Label),
            # Magic links
            (r"""(?x-i:
                \b(?:RFC|PMID) {nbsp_char}+ [0-9]+\b
                |
                \bISBN {nbsp_char}
                (?: 97[89] {nbsp_dash}? )?
                (?: [0-9] {nbsp_dash}? ){{9}}
                [0-9Xx]\b
            )""".format(nbsp_char=nbsp_char, nbsp_dash=f'(?:-|{nbsp_char})'), Name.Function.Magic),
            include('list'),
            include('inline'),
            include('text'),
        ],
        'list': [
            # Description lists
            (r'^;', Keyword, 'dt'),
            # Ordered lists, unordered lists and indents
            (r'^[#:*]+', Keyword),
            # Horizontal rules
            (r'^-{4,}', Keyword),
        ],
        'inline': [
            # Signatures
            (r'~{3,5}', Keyword),
            # Entities
            include('entity'),
            # Bold & italic
            (r"('')(''')(?!')", bygroups(Generic.Emph,
             Generic.Strong), 'inline-italic-bold'),
            (r"'''(?!')", Generic.Strong, 'inline-bold'),
            (r"''(?!')", Generic.Emph, 'inline-italic'),
            # Comments & parameters & templates
            include('replaceable'),
            # Media links
            (
                r"""(?x)
                (\[\[)
                    (File|Image) (:)
                    ([{}]*)
                    (?: (\#) ([{}]*?) )?
                """.format(title_char, f'{title_char}#%'),
                bygroups(Punctuation, Name.Namespace,  Punctuation,
                         Name.Tag, Punctuation, Name.Label),
                'medialink-inner'
            ),
            # Wikilinks
            (
                r"""(?x)
                (\[\[)(?!{}) # Should not contain URLs
                    (?: ([{}]*) (:))?
                    ([{}]*?)
                    (?: (\#) ([{}]*?) )?
                (\]\])
                """.format('|'.join(protocols), title_char.replace('/', ''),
                           title_char, f'{title_char}#%'),
                bygroups(Punctuation, Name.Namespace,  Punctuation,
                         Name.Tag, Punctuation, Name.Label, Punctuation)
            ),
            (
                r"""(?x)
                (\[\[)(?!{}) 
                    (?: ([{}]*) (:))?
                    ([{}]*?)
                    (?: (\#) ([{}]*?) )?
                    (\|)
                """.format('|'.join(protocols), title_char.replace('/', ''),
                           title_char, f'{title_char}#%'),
                bygroups(Punctuation, Name.Namespace,  Punctuation,
                         Name.Tag, Punctuation, Name.Label, Punctuation),
                'wikilink-inner'
            ),
            # External links
            (
                r"""(?x)
                (\[)
                    ((?:{}) {} {}*)
                    (\s*)
                """.format('|'.join(protocols), link_address, link_char_class),
                bygroups(Punctuation, Name.Label, Whitespace),
                'extlink-inner'
            ),
            # Tables
            (r'^(:*)(\s*?)(\{\|)([^\n]*)$', bygroups(Keyword,
             Whitespace, Punctuation, using(this, state=['root', 'attr'])), 'table'),
            # <pre> & <nowiki>
            (
                r"""(?xs)
                (<)(pre|nowiki)\b(\s*?)((?:[^>]|-->)*?)(?<!--)(?<!/)(>)
                (.*?)
                (</)(\2)\b([^>]*?)(>)
                """,
                bygroups(
                    Punctuation, Name.Tag, Whitespace, using(
                        this, state=['root', 'attr']), Punctuation,
                    using(this, state=['root', 'nowiki-ish']
                          ), Punctuation, Name.Tag, Text, Punctuation
                ),
            ),
            # <math> & <chem> & <ce>
            (
                r"""(?xs)
                (<)(math|chem|ce)\b(\s*?)((?:[^>]|-->)*?)(?<!--)(?<!/)(>)
                (.*?)
                (</)(\2)\b([^>]*?)(>)
                """,
                bygroups(
                    Punctuation, Name.Tag, Whitespace, using(
                        this, state=['root', 'attr']), Punctuation,
                    using(TexLexer), Punctuation, Name.Tag, Text, Punctuation
                ),
            ),
            # <templatedata>
            (
                r"""(?xs)
                (<)(templatedata)\b(\s*?)((?:[^>]|-->)*?)(?<!--)(?<!/)(>)
                (.*?)
                (</)(\2)\b([^>]*?)(>)
                """,
                bygroups(
                    Punctuation, Name.Tag, Whitespace, using(
                        this, state=['root', 'attr']), Punctuation,
                    using(JsonLexer), Punctuation, Name.Tag, Text, Punctuation
                ),
            ),
            # Tags with plaintext content
            (
                r"""(?xs)
                (<)(categorytree|charinsert|hiero|rss|gallery|graph
                |dynamicpagelist|imagemap|inputbox)\b(\s*?)((?:[^>]|-->)*?)(?<!--)(?<!/)(>)
                (.*?)
                (</)(\2)\b([^>]*?)(>)
                """,
                bygroups(
                    Punctuation, Name.Tag, Whitespace, using(
                        this, state=['root', 'attr']), Punctuation,
                    Text, Punctuation, Name.Tag, Text, Punctuation
                ),
            ),
            # <syntaxhighlight> & <source>
            (
                r"""(?xs)
                (<)(syntaxhighlight|source)\b(\s*?)((?:[^>]|-->)*?)(?<!--)(?<!/)(>)
                (.*?)
                (</)(\2)\b([^>]*?)(>)
                """,
                handle_syntaxhighlight,
            ),
            # <score>
            (
                r"""(?xs)
                (<)(score)\b(\s*?)((?:[^>]|-->)*?)(?<!--)(?<!/)(>)
                (.*?)
                (</)(\2)\b([^>]*?)(>)
                """,
                handle_score,
            ),
            # Tags
            (
                r'(</?)({})\b(\s*?)((?:[^>]|-->)*?)(/?\s*?(?<!--)>)'.format(
                    '|'.join(html_tags | parser_tags)),
                bygroups(Punctuation, Name.Tag,
                         Whitespace, using(this, state=['root', 'attr']), Punctuation),
            ),
            # LanguageConverter markups
            (
                r"""(?x)
                (-\{{) # Escape format()
                    (?: ([^|]) (\|))?
                    (?: (\s* (?:{variants}) \s*) (=>))?
                    (\s* (?:{variants}) \s*) (:)
                """.format(variants='|'.join(variant_langs)),
                bygroups(Punctuation, Keyword, Punctuation,
                         Name.Label, Operator, Name.Label, Punctuation),
                'lc-inner'
            ),
            (r'-\{', Punctuation, 'lc-raw'),
        ],
        'wikilink-inner': [
            # Quit in case of another wikilink
            (r'(?=\[\[)', Punctuation, '#pop'),
            (r'\]\]', Punctuation, '#pop'),
            include('inline'),
            include('text'),
        ],
        'medialink-inner': [
            (r'\]\]', Punctuation, '#pop'),
            (r'(\|)([^\n=|]*)(=)',
             bygroups(Punctuation, Name.Attribute, Operator)),
            (r'\|', Punctuation),
            include('inline'),
            include('text'),
        ],
        'quote-common': [
            # Quit in case of link/template endings
            (r'(?=\]\]|\{\{|\}\})', Punctuation, '#pop'),
            (r'\n', Text, '#pop'),
        ],
        'inline-italic': [
            include('quote-common'),
            (r"('')(''')(?!')", bygroups(Generic.Emph,
             Generic.Strong), ('#pop', 'inline-bold')),
            (r"'''(?!')", Generic.Strong, ('#pop', 'inline-italic-bold')),
            (r"''(?!')", Generic.Emph, '#pop'),
            include('inline'),
            include('text-italic'),
        ],
        'inline-bold': [
            include('quote-common'),
            (r"(''')('')(?!')", bygroups(
                Generic.Strong, Generic.Emph), ('#pop', 'inline-italic')),
            (r"'''(?!')", Generic.Strong, '#pop'),
            (r"''(?!')", Generic.Emph, ('#pop', 'inline-bold-italic')),
            include('inline'),
            include('text-bold'),
        ],
        'inline-bold-italic': [
            include('quote-common'),
            (r"('')(''')(?!')", bygroups(Generic.Emph,
             Generic.Strong), '#pop'),
            (r"'''(?!')", Generic.Strong, ('#pop', 'inline-italic')),
            (r"''(?!')", Generic.Emph, ('#pop', 'inline-bold')),
            include('inline'),
            include('text-italic'),
        ],
        'inline-italic-bold': [
            include('quote-common'),
            (r"(''')('')(?!')", bygroups(
                Generic.Strong, Generic.Emph), '#pop'),
            (r"'''(?!')", Generic.Strong, ('#pop', 'inline-italic')),
            (r"''(?!')", Generic.Emph, ('#pop', 'inline-bold')),
            include('text-bold'),
        ],
        'lc-inner': [
            (
                r"""(?x)
                (;)
                (?: (\s* (?:{variants}) \s*) (=>))?
                (\s* (?:{variants}) \s*) (:)
                """.format(variants='|'.join(variant_langs)),
                bygroups(Punctuation, Name.Label,
                         Operator, Name.Label, Punctuation)
            ),
            (r';?\s*?\}-', Punctuation, '#pop'),
            include('inline'),
            include('text'),
        ],
        'lc-raw': [
            (r'\}-', Punctuation, '#pop'),
            include('inline'),
            include('text'),
        ],
        'replaceable': [
            # Comments
            (r'(?s)<!--.*?(?:-->|\Z)', Comment.Multiline),
            # Parameters
            (
                r"""(?sx)
                (\{{3})
                    ([^|]*?)
                    (?=\}{3}|\|)
                """,
                bygroups(Punctuation, Name.Variable),
                'parameter-inner',
            ),
            # Magic variables
            (r'(\{\{)(\s*)(%s|(?-i:%s))(\s*)(\}\})' %
             ('|'.join(magic_vars_i), '|'.join(magic_vars)),
             bygroups(Punctuation, Whitespace, Name.Function, Whitespace, Punctuation)),
            # Parser functions
            (
                r"""(?xs)
                (\{\{)
                (\s* <!--.*?(?:-->|\Z))?
                (\s*) ( \# [%s]*? | %s | (?-i: %s ) ) (:) 
                """ % (title_char,  '|'.join(parser_functions_i), '|'.join(parser_functions)),
                bygroups(Punctuation, Comment.Multiline, Whitespace, Name.Function,
                         Punctuation), 'template-inner'
            ),
            # Templates
            (
                r"""(?xs)
                (\{\{)
                (\s* <!--.*?(?:-->|\Z))?
                (\s*) (?: ([%s]*?) (:) )?
                """ % title_char,
                bygroups(Punctuation, Comment.Multiline, Whitespace, Name.Namespace,
                         Punctuation), 'template-name'
            ),
        ],
        'parameter-inner': [
            (r'\}{3}', Punctuation, '#pop'),
            ('\|', Punctuation),
            include('inline'),
            include('text'),
        ],
        'template-name': [
            (r'(\s*?)(\|)', bygroups(Text, Punctuation), ('#pop', 'template-inner')),
            (r'\}\}', Punctuation, '#pop'),
            (r'\n', Text, '#pop'),
            include('replaceable'),
            *text_rules(Name.Tag),
        ],
        'template-inner': [
            (r'\}\}', Punctuation, '#pop'),
            ('\|', Punctuation),
            (
                r"""(?x)
                    (?<=\|)
                    ( (?: (?! \{\{ | \}\} )[^=\|<])*? ) # Exclude templates and tags
                    (=)
                """,
                bygroups(Name.Label, Operator)
            ),
            include('inline'),
            include('text'),
        ],
        'table': [
            # Endings
            (r'^([ \t\n\r\0\x0B]*?)(\|\})',
             bygroups(Whitespace, Punctuation), '#pop'),
            # Table rows
            (r'^([ \t\n\r\0\x0B]*?)(\|-+)(.*)$', bygroups(Whitespace, Punctuation,
             using(this, state=['root', 'attr']))),
            # Captions
            (
                r"""(?x)
                ^([ \t\n\r\0\x0B]*?)(\|\+)
                # Exclude links, template and tags
                (?: ( (?: (?! \[\[ | \{\{ )[^|\n<] )*? )(\|) )? 
                (.*?)$
                """,
                bygroups(Whitespace, Punctuation, using(this, state=[
                         'root', 'attr']), Punctuation, Generic.Heading),
            ),
            # Table data
            (
                r"""(?x)
                ( ^(?:[ \t\n\r\0\x0B]*?)\| | \|\| )
                (?: ( (?: (?! \[\[ | \{\{ )[^|\n<] )*? )(\|)(?!\|) )?
                """,
                bygroups(Punctuation, using(this, state=[
                         'root', 'attr']), Punctuation),
            ),
            # Table headers
            (
                r"""(?x)
                ( ^(?:[ \t\n\r\0\x0B]*?)!  )
                (?: ( (?: (?! \[\[ | \{\{ )[^|\n<] )*? )(\|)(?!\|) )?
                """,
                bygroups(Punctuation, using(this, state=[
                         'root', 'attr']), Punctuation),
                'table-header',
            ),
            include('list'),
            include('inline'),
            include('text'),
        ],
        'table-header': [
            # Requires another state for || handling inside headers
            (r'\n', Text, '#pop'),
            (
                r"""(?x)
                (!!|\|\|)
                (?: 
                    ( (?: (?! \[\[ | \{\{ )[^|\n<] )*? )
                    (\|)(?!\|)
                )?
                """,
                bygroups(Punctuation, using(this, state=[
                         'root', 'attr']), Punctuation)
            ),
            *text_rules(Generic.Subheading),
        ],
        'entity': [
            (r'&\S*?;', Name.Entity),
        ],
        'dt': [
            (r'\n', Text, '#pop'),
            include('inline'),
            (r':', Keyword, '#pop'),
            include('text'),
        ],
        'extlink-inner': [
            (r'\]', Punctuation, '#pop'),
            include('inline'),
            include('text'),
        ],
        'nowiki-ish': [
            include('entity'),
            include('text'),
        ],
        'attr': [
            include('replaceable'),
            (r'\s+', Whitespace),
            (r'(=)(\s*)(")', bygroups(Operator, Whitespace, String.Double), 'attr-val-2'),
            (r"(=)(\s*)(')", bygroups(Operator, Whitespace, String.Single), 'attr-val-1'),
            (r'(=)(\s*)', bygroups(Operator, Whitespace), 'attr-val-0'),
            (r'[\w:-]+', Name.Attribute),

        ],
        'attr-val-0': [
            (r'\s', Whitespace, '#pop'),
            include('replaceable'),
            *text_rules(String),
        ],
        'attr-val-1': [
            (r"'", String.Single, '#pop'),
            include('replaceable'),
            *text_rules(String.Single),
        ],
        'attr-val-2': [
            (r'"', String.Double, '#pop'),
            include('replaceable'),
            *text_rules(String.Double),
        ],
        'text-italic': text_rules(Generic.Emph),
        'text-bold': text_rules(Generic.Strong),
        'text': text_rules(Text),
    }
