"""
    Lexer for AsciiDoc
    ~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups
from pygments.token import Comment, Generic, Keyword, Name, Punctuation, String, Text

__all__ = ['AsciiDocLexer']


class AsciiDocLexer(RegexLexer):
    """
    Lexer for AsciiDoc documents.

    AsciiDoc is a lightweight markup language.  A document consists of a
    document header (title, author and revision information, attribute
    entries) followed by sections whose titles are marked with ``=``
    characters.  The body mixes paragraphs with delimited blocks, lists,
    labelled list items, admonitions and inline formatting.
    """

    name = 'AsciiDoc'
    aliases = ['asciidoc', 'adoc']
    filenames = ['*.adoc', '*.asciidoc', '*.asc', '*.ad']
    mimetypes = []
    url = 'https://asciidoctor.org/'
    version_added = '2.22'

    tokens = {
        'root': [
            # section titles
            (r'^(={2,6})([ \t]+)(.+)$',
             bygroups(Generic.Subheading, Text, Generic.Subheading)),
            # the document title is followed by the document header
            (r'^(=)([ \t]+)(.+)(\n)',
             bygroups(Generic.Heading, Text, Generic.Heading, Text), 'header'),
            # attribute entries
            (r'^(:)([^:\n]+)(:)([^\n]*)',
             bygroups(Punctuation, Name.Attribute, Punctuation, String)),
            # block attribute lines, e.g. ``[source,python]`` or ``[NOTE]``
            (r'^(\[)([^\]\n]*)(\])', bygroups(Punctuation, Keyword, Punctuation)),
            # delimited blocks
            (r'^(-{4,})[ \t]*$', Punctuation, 'listing'),
            (r'^(={4,})[ \t]*$', Punctuation, 'example'),
            (r'^(\*{4,})[ \t]*$', Punctuation, 'sidebar'),
            (r'^(_+)[ \t]*$', Punctuation, 'quote'),
            (r'^(\.{4,})[ \t]*$', Punctuation, 'literal'),
            (r'^(\+{4,})[ \t]*$', Punctuation, 'passthrough'),
            (r'^////[ \t]*$', Comment.Multiline, 'comment'),
            # list items
            (r'^([ \t]*)(\*+|\.{1,3}|-+)([ \t])', bygroups(Text, Keyword, Text)),
            (r'\[[ xX]\]', Keyword),
            # labelled list items
            (r'^([^\n:][^:\n]*?)(::)([ \t]*)', bygroups(Name.Tag, Punctuation, Text)),
            # admonitions
            (r'^(NOTE|TIP|IMPORTANT|WARNING|CAUTION)(:)',
             bygroups(Keyword, Punctuation)),
            # images
            (r'^(image::)([^[\]\n]+)(\[)([^\]\n]*)(\])',
             bygroups(Punctuation, Name.Tag, Punctuation, String, Punctuation)),
            # links
            (r'(link:)([^[\]\n]+)(\[)([^\]\n]*)(\])',
             bygroups(Punctuation, Name.Tag, Punctuation, String, Punctuation)),
            (r'(https?://[^\s\[\]<>]+)', Name.Tag),
            # inline formatting; the marks have to sit on a word boundary
            (r'(?<![A-Za-z0-9*])(\*\*[^*\ \n][^*\n]*\*\*)(?![A-Za-z0-9*])',
             Generic.Strong),
            (r'(?<![A-Za-z0-9*])(\*[^*\ \n][^*\n]*\*)(?![A-Za-z0-9*])',
             Generic.Strong),
            (r'(?<![A-Za-z0-9_])(_[^_ \n][^_\n]*_)(?![A-Za-z0-9_])',
             Generic.Emph),
            (r'(?<![A-Za-z0-9`])(`[^`\n]+`)(?![A-Za-z0-9`])', String.Backtick),
            (r'(?<![A-Za-z0-9#])(#[^#\n]+#)(?![A-Za-z0-9#])', Generic.EmphStrong),
            # attribute references
            (r'(\{[A-Za-z0-9_][A-Za-z0-9_-]*\})', Name.Variable),
            # comments
            (r'//[^\n]*', Comment.Single),
            # plain text
            (r'[^\\\s]+', Text),
            (r'.', Text),
        ],
        'header': [
            # author line
            (r'^([^\n<]*)(<[^>\n]+>)([ \t]*)(\n)',
             bygroups(Name.Attribute, String, Text, Text)),
            # revision line
            (r'^(v?[0-9][^\n:]*)(:)([^\n]*)(\n)',
             bygroups(Generic.Subheading, Punctuation, String, Text)),
            # attribute entries
            (r'^(:)([^:\n]+)(:)([^\n]*)(\n)',
             bygroups(Punctuation, Name.Attribute, Punctuation, String, Text)),
            # anything else ends the document header
            (r'[^\n]', Text, '#pop'),
        ],
        'listing': [
            (r'^(-{4,})[ \t]*$', Punctuation, '#pop'),
            (r'[^\n]+', String),
            (r'\n', String),
        ],
        'example': [
            (r'^(={4,})[ \t]*$', Punctuation, '#pop'),
            (r'[^\n]+', Text),
            (r'\n', Text),
        ],
        'sidebar': [
            (r'^(\*{4,})[ \t]*$', Punctuation, '#pop'),
            (r'[^\n]+', Text),
            (r'\n', Text),
        ],
        'quote': [
            (r'^(_+)[ \t]*$', Punctuation, '#pop'),
            (r'[^\n]+', Text),
            (r'\n', Text),
        ],
        'literal': [
            (r'^(\.{4,})[ \t]*$', Punctuation, '#pop'),
            (r'[^\n]+', String),
            (r'\n', String),
        ],
        'passthrough': [
            (r'^(\+{4,})[ \t]*$', Punctuation, '#pop'),
            (r'[^\n]+', String),
            (r'\n', String),
        ],
        'comment': [
            (r'^////[ \t]*$', Comment.Multiline, '#pop'),
            (r'[^\n]+', Comment.Multiline),
            (r'\n', Comment.Multiline),
        ],
    }

    def analyse_text(text):
        """Recognise an AsciiDoc document by its title and block syntax."""
        if not re.search(r'(?m)^=[ \t]+\S', text):
            return 0.0
        if re.search(r'(?m)^(:[\w-]+:|==+[ \t]|----[ \t]*$|\[[\w,]+])', text):
            return 1.0
        return 0.5
