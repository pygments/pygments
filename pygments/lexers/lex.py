"""
    pygments.lexers.lex
    ~~~~~~~~~~~~~~~~~~~

    Lexer for Lex and Flex scanners.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, DelegatingLexer, include, bygroups, \
    default, words, inherit
from pygments.token import Keyword, Punctuation, Comment, Other, Name, \
    Whitespace, String, Operator, Text, Number
from pygments.lexers.c_cpp import CLexer, CppLexer, CFamilyLexer

__all__ = ['LexCLexer', 'LexCppLexer']

class LexLexer(RegexLexer):
    """
    Generic Lex Lexer.
    Should not be called directly; use LexCLexer or LexCppLexer.
    """

    blank = r'[ f\r\t\v]' # non-newline whitespace
    escape = r'\\(.|x[a-fA-F0-9]{2,4}|u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8}|[0-7]{1,3})'

    tokens = {
        # Mostly lifted from CFamilyLexer, but with Other instead of String,
        # because a string may actually be a Comment.PreprocFile
        'cstring': [
            (r'"', Other, '#pop'),
            (escape, Other),
            (r'[^\\"\n]+', Other),  # all other characters
            (r'\\\n', Other),  # line continuation
            (r'\\', Other),  # stray backslash
        ],
        'names': [
            (r'\n+', Whitespace, '#pop'),
            (r'[A-Za-z_]\w*', Name),
            (r'\s+', Whitespace)
        ],
        'comments': [
            (r'(?s)/\*.*?\*/', Comment.Multiline),
            (r'//.*', Comment.Single)
        ],
        'skips': [
            # Tokens that the lexer should skip when searching for the end
            # of a C block -- so comments and strings
            include('comments'),
            (r'([LuU]|u8)?"', Other, 'cstring'),
            (r"([LuU]|u8)?'(" + escape + r"|[^\\\'\n])'", Other),
        ],

        'embeddedC': [
            (r'\{', Other, '#push'),
            (r'\}', Other, '#pop'),
            include('skips'),
            (r'[^{}/\'"]+|/', Other)
        ],
        'POSIXembeddedC': [
            (r'%\}', Punctuation, '#pop'),
            include('skips'),
            (r'[^}%/\'"]+|[%}/]', Other)
        ],

        'string': CFamilyLexer.tokens['string'],
        'charclass': [
            (r'\[', String.Regex, '#push'),
            (r'\]', String.Regex, '#pop'),
            (escape, String.Escape),
            (r'[^][\\]+', String.Regex)
        ],
        'pattern': [
            # Regexes to match regexes are not pretty.  This one tries to
            # honour the rule that if a square bracket is the first character
            # of a class, after an optional caret, it doesn't close the class
            (r'\[\^?[][]{,2}', String.Regex, 'charclass'),
            (r'"', String, 'string'),
            (escape, String.Escape),
            (r'(\{)([A-Za-z_]\w*)(\})', bygroups(Punctuation, Name, Punctuation)),
            (r'\{[-+]\}', Operator), # Flex extension: charclass operators
            (r'[^[\\\s{"]+|\{', String.Regex),
            # This amounts to breaking on whitespace without consuming it.
            # Could maybe use a lookahead assertion, but seems unnecessary
            default('#pop')
        ],

        'expectEmbeddedC': [
            (r'%\{', Punctuation, 'POSIXembeddedC'),
            (r'(\s*' + blank + r'+)(.*)', bygroups(Whitespace, Other))
        ],
        'percent': [
            (r'([sx])(' + blank + r'+)', bygroups(Keyword, Whitespace),
             ('#pop', 'names')),
            (r'(top)(\s*)(\{)', bygroups(Keyword, Whitespace, Other),
             ('#pop', 'embeddedC')),
            (r'(option)(\s+)(.+)', bygroups(Keyword, Whitespace, Text), '#pop'),
            (words(('array', 'pointer'), suffix=r'\b\s*'), Keyword, '#pop'),
            (r'([pnaeko])(' + blank + r'+)(\d+)',
             bygroups(Keyword, Whitespace, Number), '#pop'),
            # Don't let an unrecognised %directive spoil the whole scan
            (r'(.+)(\n*)', bygroups(Text, Whitespace), '#pop')
        ],
        'definitions': [
            (r'^([A-Za-z_]\w*)(\s+)(.+)$',
             bygroups(Name, Whitespace, String.Regex)),
            include('expectEmbeddedC'),
            include('comments'),
            (r'\n+', Whitespace),
            (r'(%%)(\n*)', bygroups(Keyword, Whitespace), '#pop'),
            (r'%', Punctuation, 'percent')
        ],

        'endrule': [
            # trailing comments after a rule but before the newline are
            # allowed
            include('comments'),
            (blank + r'+', Whitespace),
            (r'\n+', Whitespace, '#pop')
        ],
        'CLine': [
            # Parse a single line of C, but skip newlines if we're in a
            # block or a string -- so the shortest well-formed-ish
            # newline-terminated C expression.
            include('skips'),
            (r'[^{/\'"\n]+|/', Other),
            (r'\{', Other, 'embeddedC'),
            (r'\n+', Other, '#pop')
        ],
        'inrule': [
            (blank + r'+', Whitespace),
            (r'\|', Punctuation, ('#pop', 'endrule')),
            (r'\{', Other, ('#pop', 'endrule', 'embeddedC')),
            # No need for endrule here, comments and trailing newlines are
            # already handled by CLine
            default(('#pop', 'CLine'))
        ],
        'starts': [
            (r'\bINITIAL\b', Name.Builtin),
            (r'[A-Za-z_]\w*', Name),
            (r',', Operator),
            (r'\*', Keyword),
            (r'(>)(\s*)', bygroups(Punctuation, Whitespace),
             ('#pop', 'maybeStartScope'))
        ],
        'ruleBase': [
            (r'\n+', Whitespace),
            # Despite its appearance, <<EOF>> is a pattern, not a start condition
            (r'(<<EOF>>)(\s*)', bygroups(Keyword, Whitespace), 'inrule'),
            (r'<', Punctuation, 'starts'),
            default(('inrule', 'pattern'))
        ],
        'rules': [
            # Comments when Lex is expecting a pattern are *not* allowed
            (r'%%', Keyword, '#pop'),
            # Horizontal whitespace is still expected to begin embedded C,
            # which is allowed (but not recommended) anywhere between rules
            include('expectEmbeddedC'),
            include('ruleBase')
        ],

        # This is a flex extension: a start condition list followed by an
        # open brace (with no pattern in between) begins a start condition
        # `scope', which may be nested
        'maybeStartScope': [
            (r'(\{)(\s+)', bygroups(Punctuation, Whitespace), ('#pop', 'startScope')),
            # Is this ^ really how this works?
            default('#pop')
        ],
        'startScope': [
            # I think leading horizontal whitespace *is* allowed here,
            # unlike in the main rules section.  Still no comments
            (r'\s+', Whitespace),
            (r'(\})(\n*)', bygroups(Punctuation, Whitespace), '#pop'),
            include('ruleBase')
        ],

        'root': [
            # Just defer everything to the C lexer
            (r'(?s).+', Other)
        ]
    }

    # Set a default stack
    def get_tokens_unprocessed(self, text, stack=('root', 'rules', 'definitions')):
        yield from RegexLexer.get_tokens_unprocessed(self, text, stack)


KeywordOverrides = [
    (words(('lex', 'leng', 'text', 'more', 'less', 'wrap', 'in', 'out', 'lineno'),
           prefix='yy', suffix=r'\b'), Name.Builtin),
    (words(('STATE', '_START'), prefix='YY', suffix=r'\b'), Name.Builtin),
    (words(('BEGIN', 'INITIAL', 'REJECT', 'ECHO'), suffix=r'\b'), Name.Builtin),
    (r'(yy)?[iu]nput\b', Name.Builtin),
    inherit
]

class LexDelegateCLexer(CLexer):
    tokens = { 'keywords': KeywordOverrides }
class LexDelegateCppLexer(CppLexer):
    tokens = { 'keywords': KeywordOverrides }

class LexCLexer(DelegatingLexer):
    name = 'Lex'
    aliases = ['lex', 'flex']
    filenames = ['*.l', '*.lex']
    url = 'https://pubs.opengroup.org/onlinepubs/9799919799/utilities/lex.html'
    version_added = '2.20'
    def __init__(self, **options):
        super().__init__(LexDelegateCLexer, LexLexer, **options)

class LexCppLexer(DelegatingLexer):
    name = 'Lex with C++'
    aliases = ['lex++', 'flex++']
    filenames = ['*.ll', '*.lpp', '*.lxx', '*.L']
    url = 'https://github.com/westes/flex' # Flex provides explicit C++ support
    version_added = '2.20'
    def __init__(self, **options):
        super().__init__(LexDelegateCppLexer, LexLexer, **options)
