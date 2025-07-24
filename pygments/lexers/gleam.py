"""
    pygments.lexers.gleam
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Gleam programming language.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words, bygroups
from pygments.token import Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace

__all__ = ['GleamLexer']


class GleamLexer(RegexLexer):
    """
    Lexer for the Gleam programming language (version >= 1.0.0).
    """

    name = 'Gleam'
    url = 'https://gleam.run/'
    filenames = ['*.gleam']
    aliases = ['gleam']
    mimetypes = ['text/x-gleam']
    version_added = '2.19'

    _keywords = words((
        'as', 'assert', 'auto', 'case', 'const', 'delegate', 'derive', 'echo',
        'else', 'fn', 'if', 'implement', 'import', 'let', 'macro', 'opaque',
        'panic', 'pub', 'test', 'todo', 'type', 'use',
    ), suffix=r'\b')

    tokens = {
        'root': [
            # Comments
            (r'(///.*?)(\n)', bygroups(String.Doc, Whitespace)),
            (r'(//.*?)(\n)', bygroups(Comment.Single, Whitespace)),

            # Multiline Strings
            (r'"""(.|\n)*?"""', String),

            # Single-line Strings
            (r'"(\\"|[^"])*"', String),

            # Bit arrays
            (r'<<.*?>>', String.Other),

            # Attributes / annotations
            (r'@\w+', Name.Decorator),

            # Keywords
            (_keywords, Keyword),

            # pub fn with function name
            (r'\b(pub)(\s+)(fn)(\s+)([a-z_][a-zA-Z0-9_]*)',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Name.Function)),

            # let binding
            (r'\b(let)(\s+)(_[a-z][a-z0-9_]*|\w+)',
             bygroups(Keyword, Whitespace, Name.Variable)),

            # fn name (non-pub)
            (r'\b(fn)(\s+)([a-z_][a-zA-Z0-9_]*)',
             bygroups(Keyword, Whitespace, Name.Function)),

            # Type constructors (PascalCase)
            (r'\b[A-Z][a-zA-Z0-9_]*\b', Name.Class),

            # Discard variables (_foo)
            (r'\b_[a-z][a-z0-9_]*\b', Name.Other),

            # General identifiers
            (r'[a-zA-Z_/][a-zA-Z0-9_]*', Name),

            # Operators
            (r'(<>|\+\.?|\-\.?|\*\.?|/\.?|%\.?|<=\.?|>=\.?|<\.?|>\.?|=)', Operator),

            # Punctuation (groupings and flow)
            (r'[()\[\]{}:;,@]+', Punctuation),
            (r'(#|!=|!|==|\|>|\|\||\||->|<-|&&|<<|>>|\.\.|\.|=)', Punctuation),

            # Numbers
            (r'(\d+(_\d+)*\.(?!\.)(\d+(_\d+)*)?|\.\d+(_\d+)*)([eEf][+-]?[0-9]+)?', Number.Float),
            (r'\d+(_\d+)*[eEf][+-]?[0-9]+', Number.Float),
            (r'0[xX][a-fA-F0-9]+(_[a-fA-F0-9]+)*(\.([a-fA-F0-9]+(_[a-fA-F0-9]+)*)?)?p[+-]?\d+', Number.Float),
            (r'0[bB][01]+(_[01]+)*', Number.Bin),
            (r'0[oO][0-7]+(_[0-7]+)*', Number.Oct),
            (r'0[xX][a-fA-F0-9]+(_[a-fA-F0-9]+)*', Number.Hex),
            (r'\d+(_\d+)*', Number.Integer),

            # Whitespace
            (r'\s+', Whitespace),
        ],
    }

