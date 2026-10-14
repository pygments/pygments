"""
    pygments.lexers.sdl
    ~~~~~~~~~~~~~~~~~~~

    Lexer for SDL (Simple Declarative Language).

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import Comment, Operator, Keyword, Name, Number, String, \
    Literal, Punctuation, Text, Whitespace

__all__ = ['SDLLexer']


class SDLLexer(RegexLexer):
    """
    Lexer for SDL, a data language used for configuration files and
    serialization.

    .. versionadded:: 2.22
    """
    name = 'SDL'
    url = 'https://sdlang.org'
    aliases = ['sdl']
    filenames = ['*.sdl']
    version_added = '2.22'

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'\\\n', Text),  # line continuation
            # Comments: block, and line comments in three flavours
            (r'/\*.*?\*/', Comment.Multiline),
            (r'(//|--|#).*?$', Comment.Single),
            # Binary data: [base64], possibly spanning several lines
            (r'\[[a-zA-Z0-9 /+=]*\]', String.Other),
            # Date-times: 2016/09/03 12:34:56.789-UTC
            (r'\d{4}/\d{2}/\d{2}(?:[ \t]+\d{2}:\d{2}:\d{2}'
             r'(?:\.\d{3})?(?:-UTC)?)?', Literal.Date),
            # Durations: 5d:12:34:56.789
            (r'(?:\d+d:)?\d{2}:\d{2}:\d{2}(?:\.\d{3})?', Literal.Date),
            # Decimals, with an optional `f` suffix
            (r'-?\d+\.\d+f?\b', Number.Float),
            # Integers, with optional `L` (long) or `BD` (big decimal)
            # suffixes
            (r'-?\d+(?:L|BD)?\b', Number.Integer),
            # Booleans and null
            (words(('true', 'false', 'on', 'off', 'null'), suffix=r'\b'),
             Keyword.Constant),
            # Strings, double-quoted (with escapes) or raw (backquoted)
            (r'"', String.Double, 'string'),
            (r'`[^`]*`', String.Backtick),
            # Namespaces and attributes are identifiers distinguished by
            # the character that follows them
            (r'([a-zA-Z_][\w.$-]*)(:)', bygroups(Name.Namespace, Punctuation)),
            (r'[a-zA-Z_][\w.$-]*(?==)', Name.Attribute),
            (r'=', Operator),
            # Tag names
            (r'[a-zA-Z_][\w.$-]*', Name.Tag),
            # Block and statement punctuation
            (r'[{};]', Punctuation),
        ],
        'string': [
            (r'\\.', String.Escape),
            (r'"', String.Double, '#pop'),
            (r'[^"\\]+', String.Double),
        ],
    }
