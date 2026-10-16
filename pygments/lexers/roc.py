"""
    pygments.lexers.roc
    ~~~~~~~~~~~~~~~~~~~

    Lexer for the Roc programming language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words, include
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Whitespace

__all__ = ['RocLexer']


class RocLexer(RegexLexer):
    """
    For Roc source code.
    """

    name = 'Roc'
    url = 'https://www.roc-lang.org/'
    aliases = ['roc']
    filenames = ['*.roc']
    mimetypes = ['text/x-roc']
    version_added = '2.22'

    keywords = words((
        'app', 'as', 'break', 'crash', 'dbg', 'else', 'expect', 'for',
        'if', 'import', 'in', 'match', 'platform', 'return', 'var',
        'where', 'while',
    ), suffix=r'\b')

    # Word operators.
    word_operators = words(('and', 'or'), suffix=r'\b')

    # Multi-character operators, longest first.
    operators = words((
        '...', '..=', '..', ':=', '::', '=>', '->', '|>', '==', '!=',
        '<=', '>=', '??', '?:', '//', '.?',
    ))

    tokens = {
        'root': [
            # Comments
            (r'#.*?$', Comment.Single),
            # Block strings: a line whose content starts with `\\`
            (r'\\\\', String, 'blockstring'),
            include('expr'),
        ],
        # The rules of a plain expression; shared between the root state
        # and `${...}` interpolations.
        'expr': [
            (r'\s+', Whitespace),
            # Strings
            (r'"', String, 'string'),
            # Keywords
            (keywords, Keyword.Reserved),
            (word_operators, Operator.Word),
            # Numbers with an explicit type suffix, e.g. ``5.U8``
            (r'\d+\.(?:U8|I8|U16|I16|U32|I32|U64|I64|U128|I128|F32|F64|'
             r'Dec)\b', Number.Integer),
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'0[oO][0-7]+', Number.Oct),
            (r'0[bB][01]+', Number.Bin),
            (r'\d+\.\d+', Number.Float),
            (r'\d+', Number.Integer),
            # ``var`` names are prefixed with ``$``
            (r'\$[a-z][a-zA-Z0-9_]*', Name.Variable),
            # A lowercase name starting a line is a definition
            (r'^[a-z_][a-zA-Z0-9_]*', Name.Function),
            # Tags, nominal types and module names are uppercase
            (r'[A-Z][a-zA-Z0-9_]*', Keyword.Type),
            (r'[a-z_][a-zA-Z0-9_]*', Name),
            # Operators
            (operators, Operator),
            (r'[+\-*/%<>!?=|&.]', Operator),
            # Punctuation
            (r'[,()\[\]{}:]', Punctuation),
        ],
        # Interpolation inside a string or block string: the opening
        # ``${`` was already consumed.  A ``}`` ends the interpolation,
        # unless it belongs to a nested ``{ ... }`` expression.
        'interp': [
            (r'\{', Punctuation, '#push'),
            (r'\}', String.Interpol, '#pop'),
            include('expr'),
        ],
        'string': [
            (r'\\u\([0-9A-Fa-f]{1,6}\)', String.Escape),
            (r'\\[$"nrt\\]', String.Escape),
            (r'\$\{', String.Interpol, 'interp'),
            (r'[^"$\\]+', String),
            (r'[$\\]', String),
            (r'"', String, '#pop'),
        ],
        # One line of a block string; ``\\`` was already consumed.  The
        # next line, if it also starts with ``\\``, re-enters this state.
        'blockstring': [
            (r'\\u\([0-9A-Fa-f]{1,6}\)', String.Escape),
            (r'\\[$"nrt\\]', String.Escape),
            (r'\$\{', String.Interpol, 'interp'),
            (r'[^\n$"\\]+', String),
            (r'\n', String, '#pop'),
            (r'[$\\]', String),
        ],
    }
