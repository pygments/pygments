"""
    pygments.lexers.soar
    ~~~~~~~~~~~~~~~~~~~~

    Pygments lexers for Soar.

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re


from pygments.lexer import RegexLexer, bygroups, default, words
from pygments.token import Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace

__all__ = ['SoarLexer']


class SoarLexer(RegexLexer):

    """
    Lexer for Soar 
    """

    name = 'SOAR'
    aliases = ['soar', 'Soar']
    filenames = ['*.soar']
    mimetypes = ["text/soar", "text/x-soar"]
    url = "https://soar.eecs.umich.edu/soar_manual/03_SyntaxOfSoarPrograms/"
    version_added = '2.18'

    keyword_types = (words((
        'sp', 'propose', 'apply', 'elaborate'
    )), Keyword.Type)

    builtin_macros = (words((
        'write', 'log',
        'sin', 'cos',
    ), suffix=r'!'), Name.Function.Magic)

    tokens = {
        'root': [
            (r'\s+', Whitespace),  # whitespace
            (r'#.*$', Comment.Single),  # single-line comment
            (r'-->', Punctuation),  # the arrow
            (r'\{|\}', Punctuation),  # braces
            (r'\(|\)', Punctuation),  # parentheses
            (r'<[^>]+>', Name.Variable),  # variable names in <>
            # operators and other symbols
            (r'(\^|\+|=|<>|[{}])', Operator),
            (r'([a-zA-Z][a-zA-Z0-9_\-\*]*)', Name),  # identifiers
            (r'(\d+\.\d*|\.\d+|\d+)', Number),  # numbers
            (r'"[^"]*"', String),  # strings
        ],
    }
