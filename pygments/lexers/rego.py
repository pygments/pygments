"""
    pygments.lexers.rego
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for the Rego policy languages.

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
from pygments.lexer import RegexLexer, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, Number, Punctuation, Whitespace

class RegoLexer(RegexLexer):
    """
    For Rego source.
    """
    name = 'Rego'
    url = 'https://www.openpolicyagent.org/docs/latest/policy-language/'
    filenames = ['*.rego']
    aliases = ['rego']
    mimetypes = ['text/x-rego']
    version_added = '2.18'

    reserved_words = (
        'as', 'contains', 'data', 'default', 'else', 'every', 'false',
        'if', 'in', 'import', 'package', 'not', 'null',
        'some', 'true', 'with'
    )

    builtins = (
        # https://www.openpolicyagent.org/docs/latest/philosophy/#the-opa-document-model
        'data',  # Global variable for accessing base and virtual documents
        'input', # Represents synchronously pushed base documents
    )

    tokens = {
        'root': [
            (r'\n', Text.Whitespace),
            (r'\s+', Text),
            (r'#.*?$', Comment.Single),
            (words(reserved_words, suffix=r'\b'), Keyword),
            (words(builtins, suffix=r'\b'), Name.Builtin),
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r'`[^`]*`', String.Backtick),
            (r'-?\d+(\.\d+)?', Number),
            (r'(==|!=|<=|>=|:=)', Operator),  # Compound operators
            (r'[=<>+\-*/%&|]', Operator),     # Single-character operators
            (r'[\[\]{}(),.:;]', Punctuation),
        ]
    }

__all__ = ['RegoLexer']



