"""
    pygments.lexers.bicep
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for Bicep DSL.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, words, bygroups, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, Number, Punctuation, Whitespace, Error

__all__ = ['BicepLexer']


class BicepLexer(RegexLexer):
    """
    Lexer for Bicep DSL for Azure Resource Manager.

    .. versionadded:: 2.19
    """

    name = 'Bicep'
    url = 'https://learn.microsoft.com/en-us/azure/azure-resource-manager/bicep/'
    aliases = ['bicep']
    filenames = ['*.bicep', '*.bicepparam']
    mimetypes = ['text/x-bicep']

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'root': [
            # Whitespace
            (r'\s+', Whitespace),

            # Comments
            (r'//.*?$', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline),

            # Decorators
            (r'@[a-zA-Z_]\w*', Name.Decorator),

            # Keywords
            (words((
                'targetScope', 'resource', 'module', 'param', 'var', 'output',
                'for', 'in', 'if', 'existing', 'import', 'as', 'using', 'with'),
                suffix=r'\b'), Keyword.Declaration),

            # Data types
            (words(('string', 'int', 'bool', 'object', 'array', 'null'), suffix=r'\b'), Keyword.Type),

            # Type modifiers
            (words(('secure', 'confidential'), suffix=r'\b'), Name.Builtin.Pseudo),

            # String literals
            (r'"', String.Double, 'doublestring'),
            (r"'", String.Single, 'singlestring'),

            # Numeric literals
            (r'\d+', Number.Integer),

            # Identifiers
            (r'[a-zA-Z_]\w*', Name.Other),

            # Punctuation
            (r'[{}\[\](),.;:]', Punctuation),

            # Operators
            (r'[=+*/%!<>|&^-]', Operator),

            # Resource type references and parameters (e.g., '@2023-01-01')
            (r'@[a-zA-Z_][\w.-]*', Name.Builtin),
        ],

        'doublestring': [
            (r'\\.', String.Escape),
            (r'\$\{', String.Interpol, 'interp_sq'),
            (r'"', String.Double, '#pop'),
            (r'[^"\\$]+', String.Double),
            (r'[\$]', String.Double),
        ],

        'singlestring': [
            (r"\\.", String.Escape),
            (r"\$\{", String.Interpol, 'interp_sq'),
            (r"'", String.Single, '#pop'),
            (r"[^'\\$]+", String.Single),
            (r"[\$]", String.Single),
        ],

        'interp_sq': [
            (r'\}', String.Interpol, '#pop'),
            include('root'),
        ],
    }

    def analyse_text(text):
        """
        Bicep files typically start with 'param', 'var', 'resource', or
        have decorators like '@description()' and resource types with '@'.
        """
        if any(text.startswith(kw) for kw in ('param', 'var', 'resource', 'module', 'output')):
            return 0.8
        if '@' in text and ('param' in text or 'resource' in text):
            return 0.5
        return 0