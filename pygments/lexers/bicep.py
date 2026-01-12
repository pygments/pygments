"""
    pygments.lexers.bicep
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for Azure Bicep.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, words, include, default
from pygments.token import (
    Text, Comment, Operator, Keyword, Name, String, Number,
    Punctuation, Whitespace
)

__all__ = ['BicepLexer']


class BicepLexer(RegexLexer):
    """
    Lexer for Azure Bicep infrastructure as code language.

    Bicep is a Domain Specific Language (DSL) for deploying Azure resources
    declaratively. It aims to drastically simplify the authoring experience
    with a cleaner syntax and better support for modularity and code re-use.

    .. versionadded:: 2.19
    """

    name = 'Bicep'
    url = 'https://learn.microsoft.com/azure/azure-resource-manager/bicep/'
    aliases = ['bicep']
    filenames = ['*.bicep', '*.bicepparam']
    mimetypes = ['text/x-bicep']
    version_added = '2.19'

    tokens = {
        'root': [
            include('whitespace'),
            include('comments'),

            # Decorators
            (r'@\w+', Name.Decorator),

            # Keywords
            (words((
                'targetScope', 'resource', 'module', 'param', 'var', 'output',
                'for', 'in', 'if', 'existing', 'import', 'from', 'as',
                'metadata', 'type', 'func', 'using', 'with', 'extension',
                'provider', 'assert'
            ), suffix=r'\b'), Keyword),

            # Built-in data types
            (words((
                'string', 'int', 'bool', 'object', 'array', 'null',
                'resourceInput', 'resourceOutput'
            ), suffix=r'\b'), Keyword.Type),

            # Target scope values
            (words((
                'resourceGroup', 'subscription', 'managementGroup', 'tenant'
            ), suffix=r'\b'), Keyword.Constant),

            # Boolean literals
            (r'\b(true|false)\b', Keyword.Constant),

            # Null literal
            (r'\bnull\b', Keyword.Constant),

            # Numbers
            (r'\b\d+\b', Number.Integer),

            # Resource type with version (e.g., 'Microsoft.Storage/storageAccounts@2023-01-01')
            (r"'[A-Za-z0-9_./-]+@[\d-]+(?:-preview|-alpha|-beta)?'", String.Symbol),

            # Strings - Single quoted
            (r"'", String.Single, 'string-single'),

            # Strings - Multi-line
            (r"'''", String.Single, 'string-multiline'),

            # Property names and identifiers
            (r'[a-zA-Z_]\w*(?=\s*:)', Name.Attribute),  # Property names (before colon)
            (r'[a-zA-Z_]\w*', Name),

            # Operators
            (r'\.(?:\?)?', Operator),  # Dot and safe dereference operator
            (r'\?\?', Operator),  # Coalesce operator
            (r'\?', Operator),  # Ternary operator
            (r':', Operator),  # Ternary operator (also used in property definitions, but that's okay)
            (r'[=!<>]=?', Operator),
            (r'[+\-*/%]', Operator),
            (r'&&|\|\|', Operator),  # Logical AND/OR
            (r'\|', Operator),  # Pipe for union types
            (r'!', Operator),
            (r'=>', Operator),  # Lambda arrow for functions

            # Punctuation
            (r'[{}\[\](),;]', Punctuation),  # Removed ':' since it's now an operator
        ],

        'whitespace': [
            (r'\s+', Whitespace),
        ],

        'comments': [
            (r'//.*?$', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment-multiline'),
        ],

        'comment-multiline': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],

        'string-single': [
            (r"\\['\\nrtu]", String.Escape),
            (r"\$\{", String.Interpol, 'interpolation'),
            (r"[^'\\$]+", String.Single),
            (r"\$(?!\{)", String.Single),
            (r"'", String.Single, '#pop'),
        ],

        'string-multiline': [
            (r"\$\{", String.Interpol, 'interpolation'),
            (r"[^'$]+", String.Single),
            (r"\$(?!\{)", String.Single),
            (r"'''", String.Single, '#pop'),
            (r"'", String.Single),
        ],

        'interpolation': [
            (r'\{', Punctuation, '#push'),
            (r'\}', String.Interpol, '#pop'),
            include('root'),
        ],
    }

    def analyse_text(text):
        """Bicep files typically start with decorators, keywords, or comments."""
        # Check for common Bicep patterns
        score = 0.0

        # Check for targetScope declaration
        if re.search(r'^\s*targetScope\s*=\s*', text, re.MULTILINE):
            score += 0.4

        # Check for resource declarations
        if re.search(r'^\s*resource\s+\w+\s+["\'][\w./-]+@[\d-]+', text, re.MULTILINE):
            score += 0.4

        # Check for param declarations
        if re.search(r'^\s*param\s+\w+\s+\w+', text, re.MULTILINE):
            score += 0.2

        # Check for decorators
        if re.search(r'^\s*@\w+', text, re.MULTILINE):
            score += 0.2

        # Check for module declarations
        if re.search(r'^\s*module\s+\w+\s+["\']', text, re.MULTILINE):
            score += 0.2

        return score
