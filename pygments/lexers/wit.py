"""
    pygments.lexers.wit
    ~~~~~~~~~~~~~~~~~~~

    Lexer for the WebAssembly Interface Types (WIT) IDL.

    WIT is an IDL for the WebAssembly Component Model that describes
    component interfaces.

    The grammar can be found at:
    https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md

    :copyright: Copyright 2006-2026 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, default, words
from pygments.token import (
    Comment,
    Keyword,
    Name,
    Number,
    Operator,
    Punctuation,
    Text,
    Whitespace,
)

__all__ = ['WitLexer']


class WitLexer(RegexLexer):
    """
    Lexer for the WebAssembly Interface Types (WIT) IDL.

    .. versionadded:: 2.20
    """

    name = 'WIT'
    url = 'https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md'
    aliases = ['wit']
    filenames = ['*.wit']
    mimetypes = ['text/x-wit', 'application/x-wit']
    version_added = '2.20'

    # WIT keywords from the specification
    _keywords = (
        'as',
        'borrow',
        'constructor',
        'enum',
        'export',
        'flags',
        'from',
        'func',
        'import',
        'include',
        'interface',
        'own',
        'package',
        'record',
        'resource',
        'static',
        'type',
        'use',
        'variant',
        'with',
        'world',
    )

    # Built-in types
    _builtin_types = (
        'u8', 'u16', 'u32', 'u64',
        's8', 's16', 's32', 's64',
        'f32', 'f64',
        'char',
        'bool',
        'string',
        'tuple',
        'list',
        'option',
        'result',
        'future',
        'stream',
    )

    tokens = {
        'root': [
            # Whitespace
            (r'\s+', Whitespace),

            # Comments
            (r'//.*?$', Comment.Single),
            (r'/\*', Comment.Multiline, 'multiline-comment'),

            # Feature gates (annotations)
            (r'@(since|unstable|deprecated)\b', Name.Decorator, 'annotation'),

            # Package declaration
            (r'(package)(\s+)',
             bygroups(Keyword.Namespace, Whitespace), 'package-decl'),

            # Kebab-case identifiers MUST be matched before keywords/types
            # to handle cases like tuple-arg, list-of-items, string-list, etc.
            (r'[a-zA-Z][a-zA-Z0-9]*(?:-[a-zA-Z0-9]+)+', Name),

            # Keywords (only match when NOT followed by hyphen)
            (words(_keywords, suffix=r'(?!-)(?=\s|[<>:;,{}()\[\]=]|$)'),
             Keyword),

            # 'async' is a keyword but needs special handling
            (r'\basync(?!-)(?=\s|[<>:;,{}()\[\]=]|$)', Keyword),

            # Built-in types (only match when NOT followed by hyphen)
            (words(_builtin_types, suffix=r'(?!-)(?=\s|[<>:;,{}()\[\]=]|$)'),
             Keyword.Type),

            # Special underscore for result<_, err>
            (r'\b_\b', Keyword.Type),

            # Version numbers (semver) - after @ in package names
            (r'@\d+\.\d+\.\d+(-[a-zA-Z0-9.-]+)?(\+[a-zA-Z0-9.-]+)?',
             Name.Label),

            # Integers
            (r'\b[0-9]+\b', Number.Integer),

            # Operators and punctuation
            (r'->', Operator),
            (r'[=,;{}()<>*/.]', Punctuation),
            (r':', Punctuation),

            # Escaped identifiers (starting with %)
            (r'%[a-zA-Z][a-zA-Z0-9]*(?:-[a-zA-Z0-9]+)*', Name.Variable),

            # Regular identifiers (simple, no hyphens)
            (r'[a-zA-Z][a-zA-Z0-9]*', Name),
        ],

        'multiline-comment': [
            (r'/\*', Comment.Multiline, '#push'),  # nested comments
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[^/*]+', Comment.Multiline),
            (r'[/*]', Comment.Multiline),
        ],

        'annotation': [
            (r'\s+', Whitespace),
            (r'\(', Punctuation, 'annotation-params'),
            default('#pop'),
        ],

        'annotation-params': [
            (r'\s+', Whitespace),
            (r'(feature|version)(\s*)(=)',
             bygroups(Name.Attribute, Whitespace, Operator)),
            # Semver version in annotation
            (r'\d+\.\d+\.\d+(-[a-zA-Z0-9.-]+)?(\+[a-zA-Z0-9.-]+)?', Number),
            # Feature name (identifier)
            (r'[a-zA-Z][a-zA-Z0-9]*(?:-[a-zA-Z0-9]+)*', Name.Variable),
            (r',', Punctuation),
            (r'\)', Punctuation, '#pop:2'),
        ],

        'package-decl': [
            (r'\s+', Whitespace),
            # Escaped identifiers in package names
            (r'%[a-zA-Z][a-zA-Z0-9]*(?:-[a-zA-Z0-9]+)*', Name.Namespace),
            # Regular identifiers
            (r'[a-zA-Z][a-zA-Z0-9]*(?:-[a-zA-Z0-9]+)*', Name.Namespace),
            (r':', Punctuation),
            (r'/', Punctuation),
            # Version
            (r'@\d+\.\d+\.\d+(-[a-zA-Z0-9.-]+)?(\+[a-zA-Z0-9.-]+)?',
             Name.Label),
            # End of package declaration
            (r';', Punctuation, '#pop'),
            # Opening brace for nested package
            (r'\{', Punctuation, '#pop'),
        ],
    }
