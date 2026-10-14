"""
    pygments.lexers.mint
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for the Mint programming language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, include, words
from pygments.token import Comment, Operator, Keyword, Name, String, Number, \
    Punctuation, Whitespace

__all__ = ['MintLexer']


class MintLexer(RegexLexer):
    """
    For Mint source code.

    .. versionadded:: 2.22
    """
    name = 'Mint'
    url = 'https://www.mint-lang.com'
    aliases = ['mint']
    filenames = ['*.mint']
    version_added = '2.22'

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'root': [
            # Comments
            (r'/\*.*?\*/', Comment.Multiline),
            (r'//.*?$', Comment.Single),
            (r'\s+', Whitespace),

            # CSS blocks: `style root { ... }`
            (r'(\bstyle)(\s+)([a-z]\w*)(\s*)(\{)',
             bygroups(Keyword, Whitespace, Name.Class, Whitespace,
                      Punctuation), 'css'),

            # Entity definitions: `component Main`, `store Counter`,
            # `module Main`, `type Person`
            (r'\b(component|store|module|type)(\s+)([A-Z]\w*)',
             bygroups(Keyword, Whitespace, Name.Class)),

            # Function definitions: `fun increment`, `get fullName`
            (r'\b(fun|get)(\s+)([a-z_]\w*)',
             bygroups(Keyword, Whitespace, Name.Function)),

            # Keywords
            (words((
                'as', 'async', 'await', 'case', 'component', 'connect',
                'const', 'context', 'dbg', 'decode', 'defer', 'else', 'emit',
                'encode', 'exposing', 'false', 'for', 'fun', 'get', 'global',
                'highlight', 'if', 'let', 'locale', 'module', 'next', 'of',
                'or', 'property', 'provide', 'provider', 'return', 'routes',
                'signal', 'state', 'store', 'style', 'suite', 'test', 'true',
                'type', 'use', 'using', 'when',
            ), suffix=r'\b'), Keyword),

            # `void` (Mint's null value) and the common built-in types
            (words((
                'Array', 'Bool', 'Date', 'Html', 'Map', 'Maybe', 'Number',
                'Promise', 'Regexp', 'Result', 'Set', 'String', 'Time',
                'Void',
            ), suffix=r'\b'), Keyword.Type),

            # Strings, with interpolation and `"..." \ "..."` separators
            (r'"', String.Double, 'string'),

            # Inlined JavaScript, also interpolated
            (r'`', String.Backtick, 'js'),

            # Regexp literals: a `/` in expression position starts one
            # (Mint only treats `/` as division between operands), so it
            # must not follow an identifier, a number, a closing bracket or
            # a quote, and must not be followed by whitespace.
            (r'(?<![A-Za-z0-9_)\]"\'`])/(?!\s)(?:\\.|\[(?:\\.|[^\]\\])*\]'
             r'|[^/\\\n])+/[igmsuy]*', String.Regex),

            # Numbers (integers and floats; a unary minus is an operator)
            (r'\d+(?:\.\d+)?', Number),

            # HTML fragments: `<> ... </>`
            (r'<>', Punctuation, 'html-content'),
            (r'</>', Punctuation),

            # HTML elements and components, optionally with a style
            (r'(?<=[\s{(=,:>])(<)(/?)([A-Za-z][\w-]*(?:::[a-z]\w*)?)',
             bygroups(Punctuation, Punctuation, Name.Tag), 'html'),

            # Operators and punctuation
            (r'\|>|=>|\*\*|==|!=|<=|>=|&&|\|\||::', Operator),
            (r'[.\[\](){},:;+\-*/%<>=!|&^~\\]', Operator),

            # Capitalized identifiers are types, components or records
            (r'[A-Z]\w*', Name.Class),

            # Everything else is a name
            (r'[a-z_]\w*', Name),
        ],

        # Inside a string literal
        'string': [
            (r'\\.', String.Escape),
            (r'\#\{', String.Interpol, 'interpol'),
            (r'"', String.Double, '#pop'),
            (r'[^"\\#]+', String.Double),
            (r'.', String.Double),
        ],

        # Inside inlined JavaScript
        'js': [
            (r'\\.', String.Escape),
            (r'\#\{', String.Interpol, 'interpol'),
            (r'`', String.Backtick, '#pop'),
            (r'[^`\\#]+', String.Backtick),
            (r'.', String.Backtick),
        ],

        # Inside a `#{...}` interpolation (nests for records and blocks)
        'interpol': [
            (r'\{', Punctuation, '#push'),
            (r'\}', String.Interpol, '#pop'),
            include('root'),
        ],

        # Inside a CSS block
        'css': [
            (r'\s+', Whitespace),
            (r'/\*.*?\*/', Comment.Multiline),
            (r'//.*?$', Comment.Single),
            (r'\#\{', String.Interpol, 'interpol'),
            # Hex colours must win over the id selector below
            (r'#[0-9a-fA-F]{3,8}\b', Number),
            (r'(\:)([\w-]+)', bygroups(Punctuation, Name.Decorator)),
            (r'(\.)([\w-]+)', bygroups(Punctuation, Name.Class)),
            (r'(\#)([\w-]+)', bygroups(Punctuation, Name.Namespace)),
            (r'(@)([\w-]+)', bygroups(Punctuation, Keyword)),
            (r'\{', Punctuation, '#push'),
            (r'\}', Punctuation, '#pop'),
            # Property name, up to the colon
            (r'[\w-]+(?=\s*:)', Keyword),
            (r'([a-zA-Z_][\w-]*)(\()', bygroups(Name.Function, Punctuation)),
            # Keyword values (`red`, `solid`, ...) and numbers
            (r'[a-z-]+', Keyword.Constant),
            (r'[>+~*\[\]()]', Punctuation),
            (r'[:;,]', Punctuation),
            (r'".*?"|\'.*?\'', String.Double),
            (r'\d+(?:\.\d+)?[a-z%]*', Number),
            (r'[$^|~*]=|[-+*/%!]=?', Operator),
        ],

        # Inside the tag of an HTML element or component
        'html': [
            (r'\s+', Whitespace),
            # Attribute with a string value
            (r'([\w:-]+)(=)(".*?")',
             bygroups(Name.Attribute, Operator, String.Double)),
            # Attribute with an expression value
            (r'([\w:-]+)(=)(\{)',
             bygroups(Name.Attribute, Operator, Punctuation), 'html-attr'),
            # Boolean attribute
            (r'[\w:-]+', Name.Attribute),
            # Closing tag of a self-closing element
            (r'/>', Punctuation, '#pop'),
            # End of the opening tag: lex the children as expressions
            (r'>', Punctuation, ('#pop', 'html-content')),
        ],

        # Between the tags of an element: children are expressions
        'html-content': [
            # HTML fragments: `<> ... </>`
            (r'</>', Punctuation, '#pop'),
            (r'<>', Punctuation),
            # Closing tag
            (r'</', Punctuation, 'html-close'),
            # Nested element, component or fragment
            (r'(<)(/?)([A-Za-z][\w-]*(?:::[a-z]\w*)?)',
             bygroups(Punctuation, Punctuation, Name.Tag), 'html'),
            # String children and `#{...}` interpolations
            (r'"', String.Double, 'string'),
            (r'\#\{', String.Interpol, 'interpol'),
            # Braces nest (if/for/case blocks); the brace matching the
            # enclosing block ends the markup
            (r'\{', Punctuation, '#push'),
            (r'\}', Punctuation, '#pop'),
            include('root'),
        ],
        'html-close': [
            (r'[\w:-]+', Name.Tag),
            (r'>', Punctuation, '#pop'),
        ],

        # Inside the `{...}` of an attribute value (nests for records)
        'html-attr': [
            (r'\{', Punctuation, '#push'),
            (r'\}', Punctuation, '#pop'),
            include('root'),
        ],
    }

    def analyse_text(text):
        if 'component ' in text or 'fun render' in text:
            return 1.0
        return 0.0
