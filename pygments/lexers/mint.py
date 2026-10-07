"""
    pygments.lexers.mint
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for the Mint programming language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, default, include, using, words
from pygments.lexers.css import CssLexer
from pygments.lexers.javascript import JavascriptLexer
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Whitespace

__all__ = ['MintLexer']


class MintLexer(RegexLexer):
    """
    For Mint source code.
    """

    name = 'Mint'
    url = 'https://mint-lang.com/'
    aliases = ['mint']
    filenames = ['*.mint']
    mimetypes = ['text/x-mint']
    version_added = '2.22'
    flags = re.MULTILINE

    tokens = {
        'root': [
            include('common'),
            (r'[{}()\[\],;:]', Punctuation),
            (r'.', Operator),
        ],
        'common': [
            (r'\s+', Whitespace),
            (r'/\*', Comment.Multiline, 'comment'),
            (r'//.*?$', Comment.Single),

            (r'"', String, 'string'),
            (r'`', String.Backtick, 'js'),
            (r'/(?:\\.|[^/\\\n])+/[gimsuy]*', String.Regex),

            (r'(</?)([A-Za-z][\w.-]*)', bygroups(Punctuation, Name.Tag),
             'html-tag'),
            (r'</?>', Punctuation),

            (r'(style)(\s+)([a-zA-Z_]\w*)(\s*)(\{)',
             bygroups(Keyword, Whitespace, Name.Function, Whitespace, Punctuation),
             'style'),

            (words((
                'as', 'case', 'catch', 'component', 'connect', 'const',
                'decode', 'else', 'encode', 'enum', 'exposing', 'finally',
                'for', 'fun', 'get', 'global', 'if', 'module', 'next', 'or',
                'parallel', 'property', 'provider', 'record', 'routes',
                'sequence', 'state', 'store', 'suite', 'test', 'try', 'use',
                'using', 'when', 'where', 'with',
            ), suffix=r'\b'), Keyword),
            (words(('true', 'false'), suffix=r'\b'), Keyword.Constant),
            (r'\bof\b', Operator.Word),

            (r'@[A-Za-z]+', Name.Decorator),
            (r'[A-Z][A-Z0-9_]*\b', Name.Constant),
            (r'[A-Z][A-Za-z0-9]*\b', Name.Class),
            (r'[a-z_][a-zA-Z0-9]*\b', Name),

            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'\d+\.\d+', Number.Float),
            (r'\d+', Number.Integer),

            (r'=>|\|>|\.\.\.|\|\||&&|==|!=|<=|>=', Operator),
            (r'[!%&*+\-./<=>?^|~]+', Operator),
        ],
        'comment': [
            (r'[^*]+', Comment.Multiline),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'\*', Comment.Multiline),
        ],
        'string': [
            (r'\\.', String.Escape),
            (r'#\{', String.Interpol, 'interpol'),
            (r'[^"\\#]+', String),
            (r'#', String),
            (r'"', String, '#pop'),
        ],
        'interpol': [
            (r'\{', Punctuation, '#push'),
            (r'\}', Punctuation, '#pop'),
            include('common'),
            (r'[{}()\[\],;:]', Punctuation),
        ],
        'js': [
            (r'`', String.Backtick, '#pop'),
            (r'[^`]+', using(JavascriptLexer)),
        ],
        'html-tag': [
            (r'\s+', Whitespace),
            (r'::', Punctuation),
            (r'([\w:-]+)(\s*)(=)(\s*)',
             bygroups(Name.Attribute, Whitespace, Operator, Whitespace),
             'html-attr'),
            (r'[\w:-]+', Name.Attribute),
            (r'/?>', Punctuation, '#pop'),
        ],
        'html-attr': [
            (r'\{', Punctuation, ('#pop', 'interpol')),
            (r'"', String, ('#pop', 'string')),
            (r"[^>\s{]+", String, '#pop'),
            default('#pop'),
        ],
        'style': [
            (r'\{', Punctuation, '#push'),
            (r'\}', Punctuation, '#pop'),
            (r'#\{', String.Interpol, 'interpol'),
            (r'[^{}#]+', using(CssLexer)),
        ],
    }
