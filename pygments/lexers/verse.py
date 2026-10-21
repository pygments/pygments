"""
    pygments.lexers.verse
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Verse programming language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, include, words
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Whitespace

__all__ = ['VerseLexer']


class VerseLexer(RegexLexer):
    """
    Lexer for Verse, the functional logic programming language developed by
    Epic Games for Unreal Editor for Fortnite (UEFN).

    Based on the lexer that Epic Games publishes under CC0 in
    `verselang/book <https://github.com/verselang/book>`_.
    """

    name = 'Verse'
    url = 'https://dev.epicgames.com/documentation/en-us/fortnite/verse-language-reference'
    aliases = ['verse']
    filenames = ['*.verse']
    mimetypes = ['text/x-verse', 'application/x-verse']
    version_added = '2.22'

    flags = re.MULTILINE

    specifiers = (
        'abstract', 'allocates', 'castable', 'closed', 'computes', 'concrete',
        'constructor', 'converges', 'decides', 'epic_internal', 'final',
        'final_super', 'inline', 'internal', 'localizes', 'module_scoped_var_weak_map_key',
        'native', 'native_callable', 'no_rollback', 'open', 'override', 'persistable',
        'predicts', 'private', 'protected', 'public', 'reads', 'scoped', 'suspends',
        'transacts', 'unique', 'writes',
    )

    block_keywords = (
        'array', 'block', 'case', 'defer', 'else', 'first', 'for', 'if', 'loop',
        'map', 'then',
    )

    data_structure_keywords = (
        'class', 'enum', 'interface', 'module', 'struct',
    )

    decl_keywords = (
        'alias', 'live', 'ref', 'set', 'using', 'var',
    )

    type_keywords = (
        'any', 'char', 'char32', 'comparable', 'float', 'int', 'logic', 'option',
        'rational', 'string', 'tuple', 'type', 'void',
    )

    reserved_words = (
        'branch', 'break', 'catch', 'const', 'continue', 'do', 'in', 'is',
        'mutable', 'of', 'profile', 'race', 'return', 'rush', 'Self', 'spawn',
        'sync', 'until', 'upon', 'when', 'where', 'while', 'with', 'yield',
    )

    tokens = {
        'root': [
            (r'^([ \t]*)(<#>[^\n]*\n(?:\1[ \t]+[^\n]*\n|[ \t]*\n)*)',
             bygroups(Whitespace, Comment.Multiline)),
            (r'\n', Whitespace),
            (r'[^\S\n]+', Whitespace),

            (r'#[^\n]*', Comment.Single),
            (r'<#', Comment.Multiline, 'multiline-comment'),

            (r'(using)(\s*)(\{)', bygroups(Keyword.Namespace, Whitespace, Punctuation),
             'using-block'),

            (r'<(' + '|'.join(specifiers) + r')(\{[^}]*\})?>', Name.Decorator),

            (r'\b(' + '|'.join(data_structure_keywords) + r')(\s+)([a-zA-Z_]\w*)',
             bygroups(Keyword, Whitespace, Name.Class)),

            (words(block_keywords, prefix=r'\b', suffix=r'\b'), Keyword),
            (words(data_structure_keywords, prefix=r'\b', suffix=r'\b'), Keyword),
            (words(decl_keywords, prefix=r'\b', suffix=r'\b'), Keyword.Declaration),
            (words(type_keywords, prefix=r'\b', suffix=r'\b'), Keyword.Type),
            (words(reserved_words, prefix=r'\b', suffix=r'\b'), Keyword.Reserved),
            (words(('and', 'or', 'not'), prefix=r'\b', suffix=r'\b'), Operator.Word),
            (r'\b(true|false)\b', Keyword.Constant),

            (r'\b([a-zA-Z_]\w*)(\s*)(<\w+(?:\{[^}]*\})?>)?(\s*)(\()',
             bygroups(Name.Function, Whitespace, Name.Decorator, Whitespace, Punctuation)),

            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'0o[0-9a-fA-F]+', String.Char),
            (r'0u[0-9a-fA-F]+', String.Char),
            (r'[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?', Number.Float),
            (r'[0-9]+[eE][+-]?[0-9]+', Number.Float),
            (r'[0-9]+', Number.Integer),

            (r'"', String.Double, 'string-double'),
            (r"'(\\.|[^'\\\n])*'", String.Char),

            (r':=|=>|->|\.\.|[+\-*/]?=|<>|<=|>=|[<>+\-*/?:]', Operator),

            (r'\(([a-zA-Z_]\w*):\)', Name.Builtin.Pseudo),
            (r'[{}\[\](),;.]', Punctuation),

            (r'@[a-zA-Z_]\w*', Name.Decorator),
            (r'[a-zA-Z_]\w*', Name),
        ],

        'multiline-comment': [
            (r'<#', Comment.Multiline, '#push'),
            (r'#>', Comment.Multiline, '#pop'),
            (r'[^<#]+', Comment.Multiline),
            (r'[<#]', Comment.Multiline),
        ],

        'using-block': [
            (r'\s+', Whitespace),
            (r'[/.,]', Punctuation),
            (r'[a-zA-Z_]\w*', Name.Namespace),
            (r'\}', Punctuation, '#pop'),
        ],

        'string-double': [
            (r'\\.', String.Escape),
            (r'<#', Comment.Multiline, 'multiline-comment'),
            (r'\{', String.Interpol, 'interpolation'),
            (r'[^"\\{<]+', String.Double),
            (r'<', String.Double),
            (r'"', String.Double, '#pop'),
        ],

        'interpolation': [
            (r'\}', String.Interpol, '#pop'),
            include('root'),
        ],
    }

    def analyse_text(text):
        score = 0.0
        if re.search(r'^using\s*\{\s*/\w', text, re.MULTILINE):
            score += 0.4
        if re.search(r'<(suspends|decides|transacts|computes)>', text):
            score += 0.4
        return score
