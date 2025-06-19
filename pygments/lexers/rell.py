"""
    pygments.lexers.rell
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for the Rell language.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
""" 

import re
from pygments.lexer import RegexLexer

__all__ = ['RellLexer']

class RellLexer(RegexLexer):
    """
    A Lexer for Rell.
    """
    name = 'Rell'
    url = 'https://docs.chromia.com/rell/rell-intro'
    aliases = ['rell']
    filenames = ['*.rell']
    mimetypes = ['text/x-rell']
    version_added = '2.19.2'

    reserved_words =
        reserved_words_module + \
        reserved_words_control + \
        reserved_words_branch + \
        reserved_words_definition + \
        reserved_words_constant + \
        reserved_words_boolop + \
        reserved_words_type + \
        reserved_words_database + \
        reserved_words_other + \
        reserved_words_todo

    reserved_words_module = (
        'import', 'module',
    )

    reserved_words_control = (
        'break', 'continue', 'for', 'return', 'when', 'while',
    )

    reserved_words_branch = (
        'if', 'else',
    )

    reserved_words_definition = (
        'entity', 'enum', 'function', 'namespace', 'object', 'operation',
        'query', 'record', 'struct', 'val', 'var',
    )

    reserved_words_boolop = (
        'and', 'not', 'or',
    )

    reserved_words_database = (
        'create', 'delete', 'index', 'limit', 'offset', 'update',
    )

    reserved_words_other = (
        'abstract', 'class', 'guard', 'in', 'include', 'key', 'override',
    )
    )

    tokens = {
        'root': [
            (r'//.*?$', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline)
            (r'"', String, 'string_dq'),
            (r'\'', String, 'string_sq'),
            (r'x(\'[0-9]*\'|"[0-9]*")', String, 'byte_array'),
            (r'(big_integer|boolean|byte_array|collection|decimal|gtv|integer|'
             r'iterable|json|list|map|mutable|set|text|virtual)\b',
             Keyword.Type),
            (r'(false|true|null)\b', Keyword.Constant),
        ],
        'string_dq': [
            (r'[^\\"]+', String),
            (r'\\\\', String),  # Escaped backslash
            (r'\\"', String),  # Escaped quote
            (r'\\', String),  # Bare backslash
            (r'"', String, '#pop'),  # Closing quote
        ],
        'string_sq': [
            (r'[^\\\']+', String),
            (r'\\\\', String),  # Escaped backslash
            (r'\\\'', String),  # Escaped quote
            (r'\\', String),  # Bare backslash
            (r'\'', String, '#pop'),  # Closing quote
        ],
    }
