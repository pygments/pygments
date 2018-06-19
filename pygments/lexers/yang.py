# -*- coding: utf-8 -*-
"""
    pygments.lexers.yang
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for the YANG 1.1 modeling language. See :rfc:`7950`.

    :copyright: Copyright 2006-2018 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words
from pygments.token import Comment, Keyword, Name, Punctuation, String, Text

__all__ = ['YangLexer']


class YangLexer(RegexLexer):
    name = 'YANG'
    aliases = ['yang', 'Yang']
    filenames = ['*.yang']
    mimetypes = ['application/yang']

    KEYWORDS = (
        'action', 'anydata', 'anyxml', 'argument', 'augment', 'base',
        'belongs-to', 'bit', 'case', 'choice', 'config', 'contact', 'container',
        'default', 'description', 'deviate', 'deviation', 'enum',
        'error-app-tag', 'error-message', 'extension', 'feature',
        'fraction-digits', 'grouping', 'identity', 'if-feature', 'import',
        'include', 'input', 'key', 'leaf', 'leaf-list', 'length', 'list',
        'mandatory', 'max-elements', 'min-elements', 'modifier', 'module',
        'must', 'namespace', 'notification', 'ordered-by', 'organization',
        'output', 'path', 'pattern', 'position', 'prefix', 'presence', 'range',
        'reference', 'refine', 'require-instance', 'revision', 'revision-date',
        'rpc', 'status', 'submodule', 'type', 'typedef', 'unique', 'units',
        'uses', 'value', 'when', 'yang-version', 'yin-element', 'add',
        'current', 'delete', 'deprecated', 'false', 'invert-match', 'max',
        'min', 'not-supported', 'obsolete', 'replace', 'system', 'true',
        'unbounded', 'user', 'and', 'or', 'not',)
    TYPES = (
        'binary', 'bits', 'boolean', 'decimal64', 'empty', 'enumeration',
        'int8', 'int16', 'int32', 'int64', 'string', 'uint8', 'uint16',
        'uint32', 'uint64', 'union', 'leafref', 'identityref',
        'instance-identifier',)

    tokens = {
        'comments': [
            (r'[^*/]', Comment),
            (r'/\*', Comment, '#push'),
            (r'\*/', Comment, '#pop'),
            (r'[*/]', Comment),
        ],
        'root': [
            (r'\s+', Text),
            (r'[\{\};\+]+', Punctuation),
            (words(KEYWORDS, suffix=r'(?=[^\w-])'), Keyword),
            (words(TYPES, suffix=r'(?=[^\w-])'), Name.Class),
            (r'"[^"\\]*(?:\\.[^"\\]*)*"', String),
            (r"'[^'\\]*(?:\\.[^'\\]*)*'", String),
            (r'/\*', Comment, 'comments'),
            (r'//.*?$', Comment),
            (r'[^;\{\}\s\*\+\'"]+', Name.Variable),
        ],
    }
