# -*- coding: utf-8 -*-
"""
    pygments.lexers.yang
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for the YANG 1.1 modeling language. See :rfc:`7950`.

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import (RegexLexer, bygroups, words)
from pygments.token import (Text, Token, Name, String, Comment,
                            Number)

__all__ = ['YangLexer']

class YangLexer(RegexLexer):
    """
    Lexer for `YANG <https://tools.ietf.org/html/rfc7950/>`_, based on RFC7950

    .. versionadded:: 2.7
    """
    name = 'YANG'
    aliases = ['yang']
    filenames = ['*.yang']
    mimetypes = ['application/yang']

    #Keywords from RFC7950 ; oriented at BNF style
    TOP_STMTS_KEYWORDS = ("module", "submodule")
    MODULE_HEADER_STMT_KEYWORDS = ("yang-version", "namespace", "prefix", "belongs-to")
    META_STMT_KEYWORDS = ("organization", "contact", "description",
                          "reference", "revision")
    LINKAGE_STMTS_KEYWORDS = ("import", "include", "revision-date")
    BODY_STMT_KEYWORDS = ("extension", "feature", "identity", "typedef",
                          "grouping", "augment", "rpc", "notification",
                          "deviation", "action", "argument", "identity",
                          "if-feature", "input", "output")
    DATA_DEF_STMT_KEYWORDS = ("container", "leaf-list", "leaf", "list",
                              "choice", "anydata", "anyxml", "uses",
                              "case", "config", "deviate", "must",
                              "when", "presence", "refine")
    TYPE_STMT_KEYWORDS = ("type", "units", "default", "status", "bit",
                          "enum", "error-app-tag", "error-message",
                          "fraction-digits", "length", "min-elements",
                          "max-elements", "modifier", "ordered-by", "path",
                          "pattern", "position", "range", "require-instance",
                          "value", "yin-element", "base")
    LIST_STMT_KEYWORDS = ("key", "mandatory", "unique")

    #RFC7950 other keywords
    CONSTANTS_KEYWORDS = ("true", "false", "current", "obsolete", "deprecated",
                          "add", "delete", "replace", "not-supported",
                          "invert-match", "max", "min", "unbounded", "user")

    #RFC7950 Built-In Types
    TYPES = ("binary", "bits", "boolean", "decimal64", "empty", "enumeration",
             "int8", "int16", "int32", "int64", "string", "uint8", "uint16",
             "uint32", "uint64", "union", "leafref", "identityref", "instance-identifier")

    suffix_re_pattern = r'(?=[^\w\-\:])'

    tokens = {
        'comments': [
            (r'[^*/]', Comment),
            (r'/\*', Comment, '#push'),
            (r'\*/', Comment, '#pop'),
            (r'[*/]', Comment),
        ],
        "root": [
            (r'\s+', Text.Whitespace),
            (r'[\{\}\;]+', Token.Punctuation),
            (r'(?<![\-\w])(and|or|not|\+|\.)(?![\-\w])', Token.Operator),

            (r'"(?:\\"|[^"])*?"', String.Double),
            (r"'(?:\\'|[^'])*?'", String.Single),

            (r'/\*', Comment, 'comments'),
            (r'//.*?$', Comment),

            #match BNF stmt for `node-identifier` with [ prefix ":"]
            (r'(?:^|(?<=[\s{};]))([\w.-]+)(:)([\w.-]+)(?=[\s{};])',
             bygroups(Name.Namespace, Token.Punctuation, Name.Variable)),

            #match BNF stmt `date-arg-str`
            (r'([0-9]{4}\-[0-9]{2}\-[0-9]{2})(?=[\s\{\}\;])', Name.Label),
            (r'([0-9]+\.[0-9]+)(?=[\s\{\}\;])', Number.Float),
            (r'([0-9]+)(?=[\s\{\}\;])', Number.Integer),

            (words(TOP_STMTS_KEYWORDS, suffix=suffix_re_pattern), Token.Keyword),
            (words(MODULE_HEADER_STMT_KEYWORDS, suffix=suffix_re_pattern), Token.Keyword),
            (words(META_STMT_KEYWORDS, suffix=suffix_re_pattern), Token.Keyword),
            (words(LINKAGE_STMTS_KEYWORDS, suffix=suffix_re_pattern), Token.Keyword),
            (words(BODY_STMT_KEYWORDS, suffix=suffix_re_pattern), Token.Keyword),
            (words(DATA_DEF_STMT_KEYWORDS, suffix=suffix_re_pattern), Token.Keyword),
            (words(TYPE_STMT_KEYWORDS, suffix=suffix_re_pattern), Token.Keyword),
            (words(LIST_STMT_KEYWORDS, suffix=suffix_re_pattern), Token.Keyword),
            (words(TYPES, suffix=suffix_re_pattern), Name.Class),
            (words(CONSTANTS_KEYWORDS, suffix=suffix_re_pattern), Name.Class),

            (r'[^;{}\s\'\"]+', Name.Variable),
        ]
    }
