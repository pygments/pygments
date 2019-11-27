#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""The module that parses Pixar's Universal Scene Description file format."""

from pygments import lexer
from pygments import token

from . import _usd_builtins

__all__ = ["UsdLexer"]


_PUNCTUATION = [
    (r"\(", token.Punctuation),
    (r"\)", token.Punctuation),
    (r"\[", token.Punctuation),
    (r"\]", token.Punctuation),
    ("{", token.Punctuation),
    ("}", token.Punctuation),
]


def _keywords(words, type_):
    """list[tuple[:class:`pygments.lexer.words`, :class:`pygments.token._TokenType`]]."""
    return [(lexer.words(words, prefix=r"\b", suffix=r"\b"), type_)]


class UsdLexer(lexer.RegexLexer):
    """
    A lexer that parses Pixar's Universal Scene Description file format.

    .. versionadded:: 2.5.0
    """

    name = 'USD'
    aliases = ['usd', 'usda']
    filenames = ['*.usd', '*.usda']

    tokens = {
        'root':
            _keywords(_usd_builtins.KEYWORDS, token.Keyword) +
            _keywords(_usd_builtins.SPECIAL_NAMES, token.Name.Attribute) +
            _keywords(_usd_builtins.COMMON_ATTRIBUTES, token.Name.Attribute) +
            [(r"\b\w+:[\w:]+\b", token.Name.Attribute)] +  # more attributes
            _keywords(_usd_builtins.OPERATORS, token.Operator) +
            _keywords(_usd_builtins.TYPES, token.Keyword.Type) +
            _PUNCTUATION +
            [
                ('(\w+)(\s+)(\w+)(\s+)(=)', lexer.bygroups(
                    token.Keyword.Type,
                    token.Whitespace,
                    token.Name.Attribute,
                    token.Whitespace,
                    token.Generic,
                )),
                ('#.*?$', token.Comment.Single),
                (',', token.Generic),
                ('=', token.Operator),
                ('[+-]?([0-9]*[.])?[0-9]+', token.Number),
                (r'".*"', token.String),
                (r'<([\w/]+|[\w/]+\.\w+)>', token.Name.Namespace),
                (r'@.*@', token.String.Interpol),
                (r'\(.*"[.\\n]*".*\)', token.String.Doc),
                (r'\A#usda .+$', token.Comment.Hashbang),
                (r'\s+', token.Text),
                (r'[\w+|:|\.]', token.Generic),
            ],
    }
