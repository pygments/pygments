#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""The module that parses Pixar's Universal Scene Description file format."""

from pygments import lexer, token

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


_TYPE = r"(\w+(?:\[\])?)"
_BASE_ATTRIBUTE = r"([\w_]+(?:\:[\w_]+)*)(?:(\.)(timeSamples))?"
_WHITESPACE = r"([ \t]+)"


class UsdLexer(lexer.RegexLexer):
    """
    A lexer that parses Pixar's Universal Scene Description file format.

    .. versionadded:: 2.6.0
    """

    name = "USD"
    aliases = ["usd", "usda"]
    filenames = ["*.usd", "*.usda"]

    tokens = {
        "root": [
            (
                r"(custom){_WHITESPACE}(uniform)(\s+){}(\s+){}(\s*)(=)".format(
                    _TYPE, _BASE_ATTRIBUTE, _WHITESPACE=_WHITESPACE
                ),
                lexer.bygroups(
                    token.Keyword.Token,
                    token.Whitespace,
                    token.Keyword.Token,
                    token.Whitespace,
                    token.Keyword.Type,
                    token.Whitespace,
                    token.Name.Attribute,
                    token.Generic,
                    token.Name.Keyword.Tokens,
                    token.Whitespace,
                    token.Operator,
                ),
            ),
            (
                r"(custom){_WHITESPACE}{}(\s+){}(\s*)(=)".format(
                    _TYPE, _BASE_ATTRIBUTE, _WHITESPACE=_WHITESPACE
                ),
                lexer.bygroups(
                    token.Keyword.Token,
                    token.Whitespace,
                    token.Keyword.Type,
                    token.Whitespace,
                    token.Name.Attribute,
                    token.Generic,
                    token.Name.Keyword.Tokens,
                    token.Whitespace,
                    token.Operator,
                ),
            ),
            (
                r"(uniform){_WHITESPACE}{}(\s+){}(\s*)(=)".format(
                    _TYPE, _BASE_ATTRIBUTE, _WHITESPACE=_WHITESPACE
                ),
                lexer.bygroups(
                    token.Keyword.Token,
                    token.Whitespace,
                    token.Keyword.Type,
                    token.Whitespace,
                    token.Name.Attribute,
                    token.Generic,
                    token.Name.Keyword.Tokens,
                    token.Whitespace,
                    token.Operator,
                ),
            ),
            (
                r"{}{_WHITESPACE}{}(\s*)(=)".format(
                    _TYPE, _BASE_ATTRIBUTE, _WHITESPACE=_WHITESPACE
                ),
                lexer.bygroups(
                    token.Keyword.Type,
                    token.Whitespace,
                    token.Name.Attribute,
                    token.Generic,
                    token.Name.Keyword.Tokens,
                    token.Whitespace,
                    token.Operator,
                ),
            ),
        ]
        + _keywords(_usd_builtins.KEYWORDS, token.Keyword.Tokens)
        + _keywords(_usd_builtins.SPECIAL_NAMES, token.Name.Builtins)
        + _keywords(_usd_builtins.COMMON_ATTRIBUTES, token.Name.Attribute)
        + [(r"\b\w+:[\w:]+\b", token.Name.Attribute)]
        + _keywords(_usd_builtins.OPERATORS, token.Operator)  # more attributes
        + [(type_ + r"\[\]", token.Keyword.Type) for type_ in _usd_builtins.TYPES]
        + _keywords(_usd_builtins.TYPES, token.Keyword.Type)
        + _PUNCTUATION
        + [
            ("#.*?$", token.Comment.Single),
            (",", token.Generic),
            (";", token.Generic),  # ";"s are allowed to combine separate metadata lines
            ("=", token.Operator),
            ("[-]?([0-9]*[.])?[0-9]+", token.Number),
            (r"'''(?:.|\n)*?'''", token.String),
            (r'"""(?:.|\n)*?"""', token.String),
            (r"'.*'", token.String),
            (r'".*"', token.String),
            (r"<(\.\./)*([\w/]+|[\w/]+\.\w+[\w:]*)>", token.Name.Namespace),
            (r"@.*@", token.String.Interpol),
            (r'\(.*"[.\\n]*".*\)', token.String.Doc),
            (r"\A#usda .+$", token.Comment.Hashbang),
            (r"\s+", token.Text),
            (r"[\w|_|:|\.]+", token.Generic),
        ],
    }
