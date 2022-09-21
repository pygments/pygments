"""
    pygments.lexers.wowtoc
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for World of Warcraft TOC files, which describe game addon metadata.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups
from pygments.token import Comment, Name, Text, Punctuation, String, Keyword

__all__ = ["WoWTocLexer"]


def _create_tag_line_token(inner_pattern, inner_token, ignore_case=False):
    # this function template-izes the tag line for a specific type of tag, which will
    # have a different pattern and different token. otherwise, everything about a tag
    # line is the same
    return (
        (r"(?i)" if ignore_case else r"")
        + r"^(##)( *)"
        + inner_pattern
        + r"( *)(:)( *)(.*?)( *)$",
        bygroups(
            Keyword.Declaration,
            Text.Whitespace,
            inner_token,
            Text.Whitespace,
            Punctuation,
            Text.Whitespace,
            String,
            Text.Whitespace,
        ),
    )


class WoWTocLexer(RegexLexer):
    """
    Lexer for World of Warcraft TOC files.

    .. versionadded:: 2.13
    """

    name = "World of Warcraft TOC"
    aliases = ["wowtoc"]
    filenames = ["*.toc"]

    tokens = {
        "root": [
            # official localized tags, Notes and Title
            # (normal part is insensitive, locale part is sensitive)
            _create_tag_line_token(
                r"((?:[nN][oO][tT][eE][sS]|[tT][iI][tT][lL][eE])-(?:ptBR|zhCN|enCN|frFR|deDE|itIT|esMX|ptPT|koKR|ruRU|esES|zhTW|enTW|enGB|enUS))",
                Name.Builtin,
            ),
            # other official tags
            _create_tag_line_token(
                r"(Interface|Title|Notes|RequiredDeps|Dep[^: ]*|OptionalDeps|LoadOnDemand|LoadWith|LoadManagers|SavedVariablesPerCharacter|SavedVariables|DefaultState|Secure|Author|Version)",
                Name.Builtin,
                ignore_case=True,
            ),
            # user-defined tags
            _create_tag_line_token(
                r"(X-[^: ]*)",
                Name.Variable,
                ignore_case=True,
            ),
            # non-conforming tags, but still valid
            _create_tag_line_token(
                r"([^: ]*)",
                Name.Other,
            ),
            
            # Comments
            (r"^#.*$", Comment),
            
            # Addon Files
            (r"^.+$", Name),
        ]
    }
