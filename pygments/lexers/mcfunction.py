"""
    pygments.lexers.mcfunction
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for MCFunction and related languages.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, default, include, using
from pygments.token import (Comment, Error, Keyword, Literal, Name, Number,
                            Operator, Punctuation, String, Text, Token,
                            Whitespace)


__all__ = ['SNBTLexer', 'MCFunctionLexer']


class SNBTLexer(RegexLexer):
    """Lexer for stringified NBT, a data format used in Minecraft


    .. versionadded:: ?
    """

    name = "SNBT"
    url = ""
    aliases = ["snbt"]
    filenames = ["*.snbt"]
    mimetype = ["text/snbt"]

    tokens = {
        "root": [
            # We only look for the open bracket here since square bracket
            #  is only valid in NBT pathing (which is a mcfunction idea).
            (r"(?=\{)", Punctuation, "compound"),
        ],

        "whitespace": [
            (r"\s+", Whitespace),
        ],

        "operators": [
            (r"[,:;]", Punctuation),
        ],

        "literals": [
            (r"(true|false)", Keyword.Constant),
            (r"(-?\d*\.\d+[fFdD]?)", Number.Float),
            (r"(-?\d+[bBsSlLfFdD]?)", Number.Integer),
            (r"(-?\d+[eE]-?\d+)", Number.Float),
            # Seperate states for both types of strings so they don't entangle
            (r'"', String.Double, "literals.string_double"),
            (r"'", String.Single, "literals.string_single"),
        ],
        "literals.string_double": [
            (r"\\.", String.Escape),
            (r'[^\\"\n]+', String.Double),
            (r'"', String.Double, "#pop"),
            (r"[$\n]", Error, "#pop"),
        ],
        "literals.string_single": [
            (r"\\.", String.Escape),
            (r"[^\\'\n]+", String.Single),
            (r"'", String.Single, "#pop"),
            (r"[$\n]", Error, "#pop"),
        ],

        "compound": [
            # this handles the unquoted snbt keys
            #  note: stringified keys still work
            (r"[A-z_]+", Name.Attribute),
            include("operators"),
            include("whitespace"),
            include("literals"),
            (r"\{", Punctuation, "#push"),
            (r"\[", Punctuation, "list"),
            (r"\}", Punctuation, "#pop"),
        ],

        "list": [
            (r"(?<=\[)[BIL](?:\;)", Keyword.Type),
            (r"[A-z_]+", Name.Attribute),
            include("literals"),
            include("operators"),
            include("whitespace"),
            (r"\{", Punctuation, "#push"),
            (r"\{", Punctuation, "compound"),
            (r"\]", Punctuation, "#pop"),
        ],
    }


class MCFunctionLexer(RegexLexer):
    """Lexer for the mcfunction scripting language used in Minecraft

    Modelled somewhat after the Github mcfunction grammar:
    - "https://github.com/Arcensoth/language-mcfunction

    .. versionadded:: ?
    """

    name = "MCFunction"
    url = "https://minecraft.fandom.com/wiki/Commands"
    aliases = ["mcfunction", "mcf"]
    filenames = ["*.mcfunction"]
    mimetype = ["text/mcfunction"]

    # Used to denotate the start of a block comment, borrowed from Github's mcfunction
    _block_comment_prefix = "[>!*$]"

    tokens = {
        "root": [
            include("names"),
            include("nbt"),
            include("comments"),
            include("literals"),
            include("whitespace"),
            include("operators"),
            include("delimiters"),
            include("selectors"),
        ],

        "names": [
            # The start of a command (either beginning of line OR after the run keyword)
            #  We don't encode a list of keywords since mods, plugins, or even pre-processors
            #  may add new commands, so we have a 'close-enough' regex which catches them.
            (r"(?=^\s*)([a-z]+)", Name.Builtin),
            (r"(?<=run\s)([a-z]+)", Name.Builtin),
            # UUID
            (
                r"\b([0-9a-fA-F]+(?:(-)[0-9a-fA-F]+){4})\b",
                Name.Variable,
            ),
            include("resource-name"),
            # normal command names and scoreboards
            #  there's no way to know the differences unfortuntely
            (r"([A-z_][A-z0-9_]*)", Keyword.Constant, "names.maybe-nbt"),
            (r"[#%$][A-z0-9_]+", Name.Variable.Magic, "names.fakeplayer"),
        ],
        "names.fakeplayer": [(r"[\.A-z0-9_]+", Name.Variable.Magic), default("#pop")],
        "names.maybe-nbt": [
            (r"\[", Punctuation, "nbt.list"),
            default("#pop"),
        ],

        "resource-name": [
            (
                r"(#?)([a-z_][a-z_\.\-]*)(\:)([a-z0-9_\.\-\/]+)",
                Name.Function,
            )
        ],

        "whitespace": [
            (r"\s+", Whitespace),
        ],

        "comments": [
            (
                rf"^\s*(#\s*{_block_comment_prefix})",
                Comment.Multiline,
                (
                    "comments.block",
                    "comments.block.emphasized",
                ),
            ),
            (r"#.*$", Comment.Single),
        ],
        "comments.block": [
            (rf"^\s*#\s*{_block_comment_prefix}", Comment.Multiline, "comments.block.emphasized"),
            (r"^\s*#", Comment.Multiline, "comments.block.normal"),
            (r"(?!\n\s+#)", Text, "#pop")
        ],
        "comments.block.normal": [
            include("comments.block.param"),
            (r"\S+?", Comment.Multiline),
            (r"\n", Text, "#pop"),
            include("whitespace"),
        ],
        "comments.block.emphasized": [
            include("comments.block.param"),
            (r"\S+", String.Doc),
            (r"\n", Text, "#pop"),
            include("whitespace"),
        ],
        "comments.block.param": [
            (r"@\S+", Name.Decorator),
        ],

        "operators": [
            (r"[-~%^?!+*<>\\/|&=.]+", Operator),
        ],

        "literals": [
            (r"\.\.", Literal),
            (r"(true|false)", Keyword.Pseudo),
            (r"[a-z_]+", Name.Variable.Class),
            (r"[0-7]b", Number.Byte),
            (r"[+-]?\d*\.?\d+([eE]?[+-]?\d+)?[df]?\b", Number.Float),
            (r"[+-]?\d+\b", Number.Integer),
            (r'"', String.Double, "literals.string-double"),
            (r"'", String.Single, "literals.string-single"),
        ],
        "literals.string-double": [
            (r"\\.", String.Escape),
            (r'[^\\"\n]+', String.Double),
            (r'"', String.Double, "#pop"),
            (r"[$\n]", Error, "#pop"),
        ],
        "literals.string-single": [
            (r"\\.", String.Escape),
            (r"[^\\'\n]+", String.Single),
            (r"'", String.Single, "#pop"),
            (r"[$\n]", Error, "#pop"),
        ],

        "selectors": [
            (r"@[a-z](?=\[)", Name.Variable, "selectors-inside"),
            (r"@[a-z]", Name.Variable),
        ],
        "selectors-inside": [
            (r"\[", Punctuation),
            # this checks whether eventually there is an '=' sign
            #  otherwise, we assume NBT
            (r"[a-z_]+(?=\=\{\s*[a-z:_\/]+\=)", Name.Tag, "selectors-inside-tag-no-nbt"),
            (r"[a-z_]+(?=\=)", Name.Tag, "selectors-inside-tag"),
            (r"[,\.]", Punctuation),
            include("whitespace"),
            include("literals"),
            (r"\]", Punctuation, "#pop"),
        ],
        "selectors-inside-tag": [
            (r"[!=]", Operator),
            include("nbt"),
            include("resource-name"),
            include("whitespace"),
            include("literals"),
            (r"", Token, "#pop"),
        ],
        "selectors-inside-tag-no-nbt": [
            (r"[!=,]", Operator),
            include("resource-name"),
            include("whitespace"),
            include("literals"),
            (r"\{", Token, "#push"),
            (r"\}", Token, "#pop"),
            default("#pop"),
        ],

        # We have to push and pop on these brackets so that we don't
        #  accidentally consume too many tokens which aren't inside the SNBT.
        #  If we don't do this logic, we may accidentally pass too many tokens
        #  to the SNBTLexer.
        "nbt": [
            (r"\{", Punctuation, "nbt.compound"),
        ],
        "nbt.list": [
            (r"\[", Punctuation, "#push"),
            (r"\]", Punctuation, "#pop"),
            (r"[^\[\]]+", using(SNBTLexer, state="list")),
        ],
        "nbt.compound": [
            (r"\{", Punctuation, "#push"),
            (r"\}", Punctuation, "#pop"),
            (r"[^\{\}]+", using(SNBTLexer, state="compound")),
        ],

        "delimiters": [
            (r"[,;(){}\[\]:]", Punctuation),
        ],
    }
