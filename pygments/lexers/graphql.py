"""
    pygments.lexers.graphql
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for GraphQL, an open-source data query and manipulation
    language for APIs.

    More information:
    https://graphql.org/

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words, include, bygroups
from pygments.token import Comment, Keyword, Name, Number, Punctuation, String, Whitespace


__all__ = ["GraphQLLexer"]

OPERATION_TYPES = ("query", "mutation", "subscription")
BUILTIN_TYPES = ("Int", "Float", "String", "Boolean", "ID")
BOOLEAN_VALUES = ("true", "false", "null")
KEYWORDS = (
    "type",
    "schema",
    "extend",
    "enum",
    "scalar",
    "implements",
    "interface",
    "union",
    "input",
    "directive",
    "QUERY",
    "MUTATION",
    "SUBSCRIPTION",
    "FIELD",
    "FRAGMENT_DEFINITION",
    "FRAGMENT_SPREAD",
    "INLINE_FRAGMENT",
    "SCHEMA",
    "SCALAR",
    "OBJECT",
    "FIELD_DEFINITION",
    "ARGUMENT_DEFINITION",
    "INTERFACE",
    "UNION",
    "ENUM",
    "ENUM_VALUE",
    "INPUT_OBJECT",
    "INPUT_FIELD_DEFINITION",
)


class GraphQLLexer(RegexLexer):

    name = "GraphQL"
    aliases = ["graphql"]
    filenames = ["*.graphql"]

    tokens = {
        "ignored_tokens": [
            (r"\s+", Whitespace),  # Whitespaces
            (r"#.*$", Comment),
            (",", Punctuation),  # Insignificant commas
        ],
        "value": [
            include("ignored_tokens"),
            include("value_single0"),
        ],
        # How to avoid state duplication?
        "value_single0": [
            include("ignored_tokens"),
            (r"-?\d+(?![.eE])", Number.Integer, "#pop"),
            (
                r"-?\d+(\.\d+)?([eE][+-]?\d+)?",
                Number.Float,
                "#pop",
            ),
            (r'"', String, "string0"),
            (words(BOOLEAN_VALUES, suffix=r"\b"), Name.Builtin, "#pop"),
            (r"\$[a-zA-Z_]\w*", Name.Variable, "#pop"),
            (r"[a-zA-Z_]\w*", Name.Constant, "#pop"),
            (r"\[", Punctuation, "list_value0"),
            (r"\{", Punctuation, "object_value0"),
        ],
        "value_single": [
            include("ignored_tokens"),
            (r"-?\d+(?![.eE])", Number.Integer),
            (
                r"-?\d+(\.\d+)?([eE][+-]?\d+)?",
                Number.Float,
            ),
            (r'"', String, "string"),
            (words(BOOLEAN_VALUES, suffix=r"\b"), Name.Builtin),
            (r"\$[a-zA-Z_]\w*", Name.Variable),
            (r"[a-zA-Z_]\w*", Name.Constant),
            (r"\[", Punctuation, "list_value"),
            (r"\{", Punctuation, "object_value"),
        ],
        "list_value0": [
            (r"\]", Punctuation, "#pop:2"),
            include("list_value"),
        ],
        "list_value": [
            include("ignored_tokens"),
            include("value_single"),
            ("]", Punctuation, "#pop"),
        ],
        "object_value0": [
            (r"\}", Punctuation, "#pop:2"),
            include("object_value"),
        ],
        "object_value": [
            include("ignored_tokens"),
            (r"[a-zA-Z_]\w*", Name),
            (r":", Punctuation, "value"),
            (r"\}", Punctuation, "#pop"),
        ],
        "string": [
            include("in_string"),
            (r'"', String, "#pop"),
        ],
        "string0": [
            include("in_string"),
            (r'"', String, "#pop:2"),
        ],
        "in_string": [
            (r'\\(["\\/bfnrt]|u[a-fA-F0-9]{4})', String.Escape),
            (r'[^\\"\n]+', String),  # all other characters
        ],
        "root": [
            include("ignored_tokens"),
            (words(OPERATION_TYPES, suffix=r"\b"), Keyword, "operation"),
            (words(KEYWORDS, suffix=r"\b"), Keyword),
            (r"\{", Punctuation, "selection_set"),
            (r"fragment\b", Keyword, "fragment_definition"),
        ],
        "operation": [
            include("ignored_tokens"),
            (r"[a-zA-Z_]\w*", Name.Function),
            (r"\(", Punctuation, "variable_definition"),
            (r"\{", Punctuation, "selection_set0"),
        ],
        # Variables definition
        "variable_definition0": [
            (r"\)", Punctuation, "#pop:2"),
            include("variable_definition"),
        ],
        "variable_definition": [
            include("ignored_tokens"),
            (r"\$[a-zA-Z_]\w*", Name.Variable),
            (r"[\]!]", Punctuation),
            (r":", Punctuation, "type"),
            (r"=", Punctuation, "value"),
            (r"\)", Punctuation, "#pop"),
        ],
        "type": [
            include("ignored_tokens"),
            (r"\[", Punctuation),
            (words(BUILTIN_TYPES, suffix=r"\b"), Name.Builtin, "#pop"),
            (r"[a-zA-Z_]\w*", Name.Class, "#pop"),
        ],
        # Selection set
        "selection_set0": [
            (r"\}", Punctuation, "#pop:2"),
            include("selection_set"),
        ],
        "selection_set": [
            include("ignored_tokens"),
            (r"([a-zA-Z_]\w*)(\s*)(:)", bygroups(Name.Label, Whitespace, Punctuation)),
            (r"[a-zA-Z_]\w*", Name),  # Field
            (
                r"(\.\.\.)(\s+)(on)\b",
                bygroups(Punctuation, Whitespace, Keyword),
                "inline_fragment",
            ),
            (r"\.\.\.", Punctuation, "fragment_spread"),
            (r"\(", Punctuation, "arguments"),
            (r"@[a-zA-Z_]\w*", Name.Decorator, "directive"),
            (r"\{", Punctuation, "selection_set"),
            (r"\}", Punctuation, "#pop"),
        ],
        "directive": [
            include("ignored_tokens"),
            (r"\(", Punctuation, "arguments2"),
        ],
        "arguments": [
            include("ignored_tokens"),
            (r"[a-zA-Z_]\w*", Name),
            (r":", Punctuation, "value"),
            (r"\)", Punctuation, "#pop"),
        ],
        "arguments2": [
            (r"\)", Punctuation, "#pop:2"),
            include("arguments"),
        ],
        # Fragments
        "fragment_definition": [
            include("ignored_tokens"),
            (r"[\]!]", Punctuation),  # For NamedType
            (r"on\b", Keyword, "type"),
            (r"[a-zA-Z_]\w*", Name.Function),
            (r"@[a-zA-Z_]\w*", Name.Decorator, "directive"),
            (r"\{", Punctuation, "selection_set0"),
        ],
        "fragment_spread": [
            include("ignored_tokens"),
            (r"@[a-zA-Z_]\w*", Name.Decorator, "directive"),
            (r"[a-zA-Z_]\w*", Name, "#pop"),  # Fragment name
        ],
        "inline_fragment": [
            include("ignored_tokens"),
            (r"[a-zA-Z_]\w*", Name.Class),  # Type condition
            (r"@[a-zA-Z_]\w*", Name.Decorator, "directive"),
            (r"\{", Punctuation, "selection_set0"),
        ],
    }
