from pygments.lexer import (
    include,
    RegexLexer,
    words,
)
from pygments.token import (
    Comment,
    Keyword,
    Name,
    Number,
    Operator,
    Punctuation,
    String,
)


number_re = r"[-+]?\d+(\.\d+)?"
comment_re = r"#.*?\n"
variable_re = r"\b(\$[a-zA-Z0-9_\-]+)\b"
entity_class_re = r"[a-zA-Z_][-\w]*"
single_quote_re = r"'"
double_quote_re = r'"'

esc_char_re = r'[bdefnrstv\'"\\]'
esc_octal_re = r"[0-7][0-7]?[0-7]?"
esc_hex_re = r"(?:x[0-9a-fA-F]{2}|x\{[0-9a-fA-F]+\})"
esc_ctrl_re = r"\^[a-zA-Z]"
escape_re = (
    r"(?:\\(?:"
    + esc_char_re
    + r"|"
    + esc_octal_re
    + r"|"
    + esc_hex_re
    + r"|"
    + esc_ctrl_re
    + r"))"
)


TOKENS = {
    "builtin": (
        "thing",
        "entity",
        "attribute",
        "relation",
        "role",
    ),
    "types": (
        "string",
        "long",
        "double",
        "boolean",
        "datetime",
        "true",
        "false",
    ),
    "keywords": (
        "define",
        "undefine",
        "match",
        "get",
        "insert",
        "delete",
        "offset",
        "limit",
        "group",
        "sort",
        "asc",
        "desc",
        "sub",
        "abstract",
        "as",
        "iid",
        "type",
        "isa",
        "isa!",
        "sub",
        "sub!",
        "owns",
        "has",
        "plays",
        "relates",
        "value",
        "regex",
        "rule",
        "when",
        "then",
    ),
    "word_operators": (
        "or",
        "not",
        "like",
        "is",
    ),
    "comparator": (">=", "<=", ">", "<"),
    "cond": (
        "when",
        "then",
    ),
}


class TypeQLLexer(RegexLexer):
    name = "TypeQLLexer"
    aliases = ["typeql", "tql"]
    filenames = ["*.tql"]
    tokens = {
        "root": [
            include("keywords"),
            (double_quote_re, String.Double, "string-double-quoted"),
            (single_quote_re, String.Single, "string-single-quoted"),
            include("escaped-char"),
            include("comments"),
            include("variables"),
            (
                words(
                    TOKENS["builtin"],
                    prefix=r"\b",
                    suffix=r"\b",
                ),
                Name.Builtin,
            ),
            (
                words(
                    TOKENS["types"],
                    prefix=r"\b",
                    suffix=r"\b",
                ),
                Keyword.Type,
            ),
            (entity_class_re, Name.Class),
            (number_re, Number),
        ],
        "variables": [
            (variable_re, Name.Variable.Instance),
        ],
        "keywords": [
            (
                words(
                    TOKENS["builtin"],
                    prefix=r"\b",
                    suffix=r"\b",
                ),
                Name.Entity,
            ),
            (
                words(TOKENS["cond"], prefix=r"\b", suffix=r"\b"),
                Keyword.Namespace,
            ),
            (
                words(
                    TOKENS["keywords"],
                    prefix=r"\b",
                    suffix=r"\b",
                ),
                Keyword,
            ),
            (words(TOKENS["comparator"]), Operator),
            (
                words(
                    TOKENS["word_operators"],
                    prefix=r"\b",
                    suffix=r"\b",
                ),
                Operator.Word,
            ),
            (r":", String.Delimiter),
            (r";", Punctuation),
        ],
        "string-double-quoted": [
            include("escaped-char"),
            (double_quote_re, String.Double, "#pop"),
            (r'[^"\\]+', String.Double),
        ],
        "string-single-quoted": [
            include("escaped-char"),
            (single_quote_re, String.Single, "#pop"),
            (r"[^\'\\]+", String.Single),
        ],
        "escaped-char": [
            (escape_re, String.Escape),
        ],
        "comments": [(comment_re, Comment.Single)],
    }
