"""
pygments.lexers.cangjie
~~~~~~~~~~~~~~~~~~~~~~

Lexer for the Cangjie programming language.

Cangjie is a general-purpose programming language developed by Huawei.

:copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
:license: BSD, see LICENSE for details.
"""

import re
from pygments.lexer import ExtendedRegexLexer, include, bygroups, default
from pygments.token import (
    Comment,
    Operator,
    Keyword,
    Name,
    String,
    Number,
    Punctuation,
    Whitespace,
)

_STRING_ESCAPE = (
    r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8}|[0-7]{1,3})'
)

_ANALYSE_PATTERNS = [
    (re.compile(r"^package\s+[A-Za-z_]", re.MULTILINE), 0.05),
    (re.compile(r"^import\s+[A-Za-z_]", re.MULTILINE), 0.1),
    (re.compile(r"\bfunc\s+[A-Za-z_]"), 0.5),
    (re.compile(r"\binit\s*\("), 0.3),
    (re.compile(r"\blet\s+[A-Za-z_]"), 0.2),
    (re.compile(r"\bvar\s+[A-Za-z_]"), 0.05),
    (re.compile(r"\bconst\s+[A-Za-z_]"), 0.1),
    (re.compile(r"\bstruct\s+[A-Za-z_]"), 0.15),
    (re.compile(r"\binterface\s+[A-Za-z_]"), 0.15),
    (re.compile(r"\bextend\s+[A-Za-z_]"), 0.25),
    (re.compile(r"\bprop\s+[A-Za-z_]"), 0.2),
    (re.compile(r"\benum\s+[A-Za-z_]"), 0.1),
    (re.compile(r"quote\("), 0.4),
    (re.compile(r"case\s+.*=>"), 0.3),
    (re.compile(r"match\s*\("), 0.25),
    (re.compile(r"\$\{"), 0.1),
    (re.compile(r"\$\([A-Za-z_]"), 0.1),
    (
        re.compile(
            r"\b(Int8|Int16|Int32|Int64|IntNative|Float16|Float32|Float64|UInt8|UInt16|UInt32|UInt64)\b"
        ),
        0.15,
    ),
    (re.compile(r"^@[A-Za-z_]", re.MULTILINE), 0.1),
    (re.compile(r"\bmain\s*\(\s*\)"), 0.1),
    (re.compile(r"<:\s*[A-Za-z_]"), 0.25),
    (re.compile(r"Option<"), 0.1),
    (re.compile(r"\boverride\s+func"), 0.2),
    (re.compile(r"\bmut\s+func"), 0.2),
    (re.compile(r"\bsealed\s+(class|interface)"), 0.15),
    (re.compile(r"\bopen\s+(class|interface)"), 0.15),
    (re.compile(r"\babstract\s+class"), 0.05),
]
_MACRO_PACKAGE_PATTERN = re.compile(r"\bmacro\s+package")
_MACRO_FUNC_PATTERN = re.compile(r"\bmacro\s+[A-Za-z_]")


def _raw_string_callback(lexer, match, ctx):
    """Callback for raw strings with variable # delimiter count."""
    start_pos = match.start()
    opening = match.group(0)

    delimiter_count = len(opening) - 1
    quote = opening[-1]
    closing_delimiter = quote + "#" * delimiter_count

    text = ctx.text
    search_pos = match.end()
    end_pos = text.find(closing_delimiter, search_pos, ctx.end)

    if end_pos != -1:
        end_pos += len(closing_delimiter)
        yield start_pos, String, text[start_pos:end_pos]
        ctx.pos = end_pos
    else:
        yield start_pos, String, opening
        ctx.pos = match.end()


__all__ = ["CangjieLexer"]


def _build_keyword_map():
    keyword_map = {}
    keywords = {
        "if",
        "else",
        "for",
        "while",
        "do",
        "match",
        "case",
        "where",
        "try",
        "catch",
        "finally",
        "throw",
        "return",
        "break",
        "continue",
        "in",
        "open",
        "sealed",
        "abstract",
        "override",
        "redef",
        "mut",
        "static",
        "public",
        "private",
        "protected",
        "internal",
        "unsafe",
        "operator",
        "spawn",
        "synchronized",
        "main",
        "quote",
        "this",
        "super",
        "as",
        "is",
        "with",
        "inout",
        "true",
        "false",
        "Some",
        "None",
        "nil",
        "null",
        "perform",
        "resume",
        "throwing",
        "handle",
        "common",
        "specific",
        "features",
    }
    declarations = {
        "package",
        "import",
        "func",
        "let",
        "var",
        "const",
        "class",
        "struct",
        "interface",
        "enum",
        "extend",
        "prop",
        "type",
        "macro",
        "foreign",
        "init",
    }
    types = {
        "Int8",
        "Int16",
        "Int32",
        "Int64",
        "IntNative",
        "UInt8",
        "UInt16",
        "UInt32",
        "UInt64",
        "UIntNative",
        "Float16",
        "Float32",
        "Float64",
        "Bool",
        "Rune",
        "Unit",
        "Nothing",
        "Any",
        "This",
        "Array",
        "VArray",
        "Option",
        "String",
        "Range",
        "Duration",
        "ArrayList",
        "HashMap",
        "HashSet",
        "Exception",
        "IllegalArgumentException",
        "OverflowException",
        "NoneValueException",
        "NegativeArraySizeException",
    }
    builtins = {
        "println",
        "print",
        "sizeof",
        "countof",
        "hash",
        "toString",
        "compareTo",
        "equals",
    }
    for kw in keywords:
        keyword_map[kw] = Keyword
    for kw in declarations:
        keyword_map[kw] = Keyword.Declaration
    for kw in types:
        keyword_map[kw] = Keyword.Type
    for kw in builtins:
        keyword_map[kw] = Name.Builtin
    return keyword_map


_KEYWORD_MAP = _build_keyword_map()


class CangjieLexer(ExtendedRegexLexer):
    name = "Cangjie"
    aliases = ["cangjie"]
    filenames = ["*.cj"]
    mimetypes = ["text/x-cangjie", "application/x-cangjie"]
    url = "https://cangjie-lang.cn/"
    version_added = "2.20"
    priority = 0.1

    tokens = {
        "root": [
            include("whitespace"),
            include("annotations"),
            include("package_import"),
            include("declarations"),
            include("statements"),
            include("expressions"),
            (r"[^\S\n]+", Whitespace),
            (r"\n", Whitespace),
        ],
        "whitespace": [
            (r"[^\S\n]+", Whitespace),
            (r"//[^\n]*", Comment.Single),
            (r"/\*", Comment.Multiline, "multiline_comment"),
            (r"\n", Whitespace),
        ],
        "multiline_comment": [
            (r"/\*", Comment.Multiline, "#push"),
            (r"\*/", Comment.Multiline, "#pop"),
            (r"[^*/]+", Comment.Multiline),
            (r"[*/]", Comment.Multiline),
        ],
        "annotations": [
            (
                r"(@[A-Za-z_][A-Za-z0-9_]*)(\[)",
                bygroups(Name.Decorator, Punctuation),
                "annotation_args",
            ),
            (r"(@[A-Za-z_][A-Za-z0-9_]*)", Name.Decorator),
            (r"(@!)", Name.Decorator),
        ],
        "annotation_common": [
            (r"\\[\[\](){}\\\\]", String.Escape),
            (r"\$\{", String.Interpol, "string_interpolation"),
            (r"\$\(", Name.Variable, "macro_interpolation"),
            (r"\$[A-Za-z0-9_]+", Name.Variable),
            (r"`[A-Za-z_][A-Za-z0-9_]*`", Name.Variable),
        ],
        "annotation_args": [
            include("annotation_common"),
            (r"\]", Punctuation, "#pop"),
            (r"\)", Punctuation, "#pop:2"),
            (r"\s+", Whitespace),
            (r",", Punctuation),
            (r"\[", Punctuation, "#push"),
            (r"\{", Punctuation, "annotation_brace"),
            include("annotation_expr"),
        ],
        "annotation_expr": [
            (r"\(", Punctuation, "annotation_paren"),
            (r"([A-Za-z_][A-Za-z0-9_]*)", Name.Class),
            (r":", Punctuation),
            include("operators"),
            include("literals"),
            include("strings"),
            (r"\s+", Whitespace),
        ],
        "annotation_paren": [
            include("annotation_common"),
            (r"\)", Punctuation, "#pop"),
            (r"\s+", Whitespace),
            (r",", Punctuation),
            (r"\{", Punctuation, "annotation_brace"),
            include("annotation_expr"),
        ],
        "annotation_brace": [
            include("annotation_common"),
            (r"\{", Punctuation, "#push"),
            (r"\}", Punctuation, "#pop"),
            (r"\[", Punctuation, "#push"),
            (r"\]", Punctuation, "#pop"),
            (r",", Punctuation),
            include("annotation_expr"),
        ],
        "package_import": [
            (
                r"(package)(\s+)([A-Za-z_][A-Za-z0-9_.]*)",
                bygroups(Keyword.Namespace, Whitespace, Name.Namespace),
            ),
            (
                r"(public|protected|internal)(\s+)(package)(\s+)([A-Za-z_][A-Za-z0-9_.]*)",
                bygroups(
                    Keyword.Namespace,
                    Whitespace,
                    Keyword.Namespace,
                    Whitespace,
                    Name.Namespace,
                ),
            ),
            (
                r"(import)(\s+)([A-Za-z_][A-Za-z0-9_.]*)(\.\*)?",
                bygroups(Keyword.Namespace, Whitespace, Name.Namespace, Punctuation),
            ),
            (
                r"(import)(\s+)([A-Za-z_][A-Za-z0-9_.]*)\.\{([^}]+)\}",
                bygroups(
                    Keyword.Namespace,
                    Whitespace,
                    Name.Namespace,
                    Punctuation,
                    Name.Namespace,
                ),
            ),
            (
                r"(import)(\s+)([A-Za-z_][A-Za-z0-9_.]*)(\s+)(as)(\s+)([A-Za-z_][A-Za-z0-9_]*)",
                bygroups(
                    Keyword.Namespace,
                    Whitespace,
                    Name.Namespace,
                    Whitespace,
                    Keyword.Namespace,
                    Whitespace,
                    Name.Namespace,
                ),
            ),
            (
                r"(public)(\s+)(import)",
                bygroups(Keyword.Namespace, Whitespace, Keyword.Namespace),
            ),
        ],
        "declarations": [
            (
                r"(func)(\s+)([A-Za-z_][A-Za-z0-9_]*)(\s*)(?=[<(])",
                bygroups(Keyword.Declaration, Whitespace, Name.Function, Whitespace),
                "func_params",
            ),
            (
                r"(func)(\s+)([A-Za-z_][A-Za-z0-9_]*)",
                bygroups(Keyword.Declaration, Whitespace, Name.Function),
            ),
            (
                r"(class|struct|interface|enum|extend)(\s+)([A-Za-z_][A-Za-z0-9_]*)",
                bygroups(Keyword.Declaration, Whitespace, Name.Class),
            ),
            (
                r"(type)(\s+)([A-Za-z_][A-Za-z0-9_]*)",
                bygroups(Keyword.Declaration, Whitespace, Name),
            ),
            (
                r"(prop)(\s+)([A-Za-z_][A-Za-z0-9_]*)",
                bygroups(Keyword.Declaration, Whitespace, Name.Property),
            ),
            (
                r"(macro)(\s+)(package)",
                bygroups(Keyword.Declaration, Whitespace, Keyword.Namespace),
            ),
            (
                r"(macro)(\s+)([A-Za-z_][A-Za-z0-9_]*)",
                bygroups(Keyword.Declaration, Whitespace, Name.Function),
            ),
            (
                r"(foreign)(\s+)(func)(\s+)([A-Za-z_][A-Za-z0-9_]*)",
                bygroups(
                    Keyword.Declaration,
                    Whitespace,
                    Keyword.Declaration,
                    Whitespace,
                    Name.Function,
                ),
            ),
            (
                r"(foreign)(\s+)(func)",
                bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
            ),
            (
                r"(foreign)(\s+)(class|struct)",
                bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
            ),
            (
                r"(sealed)(\s+)(abstract)(\s+)(class)",
                bygroups(
                    Keyword.Declaration,
                    Whitespace,
                    Keyword.Declaration,
                    Whitespace,
                    Keyword.Declaration,
                ),
            ),
            (
                r"(sealed)(\s+)(class)",
                bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
            ),
            (
                r"(sealed)(\s+)(interface)",
                bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
            ),
            (
                r"(abstract)(\s+)(class)",
                bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
            ),
            (
                r"(open)(\s+)(class|interface)",
                bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
            ),
            (
                r"(operator)(\s+)(func)",
                bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
            ),
            (
                r"(let|var|const)(\s+)([A-Za-z_][A-Za-z0-9_]*)",
                bygroups(Keyword.Declaration, Whitespace, Name),
            ),
            (r"(init)(\s*)(?=\()", Keyword.Declaration, "init_params"),
            (r"(~init)(\s*)(?=\()", Keyword.Declaration, "init_params"),
        ],
        "func_params": [
            (r"\(", Punctuation, "params"),
            (r"<", Punctuation, "generic_params"),
            (r":", Punctuation),
            (r"\s+", Whitespace),
            default("root"),
        ],
        "params": [
            include("whitespace"),
            (
                r"([A-Za-z_][A-Za-z0-9_]*)(!)(:)",
                bygroups(Name.Variable, Operator, Punctuation),
            ),
            (r"([A-Za-z_][A-Za-z0-9_]*)(:)", bygroups(Name.Variable, Punctuation)),
            (
                r"(var|let|public|private|protected|internal)(\s+)([A-Za-z_][A-Za-z0-9_]*)(!)(:)",
                bygroups(
                    Keyword.Declaration,
                    Whitespace,
                    Name.Variable,
                    Operator,
                    Punctuation,
                ),
            ),
            (
                r"(var|let|public|private|protected|internal)(\s+)([A-Za-z_][A-Za-z0-9_]*)(:)",
                bygroups(Keyword.Declaration, Whitespace, Name.Variable, Punctuation),
            ),
            (r"\)", Punctuation, "#pop"),
            (r",", Punctuation),
            include("types"),
            include("expressions"),
        ],
        "init_params": [
            (r"\(", Punctuation, "params"),
            (r"\s+", Whitespace),
            default("root"),
        ],
        "generic_params": [
            (r">", Punctuation, "#pop"),
            (r",", Punctuation),
            (r"([A-Za-z_][A-Za-z0-9_]*)", bygroups(Name)),
            (r"(where)", Keyword),
            (r"(<:)", Operator),
            (r"\s+", Whitespace),
            include("types"),
        ],
        "types": [
            (
                r"([A-Za-z_][A-Za-z0-9_]*)(<)",
                bygroups(Name.Class, Punctuation),
                "generic_type_args",
            ),
            (r"([A-Za-z_][A-Za-z0-9_]*)(\?)", bygroups(Name.Class, Operator)),
            (r"\?", Operator),
            (r"([A-Za-z_][A-Za-z0-9_]*)", Name.Class),
        ],
        "generic_type_args": [
            (r">", Punctuation, "#pop"),
            (r",", Punctuation),
            (r"->", Operator),
            (r"(\$[A-Za-z0-9_]+)", Number),
            (r"(\$\()", bygroups(Name.Variable), "macro_interpolation"),
            (r"\(", Punctuation, "tuple_type"),
            (r"(\d+)", Number.Integer),
            include("types"),
            (r"\s+", Whitespace),
        ],
        "tuple_type": [
            (r"\)", Punctuation, "#pop"),
            (r"->", Operator),
            (r",", Punctuation),
            (r":", Punctuation),
            (r"\(", Punctuation, "#push"),
            include("types"),
            (r"\s+", Whitespace),
        ],
        "statements": [
            (r"(if)(?=\s*\()", Keyword, "condition"),
            (r"(else)\b", Keyword),
            (r"(while)(?=\s*\()", Keyword, "condition"),
            (r"(do)\b", Keyword),
            (r"(for)(?=\s*\()", Keyword, "for_loop"),
            (r"(match)(?=\s*\()", Keyword, "match_expr"),
            (r"(match)(?=\s*\{)", Keyword, "match_block"),
            (r"(case)(?=\s+)", Keyword, "case_pattern"),
            (r"(try)(?=\s*\{)", Keyword),
            (r"(try)(?=\s*\()", Keyword, "try_resource"),
            (r"(catch)(?=\s*\()", Keyword, "catch_param"),
            (r"(finally)\b", Keyword),
            (r"(throwing)\b", Keyword),
            (r"(throw)\b", Keyword),
            (r"(return)\b", Keyword),
            (r"(break|continue)\b", Keyword),
            (r"(unsafe)(?=\s*\{)", Keyword),
            (r"(spawn)\b", Keyword),
            (r"(synchronized)\b", Keyword),
            (r"(handle)\b", Keyword),
            (r"(perform)\b", Keyword),
            (r"(resume)\b", Keyword),
            (r"(in)\b", Keyword),
            (r"(where)\b", Keyword),
            (r"(mut)\b", Keyword),
            (r"(static)\b", Keyword),
            (r"(override)\b", Keyword),
            (r"(redef)\b", Keyword),
            (r"(abstract)\b", Keyword),
            (r"(with)\b", Keyword),
            (
                r"(const)(\s+)(func)",
                bygroups(Keyword.Declaration, Whitespace, Keyword.Declaration),
            ),
        ],
        "condition": [
            (r"\(", Punctuation),
            (r"\)", Punctuation, "#pop"),
            (r"(let)", Keyword.Declaration),
            include("expressions"),
        ],
        "for_loop": [
            (r"\)", Punctuation, "#pop"),
            (r"(let)", Keyword.Declaration),
            (r"(in)", Keyword),
            (r"(where)", Keyword),
            (r"(\.\.)(=)?", bygroups(Operator, Operator)),
            (r"\s+", Whitespace),
            include("expressions"),
        ],
        "match_expr": [
            (r"\)", Punctuation, "#pop"),
            (r"\{", Punctuation, "match_cases"),
            include("expressions"),
        ],
        "match_block": [
            (r"\{", Punctuation, "#push"),
            (r"\}", Punctuation, "#pop"),
            include("case_patterns"),
        ],
        "match_cases": [
            (r"\}", Punctuation, "#pop"),
            include("case_patterns"),
        ],
        "case_patterns": [
            (r"(case)(?=\s+)", Keyword),
            (r"(\|)", Punctuation),
            (r"(=>)", Operator),
            (r"(where)(?=\s+)", Keyword),
            (r"(\.\.\.)", Operator),
            (r"(_)", Name.Variable),
            (r"(Some|None)", Keyword.Type),
            include("expressions"),
            (r"(\n)", Whitespace),
        ],
        "case_pattern": [
            (r"(=>)", Operator, "#pop"),
            (r"(where)", Keyword),
            (r"(\|)", Punctuation),
            (r"(\.\.\.)", Operator),
            (r"(_)", Name.Variable),
            (r"(Some|None)", Keyword.Type),
            include("expressions"),
        ],
        "try_resource": [
            (r"\)", Punctuation, "#pop"),
            (r"(=)", Operator),
            (r"([A-Za-z_][A-Za-z0-9_]*)", Name.Variable),
            include("expressions"),
        ],
        "catch_param": [
            (r"\(", Punctuation),
            (r"\)", Punctuation, "#pop"),
            (r"\s+", Whitespace),
            (r"(_)", Name.Variable),
            (r"(\$[A-Za-z0-9_]+)", Name.Variable),
            (r"(e|ex|err|exception)", Name.Variable),
            (r"([A-Za-z_][A-Za-z0-9_]*)", Name.Variable),
            (r"(:)", Punctuation),
            include("types"),
            (r"(\|)", Punctuation),
        ],
        "quote_body": [
            (r"\\\(", String.Escape),
            (r"\\\)", String.Escape),
            (r"\\", String.Escape),
            (r"\$\{", String.Interpol, "string_interpolation"),
            (r"\$\(", Name.Variable, "macro_interpolation"),
            (r"(\$[A-Za-z0-9_]+)", Name.Variable),
            (r"\{", Punctuation, "#push"),
            (r"\}", Punctuation, "#pop"),
            (r"\(", Punctuation, "#push"),
            (r"\)", Punctuation, "#pop"),
            include("expressions"),
        ],
        "expressions": [
            (r"\s+", Whitespace),
            (r"quote\(", Keyword, "quote_body"),
            (r"//[^\n]*", Comment.Single),
            (r"/\*", Comment.Multiline, "multiline_comment"),
            include("annotations"),
            include("literals"),
            include("operators"),
            (r'J"', String, "jstring"),
            include("types"),
            include("strings"),
            include("identifiers"),
            (r"\{", Punctuation, "block"),
            (r"\(", Punctuation, "paren_expr"),
            (r"\[", Punctuation, "index_expr"),
            (r"\}", Punctuation, "#pop"),
            (r"\)", Punctuation, "#pop"),
            (r"\]", Punctuation, "#pop"),
            (r"(,)", Punctuation),
            (r"(;)", Punctuation),
            (r"(:)", Punctuation),
            (r"(<:)", Operator),
        ],
        "block": [
            (r"\{", Punctuation, "#push"),
            (r"\}", Punctuation, "#pop"),
            include("root"),
        ],
        "paren_expr": [
            (r"\(", Punctuation, "#push"),
            (r"\)", Punctuation, "#pop"),
            include("expressions"),
        ],
        "index_expr": [
            (r"\[", Punctuation, "#push"),
            (r"\]", Punctuation, "#pop"),
            (r"(\.\.)(=)?", bygroups(Operator, Operator)),
            include("expressions"),
        ],
        "literals": [
            (r"(true|false)", Keyword.Constant),
            (
                r"(\d+)(i8|i16|i32|i64|u8|u16|u32|u64|f16|f32|f64)",
                bygroups(Number.Integer, Number),
            ),
            (
                r"(0x[0-9a-fA-F]+)(i8|i16|i32|i64|u8|u16|u32|u64)?",
                bygroups(Number.Hex, Number),
            ),
            (
                r"(0b[01]+)(i8|i16|i32|i64|u8|u16|u32|u64)?",
                bygroups(Number.Bin, Number),
            ),
            (r"(0o[0-7]+)", Number.Oct),
            (
                r"(\d+)(\.\d+)?([eE][+-]?\d+)?(f16|f32|f64)?",
                bygroups(Number.Integer, Number.Float, Number.Float, Number.Float),
            ),
            (
                r"(\.\d+)([eE][+-]?\d+)?(f16|f32|f64)?",
                bygroups(Number.Float, Number.Float, Number.Float),
            ),
            (r"(\d+)", Number.Integer),
            (r"(\(\))", Keyword.Constant),
            (r"(b'([^\\']|\\.)*')", String.Char),
            (r"(r'([^\\']|\\.)*')", String.Char),
            (r"'([^\\']|\\.)*'", String.Char),
        ],
        "strings": [
            (r'"""', String, "triple_string"),
            (r"'''", String, "triple_single_string"),
            (r'"', String, "string"),
            (r"(#+['\"])", _raw_string_callback),
        ],
        "triple_string": [
            (r'"""', String, "#pop"),
            (r"\$\{", String.Interpol, "string_interpolation"),
            (r'[^$"\\]+', String),
            (r'"', String),
            (r"\$", String),
            (r"\\", String),
        ],
        "triple_single_string": [
            (r"'''", String, "#pop"),
            (r"\$\{", String.Interpol, "string_interpolation"),
            (r"[^$'\\]+", String),
            (r"'", String),
            (r"\$", String),
            (r"\\", String),
        ],
        "string": [
            (r'"', String, "#pop"),
            (r"\$\{", String.Interpol, "string_interpolation"),
            (_STRING_ESCAPE, String.Escape),
            (r'[^\$"\\]+', String),
            (r"\$", String),
            (r"\\", String),
        ],
        "string_interpolation": [
            (r"\}", String.Interpol, "#pop"),
            include("expressions"),
        ],
        "jstring": [
            (r'"', String, "#pop"),
            (r'[^\n"\\]+', String),
            (_STRING_ESCAPE, String.Escape),
            (r"\\", String),
        ],
        "operators": [
            (r"\+\+|--", Operator),
            (r"\.\.\.|\.\.=|\.\.", Operator),
            (r"\?\?|\?\.|\?\[|\?\(|\?\{", Operator),
            (r"\|>|~>", Operator),
            (r"<:", Operator),
            (r"::", Operator),
            (r"!in\b", Operator),
            (r"->", Operator),
            (r"=>", Operator),
            (r"<-", Operator),
            (r"\*\*=|\*=|/=|%=|\+=|-=|<<=|>>=|&=|\|=|\^=|&&=|\|\|=", Operator),
            (r"==|!=|<=|>=|<|>", Operator),
            (r"\|\||&&|!", Operator),
            (r"\+|-|\*|/|%|\*\*", Operator),
            (r"<<|>>|&|\||\^|~", Operator),
            (r"=", Operator),
            (r"\?", Operator),
            (r"\.", Operator),
        ],
        "identifiers": [
            (r"(`[A-Za-z_][A-Za-z0-9_]*`)", Name.Variable),
            (
                r"([A-Za-z_][A-Za-z0-9_]*)(!)(:)",
                bygroups(Name.Variable, Operator, Punctuation),
            ),
            (r"([A-Za-z_][A-Za-z0-9_]*)(!)", bygroups(Name.Variable, Operator)),
            (r"(\$[A-Za-z0-9_]+)", Name.Variable),
            (r"(\$\()", bygroups(Name.Variable), "macro_interpolation"),
            (r"(this|super)", Name.Builtin),
            (r"([A-Za-z_][A-Za-z0-9_]*)(\()", bygroups(Name.Function, Punctuation)),
            (r"([A-Za-z_][A-Za-z0-9_]*)", Name),
        ],
        "macro_interpolation": [
            (r"\)", Punctuation, "#pop"),
            (r"\[", Punctuation, "index_expr"),
            (r"\(", Punctuation, "#push"),
            include("expressions"),
        ],
    }

    def get_tokens_unprocessed(self, text=None, context=None):
        for index, token, value in ExtendedRegexLexer.get_tokens_unprocessed(
            self, text, context
        ):
            if token in Name:
                new_token = _KEYWORD_MAP.get(value)
                if new_token:
                    token = new_token
            yield index, token, value

    @staticmethod
    def analyse_text(text):
        score = 0.0
        for pattern, weight in _ANALYSE_PATTERNS:
            if pattern.search(text):
                score += weight
        if _MACRO_PACKAGE_PATTERN.search(text):
            score += 0.5
        elif _MACRO_FUNC_PATTERN.search(text):
            score += 0.3
        if '"""' in text:
            score += 0.05
        return min(score, 1.0)
