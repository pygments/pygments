"""
    pygments.lexers.lateralus
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Lateralus programming language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, words, include
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Text, Whitespace

__all__ = ['LateralusLexer']


class LateralusLexer(RegexLexer):
    """
    Lexer for the Lateralus programming language, a statically typed,
    pipeline-oriented language with algebraic data types, pattern matching,
    and first-class effect capabilities.
    """

    name = 'Lateralus'
    url = 'https://lateralus.dev'
    aliases = ['lateralus', 'ltl']
    filenames = ['*.ltl']
    mimetypes = ['text/x-lateralus']
    version_added = '2.21'

    keywords = (
        'fn', 'let', 'mut', 'match', 'if', 'else', 'elif', 'while', 'for',
        'in', 'return', 'break', 'continue', 'import', 'export', 'module',
        'pub', 'priv', 'struct', 'enum', 'impl', 'trait', 'where', 'type',
        'const', 'static', 'async', 'await', 'spawn', 'guard', 'defer',
        'use', 'as', 'self', 'Self', 'super', 'yield', 'do',
    )

    builtin_types = (
        'int', 'i8', 'i16', 'i32', 'i64', 'i128',
        'uint', 'u8', 'u16', 'u32', 'u64', 'u128',
        'float', 'f32', 'f64',
        'bool', 'str', 'char', 'bytes', 'any', 'never',
        'list', 'map', 'set', 'tuple', 'Option', 'Result',
        'Some', 'None', 'Ok', 'Err',
    )

    builtin_funcs = (
        'print', 'println', 'eprint', 'eprintln', 'format', 'panic',
        'assert', 'assert_eq', 'todo', 'unimplemented', 'unreachable',
        'len', 'range', 'map', 'filter', 'reduce', 'fold', 'zip',
        'enumerate', 'sort', 'sorted', 'reverse', 'sum', 'min', 'max',
    )

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            # Line comments
            (r'//[^\n]*', Comment.Single),
            # Doc comments
            (r'///[^\n]*', Comment.Special),
            # Nested block comments
            (r'/\*', Comment.Multiline, 'block_comment'),
            # Attribute decorators: @memo, @doc("..."), @foreign("c")
            (r'@[A-Za-z_][A-Za-z0-9_]*', Name.Decorator),
            # Capability annotations: #[caps(io, net)]
            (r'#\[', Name.Decorator, 'attribute'),
            # Keywords
            (words(keywords, suffix=r'\b'), Keyword),
            # Builtin types
            (words(builtin_types, suffix=r'\b'), Keyword.Type),
            # Boolean and null-ish literals
            (words(('true', 'false', 'null'), suffix=r'\b'), Keyword.Constant),
            # Builtin functions (followed by a call)
            (words(builtin_funcs, suffix=r'\b'), Name.Builtin),
            # Function definitions
            (r'(fn)(\s+)([A-Za-z_][A-Za-z0-9_]*)',
             bygroups(Keyword, Whitespace, Name.Function)),
            # Type / struct / enum / trait definitions
            (r'(struct|enum|trait|type|impl)(\s+)([A-Z][A-Za-z0-9_]*)',
             bygroups(Keyword, Whitespace, Name.Class)),
            # Module path: `foo::bar::baz`
            (r'([A-Za-z_][A-Za-z0-9_]*)(::)',
             bygroups(Name.Namespace, Punctuation)),
            # ADT variants: uppercase-leading identifier in value position
            (r'\b[A-Z][A-Za-z0-9_]*\b', Name.Class),
            # Raw strings: r"...", r#"..."#
            (r'r(#+)"(?:\\.|[^"\\])*"\1', String),
            (r'r"(?:\\.|[^"\\])*"', String),
            # Byte strings: b"..."
            (r'b"(?:\\.|[^"\\])*"', String),
            # Interpolated / regular strings
            (r'"', String, 'string'),
            # Char literals
            (r"'(?:\\.|[^'\\])'", String.Char),
            # Numeric literals with optional type suffixes
            (r'\d[\d_]*\.\d[\d_]*(?:[eE][-+]?\d+)?'
             r'(?:_?(?:f32|f64))?', Number.Float),
            (r'0x[0-9a-fA-F_]+(?:_?(?:[iu](?:8|16|32|64|128)))?', Number.Hex),
            (r'0o[0-7_]+(?:_?(?:[iu](?:8|16|32|64|128)))?', Number.Oct),
            (r'0b[01_]+(?:_?(?:[iu](?:8|16|32|64|128)))?', Number.Bin),
            (r'\d[\d_]*(?:_?(?:[iu](?:8|16|32|64|128)))?', Number.Integer),
            # Pipeline operator (Lateralus's signature feature)
            (r'\|>', Operator),
            # Other operators
            (r'(==|!=|<=|>=|->|=>|&&|\|\||<<|>>|::|\.\.=?|\?\?|'
             r'[+\-*/%<>!=&|^~?])', Operator),
            # Punctuation
            (r'[{}()\[\];,.:]', Punctuation),
            # Identifiers
            (r'[A-Za-z_][A-Za-z0-9_]*', Name),
        ],
        'block_comment': [
            (r'[^/*]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[/*]', Comment.Multiline),
        ],
        'string': [
            (r'[^"\\{}]+', String),
            (r'\\.', String.Escape),
            (r'\{[^}]*\}', String.Interpol),
            (r'"', String, '#pop'),
        ],
        'attribute': [
            (r'[^\[\]]+', Name.Decorator),
            (r'\[', Name.Decorator, '#push'),
            (r'\]', Name.Decorator, '#pop'),
        ],
    }
