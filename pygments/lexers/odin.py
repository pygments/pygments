import re

from pygments.lexer import RegexLexer, bygroups, words, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

__all__ = ['OdinLangLexer']


class OdinLangLexer(RegexLexer):
    """
    For `Odin <https://odin-lang.org>` source.
    """
    name = 'Odin'
    url = 'https://odin-lang.org'
    filenames = ['*.odin']
    aliases = ['odinlang']
    mimetypes = ['text/x-odinsrc']
    version_added = '2.20' 

    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text),  # line continuations
            (r'//(.*?)\n', Comment.Single),
            (r'/\*', Comment.Multiline, 'nested_comment'),
            (r'(foreign|import|package)\b', Keyword.Namespace),
            (r'(proc|struct|union|enum|bit_set|matrix|map|typeid|using)\b',
             Keyword.Declaration),
            (words((
                'asm', 'auto_cast', 'bit_set', 'break', 'case', 'cast', 
                'context', 'continue', 'defer', 'distinct', 'do', 
                'dynamic', 'else', 'enum', 'fallthrough', 'for', 'foreign', 
                'if', 'import', 'in', 'map', 'matrix', 'not_in', 'or_else', 
                'or_return', 'package', 'proc', 'return', 'struct', 'switch', 
                'transmute', 'typeid', 'union', 'using', 'when', 'where'), suffix=r'\b'),
             Keyword),
            (r'(true|false|nil)\b', Keyword.Constant),
            # It seems the builtin types aren't actually keywords, but
            # can be used as functions. So we need two declarations.
            (words((
                'bool', 'b8', 'b16', 'b32', 'b64', 
                'i8', 'u8', 'i16', 'u16', 'i32', 'u32', 'i64', 'u64', 'i128', 'u128', 
                'rune', 
                'f16', 'f32', 'f64', 
                'complex32', 'complex64', 'complex128', 
                'quaternion64', 'quaternion128', 'quaternion256', 
                'int', 'uint', 'uintptr', 'rawptr', 
                'string', 'cstring', 'any', 'typeid', 
                'i16le', 'u16le', 'i32le', 'u32le', 'i64le', 'u64le', 'i128le', 'u128le', 
                'i16be', 'u16be', 'i32be', 'u32be', 'i64be', 'u64be', 'i128be', 'u128be', 
                'f16le', 'f32le', 'f64le', 'f16be', 'f32be', 'f64be', 'byte', 

                'len', 'cap', 'size_of', 'align_of', 'offset_of', 'offset_of_by_string', 
                'type_of', 'type_info_of', 'typeid_of', 'swizzle', 'complex', 
                'quaternion', 'real', 'imag', 'jmag', 'kmag', 'conj', 
                'expand_to_tuple', 'min', 'max', 'abs', 'clamp', 
                'soa_zip', 'soa_unzip', 'transpose', 'outer_product', 
                'hadamard_product', 'matrix_flatten'), suffix=r'\b(\()'),
             bygroups(Name.Builtin, Punctuation)),
            (words((
                'bool', 'b8', 'b16', 'b32', 'b64', 
                'i8', 'u8', 'i16', 'u16', 'i32', 'u32', 'i64', 'u64', 'i128', 'u128', 
                'rune', 
                'f16', 'f32', 'f64', 
                'complex32', 'complex64', 'complex128', 
                'quaternion64', 'quaternion128', 'quaternion256', 
                'int', 'uint', 'uintptr', 'rawptr', 
                'string', 'cstring', 'any', 'typeid', 
                'i16le', 'u16le', 'i32le', 'u32le', 'i64le', 'u64le', 'i128le', 'u128le', 
                'i16be', 'u16be', 'i32be', 'u32be', 'i64be', 'u64be', 'i128be', 'u128be', 
                'f16le', 'f32le', 'f64le', 'f16be', 'f32be', 'f64be', 'byte'), suffix=r'\b'),
             Keyword.Type),
            # imaginary_lit
            (r'\d+i', Number),
            (r'\d+\.\d*([Ee][-+]\d+)?i', Number),
            (r'\.\d+([Ee][-+]\d+)?i', Number),
            (r'\d+[Ee][-+]\d+i', Number),
            # float_lit
            (r'\d+(\.\d+[eE][+\-]?\d+|'
             r'\.\d*|[eE][+\-]?\d+)', Number.Float),
            (r'\.\d+([eE][+\-]?\d+)?', Number.Float),
            # int_lit
            # -- binary_lit
            (r'0b[01]+', Number.Bin),
            # -- octal_lit
            (r'0o[0-7]+', Number.Oct),
            # -- hex_lit
            (r'0[x][0-9a-fA-F]+', Number.Hex),
            # -- decimal_lit
            (r'([0-9]+)', Number.Integer),
            (r'(0d[0-9]+)', Number.Integer),
            # -- dozenal_lit
            (r'(0z[0-9abAB]+)', Number.Integer),
            # char_lit
            (r"""'(\\['"\\abfnrtv]|\\x[0-9a-fA-F]{2}|\\[0-7]{1,3}"""
             r"""|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8}|[^\\])'""",
             String.Char),
            # StringLiteral
            # -- raw_string_lit
            (r'`[^`]*`', String),
            # -- interpreted_string_lit
            (r'"(\\\\|\\[^\\]|[^"\\])*"', String),
            # Tokens
            (r'([+-\/%~!=<>]=?|%%=?|---|\?|\|(?:=|\|=?)?|&(?:=|&=?|\~=?)?|>(?:>=?|=)?|<(?:<=?|=)?|:\s*=|:\s*:|\.\.[=<]?|->|[@#$])', Operator),
            (r'[|^<>=!()\[\]{}.,;:]', Punctuation),
            # identifier
            (r'[^\W\d]\w*', Name.Other),
        ],
        'nested_comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
    }
