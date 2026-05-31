"""
    pygments.lexers.mlir
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for MLIR (Multi-Level Intermediate Representation).

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words
from pygments.token import (Comment, Keyword, Name, Number, Operator,
                             Punctuation, String, Whitespace)

__all__ = ['MlirLexer']


class MlirLexer(RegexLexer):
    """
    For MLIR (Multi-Level Intermediate Representation) source code.

    MLIR is a compiler infrastructure project from LLVM/Google used
    by TensorFlow, IREE, Mojo, and other compilers as a flexible IR.

    .. versionadded:: 2.21
    """

    name = 'MLIR'
    url = 'https://mlir.llvm.org/'
    aliases = ['mlir']
    filenames = ['*.mlir']
    mimetypes = ['text/x-mlir']
    version_added = '2.21'

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'//.*?$', Comment.Single),
            # Attributes and dialects:  #attr, !type, @symbol, %value, ^block, $constraint
            (r'#[a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*', Name.Attribute),
            (r'![a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*', Keyword.Type),
            (r'@[a-zA-Z_$][\w$]*', Name.Function),
            (r'%[a-zA-Z_$][\w$]*|%\d+', Name.Variable),
            (r'\^[a-zA-Z_]\w*|\^\d+', Name.Label),
            (r'\$[a-zA-Z_]\w*', Name.Variable.Magic),
            # Keywords
            (words((
                'func', 'return', 'call', 'module', 'region', 'block',
                'def', 'type', 'attribute', 'affine_map', 'affine_set',
                'loc', 'unreachable', 'br', 'cond_br',
                # Common ops
                'arith', 'memref', 'tensor', 'vector', 'linalg', 'scf',
                'cf', 'bufferization', 'gpu', 'llvm', 'math', 'index',
                'transform', 'pdl', 'irdl',
            ), suffix=r'\b'), Keyword),
            (words((
                'true', 'false', 'dense', 'sparse', 'opaque',
                'unit', 'none',
            ), suffix=r'\b'), Keyword.Constant),
            # Built-in types
            (words((
                'index', 'bf16', 'f16', 'f32', 'f64', 'f80', 'f128',
                'i1', 'i8', 'i16', 'i32', 'i64', 'i128',
                'si8', 'si16', 'si32', 'si64',
                'ui8', 'ui16', 'ui32', 'ui64',
                'complex', 'vector', 'tensor', 'memref',
                'tuple', 'none', 'opaque',
            ), suffix=r'\b'), Keyword.Type),
            # Numbers
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'[+-]?\d+\.\d*([eE][+-]?\d+)?', Number.Float),
            (r'[+-]?\d+', Number.Integer),
            # Strings
            (r'"(?:\\.|[^"\\])*"', String.Double),
            # Operators and punctuation
            (r'->|::|[=<>!+\-*/%&|^~]', Operator),
            (r'[(){};:,\[\]<>]', Punctuation),
            (r'\.', Punctuation),
            (r'[a-zA-Z_]\w*', Name),
        ],
    }
