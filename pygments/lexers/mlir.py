"""
    pygments.lexers.mlir
    ~~~~~~~~~~~~~~~~~~~~

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
    For `MLIR <https://mlir.llvm.org/>`_ source code.

    MLIR is a compiler infrastructure for building reusable and extensible
    compiler infrastructure. Used by TensorFlow, IREE, ONNX-MLIR and others.

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
            (r'/\*', Comment.Multiline, 'comment'),
            # Builtin types
            (words((
                'i1', 'i8', 'i16', 'i32', 'i64', 'i128',
                'si8', 'si16', 'si32', 'si64',
                'ui8', 'ui16', 'ui32', 'ui64',
                'f16', 'f32', 'f64', 'f80', 'f128', 'bf16',
                'index', 'none',
            ), suffix=r'(?![\w$])'), Keyword.Type),
            # Dialect keywords
            (words((
                'func', 'module', 'return', 'br', 'cond_br', 'call',
                'call_indirect', 'constant', 'unreachable',
                'alloc', 'alloca', 'load', 'store', 'dealloc',
                'memref_cast', 'tensor_cast',
                'affine', 'memref', 'tensor', 'vector',
                'scf', 'linalg', 'arith', 'math', 'bufferization',
                'cf', 'gpu', 'llvm', 'spirv', 'tosa',
                'shape', 'transform', 'irdl',
            ), suffix=r'(?![\w$])'), Keyword),
            # Attributes / dialect namespaces
            (r'#[A-Za-z_][\w.]*(?:<[^>]*>)?', Name.Decorator),
            (r'![A-Za-z_][\w.]*', Name.Decorator),
            # SSA values  %foo  %0
            (r'%[A-Za-z_$][\w$]*(?:#\d+)?', Name.Variable),
            (r'%\d+(?:#\d+)?', Name.Variable),
            # Block labels  ^bb0
            (r'\^[A-Za-z_$][\w$]*', Name.Label),
            # Symbol references  @main
            (r'@[A-Za-z_$][\w$]*', Name.Function),
            # Strings
            (r'"[^"\\]*(?:\\.[^"\\]*)*"', String.Double),
            # Numbers
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'-?\d+\.\d*(?:[eE][+-]?\d+)?', Number.Float),
            (r'-?\d+', Number.Integer),
            # Operators / punctuation
            (r'->|::|[+\-*/%&|^~]|==|!=|<=?|>=?', Operator),
            (r'[(){};:,.<>\[\]]', Punctuation),
            (r'[A-Za-z_$][\w$]*', Name),
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
    }
