# -*- coding: utf-8 -*-
"""
    pygments.lexers.graphics
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for computer graphics related languages.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words
from pygments.token import Text, Comment, Operator, Keyword, Name, \
    Number, Punctuation

__all__ = ['GLShaderLexer']


class GLShaderLexer(RegexLexer):
    """
    GLSL (OpenGL Shader) lexer.

    .. versionadded:: 1.1
    """
    name = 'GLSL'
    aliases = ['glsl']
    filenames = ['*.vert', '*.frag', '*.geo']
    mimetypes = ['text/x-glslsrc']

    tokens = {
        'root': [
            (r'^#.*', Comment.Preproc),
            (r'//.*', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
            (r'\+|-|~|!=?|\*|/|%|<<|>>|<=?|>=?|==?|&&?|\^|\|\|?',
             Operator),
            (r'[?:]', Operator),  # quick hack for ternary
            (r'\bdefined\b', Operator),
            (r'[;{}(),\[\]]', Punctuation),
            # FIXME when e is present, no decimal point needed
            (r'[+-]?\d*\.\d+([eE][-+]?\d+)?', Number.Float),
            (r'[+-]?\d+\.\d*([eE][-+]?\d+)?', Number.Float),
            (r'0[xX][0-9a-fA-F]*', Number.Hex),
            (r'0[0-7]*', Number.Oct),
            (r'[1-9][0-9]*', Number.Integer),
            (words((
                'attribute', 'const', 'uniform', 'varying', 'centroid', 'break',
                'continue', 'do', 'for', 'while', 'if', 'else', 'in', 'out',
                'inout', 'float', 'int', 'void', 'bool', 'true', 'false',
                'invariant', 'discard', 'return', 'mat2', 'mat3' 'mat4',
                'mat2x2', 'mat3x2', 'mat4x2', 'mat2x3', 'mat3x3', 'mat4x3',
                'mat2x4', 'mat3x4', 'mat4x4', 'vec2', 'vec3', 'vec4',
                'ivec2', 'ivec3', 'ivec4', 'bvec2', 'bvec3', 'bvec4',
                'sampler1D', 'sampler2D', 'sampler3D' 'samplerCube',
                'sampler1DShadow', 'sampler2DShadow', 'struct'),
                prefix=r'\b', suffix=r'\b'),
             Keyword),
            (words((
                'asm', 'class', 'union', 'enum', 'typedef', 'template', 'this',
                'packed', 'goto', 'switch', 'default', 'inline', 'noinline',
                'volatile', 'public', 'static', 'extern', 'external', 'interface',
                'long', 'short', 'double', 'half', 'fixed', 'unsigned', 'lowp',
                'mediump', 'highp', 'precision', 'input', 'output',
                'hvec2', 'hvec3', 'hvec4', 'dvec2', 'dvec3', 'dvec4',
                'fvec2', 'fvec3', 'fvec4', 'sampler2DRect', 'sampler3DRect',
                'sampler2DRectShadow', 'sizeof', 'cast', 'namespace', 'using'),
                prefix=r'\b', suffix=r'\b'),
             Keyword),  # future use
            (r'[a-zA-Z_][a-zA-Z_0-9]*', Name),
            (r'\.', Punctuation),
            (r'\s+', Text),
        ],
    }
