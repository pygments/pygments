# Copyright (c) 2023, NVIDIA CORPORATION.
"""
    pygments.lexers.numbair
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for other Numba Intermediate Representation.

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.styles.default import DefaultStyle
from pygments.styles.manni import ManniStyle
from pygments.styles.monokai import MonokaiStyle
from pygments.styles.native import NativeStyle

from pygments.lexer import RegexLexer, include, bygroups, words
from pygments.token import Text, Name, String,  Punctuation, Keyword, \
    Operator, Number

from pygments.style import Style

__all__ = ["NumbaIRLexer"]

class NumbaIRLexer(RegexLexer):
    """
    Lexer for Numba IR
    """
    name = 'Numba_IR'
    url = "https://numba.readthedocs.io/en/stable/developer/architecture.html#stage-2-generate-the-numba-ir"
    aliases = ['numba_ir', 'numbair']
    filenames = ['*.numba_ir']
    mimetypes = ['text/x-numba_ir', 'text/x-numbair']

    identifier = r'\$[a-zA-Z0-9._]+'
    fun_or_var = r'([a-zA-Z_]+[a-zA-Z0-9]*)'

    tokens = {
        'root' : [
            (r'(label)(\ [0-9]+)(:)$',
                bygroups(Keyword, Name.Label, Punctuation)),

            (r' = ', Operator),
            include('whitespace'),
            include('keyword'),

            (identifier, Name.Variable),
            (fun_or_var + r'(\()',
                bygroups(Name.Function, Punctuation)),
            (fun_or_var + r'(\=)',
                bygroups(Name.Attribute, Punctuation)),
            (fun_or_var, Name.Constant),
            (r'[0-9]+', Number),

            # <built-in function some>
            (r'<[^>\n]*>', String),

            (r'[=<>{}\[\]()*.,!\':]|x\b', Punctuation)
        ],

        'keyword':[
            (words((
                'del', 'jump', 'call', 'branch',
            ), suffix=' '), Keyword),
        ],

        'whitespace': [
            (r'(\n|\s)', Text),
        ],
    }
