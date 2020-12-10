# -*- coding: utf-8 -*-
"""
    pygments.lexers.esoteric
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for esoteric languages.

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer
from pygments.token import *

import re

__all__ = ['AMDGCNLexer']

class AMDGCNLexer(RegexLexer):
    name = 'AMDGCN'
    aliases = ['amdgcn']
    filenames = ['*.isa']
    
    flags = re.IGNORECASE

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'[\r\n]+', Text),
            (r'(([a-zA-Z_0-9])*:([a-zA-Z_0-9])*)', Name.Attribute),
            (r'(\[|\]|\(|\)|,|\:|\&)', Text),
            (r'([;#]|//).*?\n', Comment.Single),
            (r'((s_)?(ds|buffer|flat|image)_[a-zA-Z0-9_]+)', Keyword.Reserved),
            (r'(_lo|_hi)', Name.Variable),
            (r'(vmcnt|lgkmcnt|expcnt|vmcnt|lit|unorm|glc)', Name.Attribute),
            (r'(label_[a-zA-Z0-9]+)', Keyword),
            (r'(_L[0-9]*)', Name.Variable),
            (r'(s|v)_[a-zA-Z0-9_]+', Keyword),
            (r'(v[0-9.]+|vcc|exec|v)', Name.Variable),
            (r's[0-9.]+|s', Name.Variable),
            (r'[0-9]+\.[^0-9]+', Number.Float),
            (r'(0[xX][a-zA-Z0-9]+)|([0-9]+)', Number.Integer)
        ]
    }