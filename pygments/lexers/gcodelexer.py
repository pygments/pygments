# -*- coding: utf-8 -*-
"""
    pygments.lexers.gcodelexer.py
    ~~~~~~~~~~~~~~~~~~~~~~
    Lexers for gcode Language.
    :copyright: Copyright 2006-2017 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *

__all__ = ['gcodeLexer']

class gcodeLexer(RegexLexer):
    """
    For gcode source code.
    
    .. versionadded:: 2.9
    """
    name = 'g-code'
    aliases = ['gcode']
    filenames = ['*.gcode']
    
    tokens = {
        'root': [
            (r';.*\n', Comment),
            (r'^[gmGM]\d{1,4}\s',Name.Builtin), # M or G commands
            (r'([^gGmM])([+-]?\d*[.]?\d+)', bygroups(Keyword,Number)),
            (r'\s', Text.Whitespace),
            (r'.*\n', Text),
        ]
    }
    
