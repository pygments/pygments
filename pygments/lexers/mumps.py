# -*- coding: utf-8 -*-
"""
    pygments.lexers.mumps
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for MUMPS

    :copyright: Copyright 2020 by the Pygments team, see AUTHORS.
    :licens: BSD, see LICENSE for details.
"""

import re
from pygments.lexer import RegexLexer, words, bygroups, default, include
from pygments.token import Whitespace, Keyword

__all__ = ['MumpsLexer']

class MumpsLexer(RegexLexer):
    """
    For MUMPS source code.
    
    Derived from `The Annotated M[UMPS] Standard <http://71.174.62.16/Demo/AnnoStd>`
    Section numbers below refer to the sections on that site, most often from the latest (1995) standard.
    """

    name = 'MUMPS'
    aliases = ['Mumps', 'mumps', 'M']
    filenames = ['*.m', '*.mumps', '*.epc', '*.int']
    # Filenames aren't meaningful in M, but some implementations allow file export/import of routines
    # For example, YottaDB would have the source file for "dmex" in "dmex.m"
    flags = re.IGNORECASE

    tokens = {
        'root': [
            (' ', Whitespace, 'linebody'),
        ],
        'linebody': [
            ('q', Keyword),
            ('\n', Whitespace, '#pop'),
        ],
    }
