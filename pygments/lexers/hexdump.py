# -*- coding: utf-8 -*-
"""
    pygments.lexers.hexdump
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for hexadecimal dumps.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Text, Name, Number, String, Punctuation

__all__ = ['HexdumpLexer']


class HexdumpLexer(RegexLexer):
    """
    For hex dumps output by the UNIX hexdump and hexcat tools.
    """
    name = 'Hexdump'
    aliases = ['hexdump']

    hd = r'[0-9A-Ha-h]'

    tokens = {
        'root': [
            (r'\n', Text),
            include('offset'),
            (hd+r'{4}', Number.Hex, 'stringless-mode'),
            (hd+r'{2}', Number.Hex),
            (r'(\s+)(\|)(.{16})(\|)$', bygroups(Text, Punctuation, String, Punctuation), 'piped-strings'),
            (r'(\s+)(\|)(.{1,15})(\|)$', bygroups(Text, Punctuation, String, Punctuation)),
            (r'(\s+)(.{1,15})$', bygroups(Text, String)),
            (r'(\s+)(.{16})$', bygroups(Text, String), 'nonpiped-strings'),
            (r'\s', Text),
            (r'^\*', Punctuation),
        ],
        'offset': [
            (r'^('+hd+'+)(:)', bygroups(Name.Label, Punctuation), 'offset-mode'),
            (r'^'+hd+'+', Name.Label),
        ],
        'offset-mode': [
            (r'\s', Text, '#pop'),
            (hd+'+', Name.Label),
            (r':', Punctuation)
        ],
        'stringless-mode': [
            (r'\n', Text),
            include('offset'),
            (hd+r'{2}', Number.Hex),
            (r'\s', Text),
            (r'^\*', Punctuation),
        ],
        'piped-strings': [
            (r'\n', Text),
            include('offset'),
            (hd+r'{2}', Number.Hex),
            (r'(\s)(\|)(.{1,16})(\|)$', bygroups(Text, Punctuation, String, Punctuation)),
            (r'\s', Text),
            (r'^\*', Punctuation),
        ],
        'nonpiped-strings': [
            (r'\n', Text),
            include('offset'),
            (hd+r'{2}', Number.Hex),
            (r'(\s+)(.{1,16})$', bygroups(Text, String)),
            (r'\s', Text),
            (r'^\*', Punctuation),
        ],
    }
