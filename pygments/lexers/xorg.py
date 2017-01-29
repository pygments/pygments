# -*- coding: utf-8 -*-
"""
    pygments.lexers.xorg
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for Xorg configs.

    :copyright: Copyright 2006-2017 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups
from pygments.token import Comment, String, Name, Text

__all__ = ['XorgLexer']


class XorgLexer(RegexLexer):
    name = 'Xorg'
    aliases = ['xorg.conf']
    filenames = ['xorg.conf']
    mimetypes = []

    tokens = {
        'root': [
            (r'#.*$', Comment),
            (r'((|Sub)Section)(\s+)("\w+")',
             bygroups(String.Escape, String.Escape, Text, String.Escape)),
            (r'(End(|Sub)Section)', String.Escape),

            (r'(^(?!S|E|#)(|\s+)(?!Sec|End|Sub)\w+)(\s+)([^\n#]+)',
             bygroups(Name.Builtin, Text, Text, Name.Constant)),
        ],
    }
