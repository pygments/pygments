# -*- coding: utf-8 -*-
"""
    pygments.lexers.rita
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for RITA language

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, using, this, \
    inherit, words
from pygments.token import Comment, Operator, Keyword, Name, Literal, Punctuation, Text

__all__ = ['RitaLexer']


class RitaLexer(RegexLexer):
    """
    Lexer for `RITA <https://github.com/zaibacu/rita-dsl>`_
     .. versionadded:: 2.7
    """
    name = 'Rita'
    filenames = ['*.rita']
    aliases = ['rita']
    mimetypes = ['text/rita']

    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'#(.*?)\n', Comment.Single),
            (r'@(.*?)\n', Operator),  # Yes, whole line as an operator
            (r'(\"|\')(\w|\d|[^\"\']|(\\\")|(\\\'))+?(\"|\')', Literal),
            (r'([A-Z_]+)', Keyword),
            (r'([a-z_]+)', Name),
            (r'((->)|[!?+*|=])', Operator),
            (r'[\(\),\{\}]', Punctuation)
        ]
    }

