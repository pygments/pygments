# -*- coding: utf-8 -*-
"""
    pygments.lexers.capnproto
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for the Cap'n Proto schema language.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Literal

__all__ = ['CapnProtoLexer']


class CapnProtoLexer(RegexLexer):
    """
    For `Cap'n Proto <https://capnproto.org>`_ source.

    .. versionadded:: 2.2
    """
    name = 'Cap\'n Proto'
    filenames = ['*.capnp']
    aliases = ['capnp']

    flags = re.MULTILINE | re.UNICODE


    tokens = {
        'root': [
            (r'#.*?$', Comment.Single),
            (r'@[0-9a-zA-Z]*', Name.Decorator),
            (r'=', Literal, 'expression'),
            (r':', Name.Class, 'type'),
            (r'\$', Name.Attribute, 'annotation'),
            (r'(struct|enum|interface|union|import|using|const|annotation|extends|in|of|on|as|with|from|fixed)\b',
                Keyword),
            (r'[a-zA-Z0-9_.]+', Name),
            (r'[^#@=:$a-zA-Z0-9_]+', Text),
        ],
        'type': [
            (r'[^][=;,(){}$]+', Name.Class),
            (r'[[(]', Name.Class, 'parentype'),
            (r'', Name.Class, '#pop')
        ],
        'parentype': [
            (r'[^][;()]+', Name.Class),
            (r'[[(]', Name.Class, '#push'),
            (r'[])]', Name.Class, '#pop'),
            (r'', Name.Class, '#pop')
        ],
        'expression': [
            (r'[^][;,(){}$]+', Literal),
            (r'[[(]', Literal, 'parenexp'),
            (r'', Literal, '#pop')
        ],
        'parenexp': [
            (r'[^][;()]+', Literal),
            (r'[[(]', Literal, '#push'),
            (r'[])]', Literal, '#pop'),
            (r'', Literal, '#pop')
        ],
        'annotation': [
            (r'[^][;,(){}=:]+', Name.Attribute),
            (r'[[(]', Name.Attribute, 'annexp'),
            (r'', Name.Attribute, '#pop')
        ],
        'annexp': [
            (r'[^][;()]+', Name.Attribute),
            (r'[[(]', Name.Attribute, '#push'),
            (r'[])]', Name.Attribute, '#pop'),
            (r'', Name.Attribute, '#pop')
        ],
    }
