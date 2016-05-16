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
    Number, Punctuation

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
            (r'\b(struct)\b(?:([A-Za-z]+))?', bygroups(Keyword.Declaration,
                Name)),
            (words(('union', 'enum', 'const', 'interface', 'annotation'),
                suffix=r'\b'),
                Keyword),
            (r'(using|import)\b', Keyword.Namespace),
            (r':(Void|Bool|U?Int(8|16|32|64)|Float(32|64)|Text|Data|'\
                    'List\([.a-zA-Z0-9()]*\)|AnyPointer|Capability|'\
                    'union|group)',
                Keyword.Type),
            (r':[.a-zA-Z0-9()]+', Name),
            (r'\b(true|false|void)\b', Keyword.Constant),
            (r'@(0x[a-fA-F0-9]+|\d+)', Keyword),
            (r'0x\h+', Number.Hex),
            (r'\d+\.\d+', Number.Float),
            (r'\d+', Number.Integer),
            (r'"', String, 'string'),
            (r'#.*$', Comment),
            (r'\w+', Name),
            (r'[{}()\[\],.;=$]', Punctuation),
            (r'\s+', Text),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'[^\\"]+', String),
            (r'\\"', String.Escape),
        ],
    }
