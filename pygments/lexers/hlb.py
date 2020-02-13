# -*- coding: utf-8 -*-
"""
    pygments.lexers.hlb
    ~~~~~~~~~~~~~~~~~~

    Lexers for HLB.

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

import re

__all__=['HlbLexer']

class HlbLexer(RegexLexer):
    """
    Lexer for `HLB <https://github.com/openllb/hlb>`_ source code.

    .. versionadded:: 2.6
    """
    name = 'Hlb'
    aliases = ['hlb']
    filenames = ['*.hlb']
    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root' : [
            (u'(#.*)', bygroups(Comment.Single)),
            (u'((\\b(0(b|B|o|O|x|X)[a-fA-F0-9]+)\\b)|(\\b(0|[1-9][0-9]*)\\b)|(\\b(true|false)\\b))', bygroups(Name.Constant)),
            (u'(\\bstring\\b|\\bint\\b|\\bbool\\b|\\bfs\\b|\\boption\\b)', bygroups(Keyword.Type)),
            (u'(\\b[a-zA-Z_][a-zA-Z0-9]*\\b)(\\()', bygroups(Keyword, Punctuation), 'params'),
            (u'(\\{)', bygroups(Punctuation), 'block'),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\"', String),
            (r'[^\\"]+', String)
        ],
        'block' : [
            (u'(\\})', bygroups(Punctuation), '#pop'),
            (u'(#.*)', bygroups(Comment.Single)),
            (u'((\\b(0(b|B|o|O|x|X)[a-fA-F0-9]+)\\b)|(\\b(0|[1-9][0-9]*)\\b)|(\\b(true|false)\\b))', bygroups(Name.Constant)),
            (r'"', String, 'string'),
            (u'(\\b(with|as|variadic)\\b)', bygroups(Name.Builtin)),
            (u'(\\bstring\\b|\\bint\\b|\\bbool\\b|\\bfs\\b|\\boption\\b)([\\t ]+)(\\{)', bygroups(Keyword.Type, Text, Punctuation), 'block'),
            (u'(\\b((?!(scratch|image|resolve|http|checksum|chmod|filename|git|keepGitDir|local|includePatterns|excludePatterns|followPaths|generate|frontendInput|shell|run|readonlyRootfs|env|dir|user|network|security|host|ssh|secret|mount|target|localPath|uid|gid|mode|readonly|tmpfs|sourcePath|cache|mkdir|createParents|chown|createdTime|mkfile|rm|allowNotFound|allowWildcards|copy|followSymlinks|contentsOnly|unpack|createDestPath)\\b)[a-zA-Z_][a-zA-Z0-9]*\\b))', bygroups(Name.Variable)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ], 
        'params' : [
            (u'(\\))', bygroups(Punctuation), '#pop'),
            (u'(\\bstring\\b|\\bint\\b|\\bbool\\b|\\bfs\\b|\\boption\\b)', bygroups(Keyword.Type)),
            (u'(\\b[a-zA-Z_][a-zA-Z0-9]*\\b)', bygroups(Name.Variable)),
            ('(\n|\r|\r\n)', Text),
            ('.', Text),
        ]
    }

