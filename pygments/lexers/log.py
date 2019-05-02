# -*- coding: utf-8 -*-
"""
    pygments.lexers.log
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for various log file formats.

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include
from pygments.token import *

__all__ = ['KernelLogLexer']


class KernelLogLexer(RegexLexer):
    name = 'Kernel log'
    aliases = ['kmsg', 'dmesg']
    filenames = ['*.kmsg', '*.dmesg']

    tokens = {
        'root': [
            (r'^(?=\[)', Text, 'unknown'),
            (r'^([^:]+):debug\s*: (?=\[)', Text, 'debug'),
            (r'^([^:]+):info\s*: (?=\[)', Text, 'info'),
            (r'^([^:]+):warn\s*: (?=\[)', Text, 'warn'),
            (r'^([^:]+):notice\s*: (?=\[)', Text, 'warn'),
            (r'^([^:]+):err\s*: (?=\[)', Text, 'error'),
            (r'^([^:]+):crit\s*: (?=\[)', Text, 'error'),
        ],
        'unknown': [
            (r'^(?=.+(warning|notice|audit|deprecated))', Text, 'warn'),
            (r'^(?=.+(error|critical|fail|Bug))', Text, 'error'),
            (r'', Text, 'info'),
        ],
        'base': [
            (r'\[[0-9\. ]+\] ', Number),
            (r'(?<=\] ).+?:', Keyword),
            (r'\n', Text, '#pop'),
        ],
        'debug': [
            include('base'),
            (r'.+\n', Text, '#pop')
        ],
        'info': [
            include('base'),
            (r'.+\n', Text, '#pop')
        ],
        'warn': [
            include('base'),
            (r'.+', Comment, '#pop')
        ],
        'error': [
            include('base'),
            (r'.+\n', Generic.Error, '#pop')
        ]
    }
