"""
    pygments.lexers.tal
    ~~~~~~~~~~~~~~~~~~~

    Lexers for Uxntal

    .. versionadded:: 2.12

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
import keyword

from pygments.lexer import RegexLexer
from pygments.token import Comment, Keyword, Name, String, Number, Punctuation, Whitespace, Literal

__all__ = ['TalLexer']

class TalLexer(RegexLexer):

    name = 'Tal'
    aliases = ['tal', 'uxntal']
    filenames = ['*.tal']
    mimetypes = ['text/x-uxntal']

    tokens = {
        'comment': [
            (r'(?:(?<=\s)|^)\((?:(?=\s)|$)', Comment.Multiline, '#push'), # nested comments
            (r'(?:(?<=\s)|^)\)(?:(?=\s)|$)', Comment.Multiline, '#pop'), # nested comments
            (r'\S+', Comment.Multiline), # comments
            (r'\s+', Comment.Multiline), # comments
        ],
        'root': [
            (r'\s+', Whitespace), # spaces
            (r'(?:(?<=\s)|^)\((?:(?=\s)|$)', Comment.Multiline, 'comment'), # comments
            (r'(?:(?<=\s)|^)(BRK|LIT|INC|POP|DUP|NIP|SWP|OVR|ROT|EQU|NEQ|GTH|LTH|JMP|JCN|JSR|STH|LDZ|STZ|LDR|STR|LDA|STA|DEI|DEO|ADD|SUB|MUL|DIV|AND|ORA|EOR|SFT)2?k?r?(?:(?=\s)|$)', Keyword.Reserved), # instructions
            (r'(?:(?<=\s)|^)[][{}](?:(?=\s)|$)', Punctuation), # delimiers
            (r'#([0-9a-f]{2}){1,2}', Number.Hex), # integer
            (r'"\S+', String), # raw string
            (r'\'\S', String.Char), # raw char
            (r'([0-9a-f]{2}){1,2}(?:(?=\s)|$)', Literal), # raw integer
            (r'\|[0-9a-f]{1,4}(?:(?=\s)|$)', Keyword.Declaration), # abs pad
            (r'\$[0-9a-f]{1,4}(?:(?=\s)|$)', Keyword.Constant), # rel pad
            (r'%\S+', Name.Decorator), # macro
            (r'@\S+', Name.Function), # label
            (r'&\S+', Name.Label), # sublabel
            (r'/\S+', Name.Tag), # spacer
            (r'\.\S+', Name.Variable.Magic), # zero page addr
            (r',\S+', Name.Variable.Instance), # rel addr
            (r';\S+', Name.Variable.Global), # abs addr
            (r':\S+', Literal), # raw addr
            (r'~\S+', Keyword.Namespace), # include
            (r'\S+', Name),
        ]
    }

    def analyse_text(text):
        return '|0100' in text[:500]
