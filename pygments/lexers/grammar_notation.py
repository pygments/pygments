# -*- coding: utf-8 -*-
"""
    pygments.lexers.grammar_notation
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for parser generators.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, DelegatingLexer, \
    include, bygroups, using, words
from pygments.token import Punctuation, Other, Text, Comment, Operator, \
    Keyword, Name, String, Number, Whitespace, Literal

__all__ = ['AbnfLexer']


# EBNF shold be moved here, i think.

class AbnfLexer(RegexLexer):
    """
    Lexer for `IETF 7405 ABNF
    <http://www.ietf.org/rfc/rfc7405.txt>`_
    (Updates `5234 <http://www.ietf.org/rfc/rfc5234.txt>`_)
    grammars.

    .. versionadded:: 2.1
    """

    name = 'ABNF'
    aliases = ['abnf']
    filenames = ['*.abnf']
    mimetypes = ['text/x-abnf']

    _core_rules = (
        'ALPHA', 'BIT', 'CHAR', 'CR', 'CRLF', 'CTL', 'DIGIT',
        'DQUOTE', 'HEXDIG', 'HTAB', 'LF', 'LWSP', 'OCTET',
        'SP', 'VCHAR', 'WSP',)

    def nonterminal_cb(self, match):
        txt = match.group(0)
        if txt in self._core_rules:
            # Strictly speaking, these are not keyword but
            # is called `Core Rule'.
            yield match.start(), Keyword, txt
        else:
            yield match.start(), Name.Class, txt

    tokens = {
        'root': [
            # comment
            (r';.*$', Comment.Single),

            # quoted
            (r'(%[si])?"', Literal, 'quoted-termination'),

            # binary (but i have never seen...)
            (r'%b[01]+\-[01]+\b', Literal),  # range
            (r'%b[01]+(\.[01]+)*\b', Literal),  # concat

            # decimal
            (r'%d[0-9]+\-[0-9]+\b', Literal),  # range
            (r'%d[0-9]+(\.[0-9]+)*\b', Literal),  # concat

            # hexadecimal
            (r'%x[0-9a-fA-F]+\-[0-9a-fA-F]+\b', Literal),  # range
            (r'%x[0-9a-fA-F]+(\.[0-9a-fA-F]+)*\b', Literal),  # concat

            # repetition (<a>*<b>element) including nRule
            (r'\b[0-9]+\*[0-9]+', Operator),
            (r'\b[0-9]+\*', Operator),
            (r'\b[0-9]+', Operator),
            (r'\*', Operator),

            # nonterminals (ALPHA *(ALPHA / DIGIT / "-"))
            (r'[a-zA-Z][a-zA-Z0-9-]+\b', nonterminal_cb),

            # operators
            (r'(=/|=|/)', Operator),

            # punctuation
            (r'[\[\]()]', Punctuation),

            # fallback
            (r'.', Text),
        ],
        'quoted-termination': [
            # double quote itself in this state, it is as '%x22'.
            (r'"', Literal, '#pop'),
            (r'.', Literal),
        ]
    }
