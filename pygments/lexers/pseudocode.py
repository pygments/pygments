"""
    pygments.lexers.pseudocode
    ~~~~~~~~~~~~~~~~~~~

    Pygments lexers for the imperative language Pseudocode.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer, RegexLexer, include, bygroups, using, \
    this, combined, default, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace
from pygments.util import shebang_matches
from pygments import unistring as uni

__all__ = ['PseudocodeLexer']


class PseudocodeLexer(RegexLexer):
    """
    For Pseudocode source code.
    """

    name = 'Pseudocode'
    url = 'https://pseudocode.site/'
    aliases = ['pseudocode']
    filenames = ['*.pc']
    mimetypes = ['text/x-pseudocode']

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'root': [
            (r'[^\S\n]+', Whitespace),
            (r'(#.*?)(\n)', bygroups(Comment.Single, Whitespace)),
            # keywords: go before method names to avoid lexing "throw new XYZ"
            # as a method signature
            (r'(main|func|if|elif|any|else|while|until|repeat|from|to|step|for|native|final|const|public|is)\b', Keyword),
            # method names
            (r'((?:(?:[^\W\d]|\$)[\w.\[\]$<>]*\s+)+?)'  # return arguments
             r'((?:[^\W\d]|\$)[\w$]*)'                  # method name
             r'(\s*)(\()',                              # signature start
             bygroups(using(this), Name.Function, Whitespace, Punctuation)),
            (r'(var|bool|text|char|nr|int|def)\b', Keyword.Type),
            (r'(true|false|null)\b', Keyword.Constant),
            (r'(import\s)', bygroups(Keyword.Namespace, Whitespace), 'import'),
            (r'"', String, 'string'),
            (r'(\.)((?:[^\W\d]|\$)[\w$]*)', bygroups(Punctuation, Name.Attribute)),
            (r'^(\s*)((?:[^\W\d]|\$)[\w$]*)(:)', bygroups(Whitespace, Name.Label, Punctuation)),
            (r'([^\W\d]|\$)[\w$]*', Name),
            (r'\d+', Number),
            (r'([\+|\-|\*|\/|\%|\^|\!|\=]=?)|(\\b(and|nand|or|nor|xor|xnor|root|not|in)\\b)|(<|<=|=|>=|>)|(≤|≥|≠)', Operator),
            (r'[{}();:.,]', Punctuation),
            (r'\n', Whitespace)
        ],
        'import': [
            (r'[\w.]+\*?', Name.Namespace, '#pop')
        ],
        'string': [
            (r'[^\\"]+', String),
            (r'\\\\', String),  # Escaped backslash
            (r'\\"', String),  # Escaped quote
            (r'\\', String),  # Bare backslash
            (r'"', String, '#pop'),  # Closing quote
        ],
    }