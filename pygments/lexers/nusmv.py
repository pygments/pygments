# -*- coding: utf-8 -*-
"""
    pygments.lexers.nusmv
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for the NuSMV language.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer
from pygments.token import Comment, Generic, Keyword, Name, Number, Operator, \
        Punctuation, Text

__all__ = ['NuSMVLexer']


class NuSMVLexer(RegexLexer):
    """
    Lexer for the NuSMV language.
    """

    name = 'NuSMV'
    aliases = ['nusmv']
    filenames = ['*.smv']
    mimetypes = []

    tokens = {
        'root': [
            (r'--.*\n', Comment),
            ((r'\b(MODULE|DEFINE|MDEFINE|CONSTANTS|VAR|IVAR|FROZENVAR|INIT'
                r'|TRANS|INVAR|SPEC|CTLSPEC|LTLSPEC|PSLSPEC|COMPUTE|NAME'
                r'|INVARSPEC|FAIRNESS|JUSTICE|COMPASSION|ISA|ASSIGN|CONSTRAINT'
                r'|SIMPWFF|CTLWFF|LTLWFF|PSLWFF|COMPWFF|IN|MIN|MAX|MIRROR|PRED'
                r'|PREDICATES)\b'), Keyword.Declaration),
            (r'\bprocess\b', Keyword),
            (r'\b(array|of|boolean|integer|real|word)\b', Keyword.Type),
            (r'\b(case|esac)\b', Keyword),
            ((r'\b(word1|bool|signed|unsigned|extend|resize|sizeof|uwconst'
                r'|swconst|init|self|count|abs|max|min)\b'), Name.Builtin),
            ((r'\b(EX|AX|EF|AF|EG|AG|E|F|O|G|H|X|Y|Z|A|U|S|V|T|BU|EBF|ABF|EBG'
                r'|ABG|next|mod|union|in|xor|xnor)\b'), Operator.Word),
            (r'\b(TRUE|FALSE)\b', Keyword.Constant),
            (r'\-?\d+\b', Number.Integer),
            (r'0[su][bBoOdDhH]\d*_[\da-fA-F]+', Number.Integer),
            (r'\w[\w\d\$#-]*', Name.Variable),
            (r'\s+', Text.Whitespace),
            (r':=', Operator),
            (r'[&\|\+\-\*/<>!=]', Operator),
            (r'[\(\)\[\]\{\};\?:\.,]', Punctuation),
            (r'.', Generic.Error),
        ]
    }

