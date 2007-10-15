# -*- coding: utf-8 -*-
"""
    pygments.lexers.math
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for math languages.

    :copyright: 2007 by Christopher Creutzig.
    :license: BSD, see LICENSE for more details.
"""

from pygments.lexer import RegexLexer, bygroups
from pygments.token import Comment, String, Punctuation, Keyword, Name, \
    Operator, Number, Text

__all__ = ['MuPADLexer']

class MuPADLexer(RegexLexer):
    """
    A `MuPAD <http://www.mupad.com>`_ lexer.
    Contributed by Christopher Creutzig <christopher@creutzig.de>.

    *New in Pygments 0.8.*
    """
    name = 'MuPAD'
    aliases = ['mupad']
    filenames = ['*.mu']

    tokens = {
      'root' : [
        (r'//.*?$', Comment.Single),
        (r'/\*', Comment.Multiline, 'comment'),
        (r'"(?:[^"\\]|\\.)*"', String),
        (r'\(|\)|\[|\]|\{|\}', Punctuation),
        (r'''(?x)\b(?:
            next|break|end|
            axiom|end_axiom|category|end_category|domain|end_domain|inherits|
            if|%if|then|elif|else|end_if|
            case|of|do|otherwise|end_case|
            while|end_while|
            repeat|until|end_repeat|
            for|from|to|downto|step|end_for|
            proc|local|option|save|begin|end_proc|
            delete|frame
          )\b''', Keyword),
        (r'''(?x)\b(?:
            DOM_ARRAY|DOM_BOOL|DOM_COMPLEX|DOM_DOMAIN|DOM_EXEC|DOM_EXPR|
            DOM_FAIL|DOM_FLOAT|DOM_FRAME|DOM_FUNC_ENV|DOM_HFARRAY|DOM_IDENT|
            DOM_INT|DOM_INTERVAL|DOM_LIST|DOM_NIL|DOM_NULL|DOM_POLY|DOM_PROC|
            DOM_PROC_ENV|DOM_RAT|DOM_SET|DOM_STRING|DOM_TABLE|DOM_VAR
          )\b''', Name.Class),
        (r'''(?x)\b(?:
            PI|EULER|E|CATALAN|
            NIL|FAIL|undefined|infinity|
            TRUE|FALSE|UNKNOWN
          )\b''',
          Name.Constant),
        (r'\b(?:dom|procname)\b', Name.Builtin.Pseudo),
        (r'\.|,|:|;|=|\+|-|\*|/|\^|@|>|<|\$|\||!|\'|%|~=', Operator),
        (r'''(?x)\b(?:
            and|or|not|xor|
            assuming|
            div|mod|
            union|minus|intersect|in|subset
          )\b''',
          Operator.Word),
        (r'\b(?:I|RDN_INF|RD_NINF|RD_NAN)\b', Number),
        #(r'\b(?:adt|linalg|newDomain|hold)\b', Name.Builtin),
        (r'''(?x)
          ((?:[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)
          (?:::[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)*)\s*([(])''',
          bygroups(Name.Function, Punctuation)),
        (r'''(?x)
          (?:[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)
          (?:::[a-zA-Z_#][a-zA-Z_#0-9]*|`[^`]*`)*''', Name.Variable),
        (r'[0-9]+(?:\.[0-9]*)?(?:e[0-9]+)?', Number),
        (r'\.[0-9]+(?:e[0-9]+)?', Number),
        (r'.', Text)
      ],
      'comment' : [
        (r'[^*/]', Comment.Multiline),
        (r'/\*', Comment.Multiline, '#push'),
        (r'\*/', Comment.Multiline, '#pop'),
        (r'[*/]', Comment.Multiline)
      ]
    }
