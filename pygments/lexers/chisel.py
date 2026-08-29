# -*- coding: utf-8 -*- 
"""
    pygments.lexers.chisel
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for Chisel, a hardware construction language extended from Scala.

    :copyright: Copyright 2006-2019 by the Pygments team.
    :license: BSD, see LICENSE for details.
"""
import re

from pygments.lexer import Lexer, RegexLexer, include, bygroups, using, \
    this, combined, default, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation
from pygments.util import shebang_matches
from pygments.lexers.jvm import ScalaLexer
from pygments import unistring as uni

__all__ = ['ChiselLexer']

class ChiselLexer(RegexLexer):
    """
        For Chisel source code.
        Lexer created by John Andrews.
    """

    name = 'Chisel'
    aliases = ['chisel']
    filenames = ['*.scala']
    mimetypes = ['text/x-scala']

    flags = re.MULTILINE | re.DOTALL

    # don't use raw unicode strings!
    op = ScalaLexer.op

    letter = ScalaLexer.letter

    upper = ScalaLexer.upper

    idrest = ScalaLexer.idrest
    letter_letter_digit = ScalaLexer.letter_letter_digit

    tokens = ScalaLexer.tokens.copy()
    tokens['root'] = [
            # method names
            (r'(class|trait|object)(\s+)', bygroups(Keyword, Text), 'class'),
            (r'[^\S\n]+', Text),
            (r'//.*?\n', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment'),
            (u'@%s' % idrest, Name.Decorator),
            (u'(abstract|ca(?:se|tch)|d(?:ef|o)|e(?:lse|xtends)|'
             u'f(?:inal(?:ly)?|or(?:Some)?)|i(?:f|mplicit)|'
             u'lazy|match|new|override|pr(?:ivate|otected)'
             u'|re(?:quires|turn)|s(?:ealed|uper)|'
             u't(?:h(?:is|row)|ry)|va[lr]|w(?:hile|ith)|yield)\\b|'
             u'(<[%:-]|=>|>:|[#=@_\u21D2\u2190])(\\b|(?=\\s)|$)', Keyword),
            (u':(?!%s)' % op, Keyword, 'type'),
            (r'(In|Out)put|IO', Name.Builtin),
            (r'(Modu|Bund)le', Name.Builtin),
            (r'(([US])?Int|Bool|Vec(Init)?|Wire|Reg(Init)?|Mux|Mem|Enum)\b', Name.Builtin),
            (r'(when|\.elsewhen|\.otherwise|switch|is)\b', Name.Builtin),
            (r'log2|(ceil|floor)\b', Name.Builtin),
            (r'isPow2\b', Name.Builtin),
            # (u'%s%s\\b' % (upper, idrest), Name.Class),
            (r'(true|false|null)(\.B)?\b', Keyword.Constant),
            (r'(import|package)(\s+)', bygroups(Keyword, Text), 'import'),
            (r'(type)(\s+)', bygroups(Keyword, Text), 'type'),
            (r'""".*?"""(?!")', String),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r"'\\.'|'[^\\]'|'\\u[0-9a-fA-F]{4}'", String.Char),
            (u"'%s" % idrest, Text.Symbol),
            (r'[fs]"""', String, 'interptriplestring'),  # interpolated strings
            (r'[fs]"', String, 'interpstring'),  # interpolated strings
            (r'raw"(\\\\|\\"|[^"])*"', String),  # raw strings
            # (ur'(\.)(%s|%s|`[^`]+`)' % (idrest, op), bygroups(Operator,
            # Name.Attribute)),
            (idrest, Name),
            (r'`[^`]+`', Name),
            (r'\[', Operator, 'typeparam'),
            (r'[(){};,.#]', Operator),
            (op, Operator),
            (r'[0-9]+L?(\.[WU])?', Number.Integer),
            (r'([0-9][0-9]*\.[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?[fFdD]?',
             Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'\n', Text)
        ]

def test(filename):
    contents = open(filename).read()
    tokens = list(ChiselLexer().get_tokens(contents))
    for token in tokens:
        print(token)
