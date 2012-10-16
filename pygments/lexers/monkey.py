# -*- coding: utf-8 -*-
"""
    pygments.lexers.monkey
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the monkey language

    :copyright: Copyright 2012
    :license: BSD, see LICENSE for details.
"""
import re

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Punctuation, Text, Comment, Operator, Keyword, Name, String, Number, Literal, Other

__all__ = ['MonkeyLexer']

class MonkeyLexer(RegexLexer):
    """
    For
    `Monkey <https://en.wikipedia.org/wiki/Monkey_(programming_language)>`_
    source code.
    """

    name = 'Monkey'
    aliases = ['monkey']
    filenames = ['*.monkey']
    mimetypes = [] # TODO

    name_variable = r'[a-z_][a-zA-Z0-9_]*\b'
    name_function = r'[A-Z][a-zA-Z0-9_]*\b'
    name_constant = r'[A-Z_][A-Z0-9_]*\b'
    keyword_type = r'(Int|Float|String|Bool|Object|Array|Void)'

    tokens = {
        'root': [
            #Text
            (r'\n', Text), 
            (r'\s+', Text),
            # Comments
            (r"'.*$", Comment),
            (r'(?i)^#rem\b', Comment.Multiline, 'comment'),
            (r'^(#If\|#ElseIf|#Else|#End|#Print|#Error)\s.*?$', Comment.Preproc),
            # String
            ('"', String.Double, 'string'),
            # Numbers
            (r'[0-9]+\.[0-9]*(?!\.)', Number.Float),
            (r'\.[0-9]*(?!\.)', Number.Float),
            (r'[0-9]+', Number.Integer),
            (r'\$[0-9a-f]+', Number.Hex),
            (r'\%[10]+', Number), # Binary
            # Native data types
            (r'\b%s\b' % keyword_type, Keyword.Type),
            # Exception handling
            (r'(?i)\b(Try|Catch)\b', Keyword.Reserved),
            (r'Throwable', Name.Exception),
            # Builtins
            (r'(?i)\b(Null|True|False)\b', Name.Builtin),
            (r'(?i)\b(Self|Super)\b', Name.Builtin.Pseudo),
            (r'\b(HOST|LANG|TARGET|CONFIG)\b', Name.Constant),
            # Keywords
            (r'(?i)^(Import|Extern).*$', Keyword.Namespace),
            (r'(?i)^Strict\b', Keyword.Reserved),
            (r'(?i)(Const|Local|Global|Field)(\s+)', bygroups(Keyword.Declaration, Text), 'variables'),
            (r'(?i)(Class|New|Extends|Implements)(\s+)', bygroups(Keyword.Reserved, Text), 'classname'),
            (r'(?i)(Function|Method)(\s+)', bygroups(Keyword.Reserved, Text), 'funcname'),
            (r'(?i)(End|Return|Final|Public|Private)\b', Keyword.Reserved),
            # Flow Control stuff
            (r'(?i)(If|Then|Else|ElseIf|EndIf|'
             r'Select|Case|Default|'
             r'While|Wend|'
             r'Repeat|Until|Forever|'
             r'For|To|Until|Step|EachIn|Next|'
             r'Exit|Continue)\s+', Keyword.Reserved),
            # not used yet
            (r'(?i)\b(Module|Inline)\b', Keyword.Reserved),
            # Other
            (r'<=|>=|<>|[*]=|/=|[+]=|-=|&=|~=|[|]=|[-&*/^+=<>]', Operator),
            (r'Not|Mod|Shl|Shr|And|Or', Operator.Word),
            (r'[\(\){}!#,.:]', Punctuation),
            # catch the rest
            (r'\b%s\b' % name_constant, Name.Constant),
            (r'\b%s\b' % name_function, Name.Function),
            (r'\b%s\b' % name_variable, Name.Variable),
        ],
        'funcname': [
            (r'[A-Z][a-zA-Z0-9_]*', Name.Function),
            (r':', Punctuation, 'classname'),
            (r'\(', Punctuation, 'variables'),
            (r'\)', Punctuation, '#pop') 
        ],
        'classname': [
            (r'\b%s\b' % keyword_type, Keyword.Type, '#pop'),
            (r'[A-Z][a-zA-Z0-9_]*', Name.Class, '#pop'),
            (r'', Text, '#pop')
        ],
        'variables': [
            (r'%s' % name_constant, Name.Constant),
            (r'%s' % name_variable, Name.Variable),
            (r':', Punctuation, 'classname'),
            (r',\s+', Punctuation, '#push'),
            (r'', Text, '#pop')
        ],
        'string': [
            (r'""', String.Double),
            (r'"C?', String.Double, '#pop'),
            (r'[^"]+', String.Double)
        ],
        'comment' : [
            (r'[^#].*\n', Comment.Multiline),
            (r'(?i)#End.*?\n', Comment.Multiline, "#pop"),
            (r'^#', Comment.Multiline, "#push"),
        ],
    }