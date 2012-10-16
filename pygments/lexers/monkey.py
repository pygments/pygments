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

    name_variable = r'[a-z_][a-zA-Z0-9_]*'
    name_function = r'[A-Z][a-zA-Z0-9_]*'
    name_constant = r'[A-Z_][A-Z0-9_]*'
    name_class = r'[A-Z][a-zA-Z0-9_]*'
    name_module = r'[a-z0-9_]*'

    keyword_type = r'(?:Int|Float|String|Bool|Object|Array|Void)'
    # ? == Bool // % == Int // # == Float // $ == String
    keyword_type_special = r'[?%#$]'

    flags = re.MULTILINE

    tokens = {
        'root': [
            #Text
            (r'\n', Text),
            (r'\r', Text),
            (r'\t+', Text),  
            (r'\s+', Text),
            # Comments
            (r"'.*", Comment),
            (r'(?i)^#rem\b', Comment.Multiline, 'comment'),
            (r'(?i)^(?:#If|#ElseIf|#Else|#End|#EndIf|#Print|#Error)\s?.*$', Comment.Preproc),
            # String
            ('"', String.Double, 'string'),
            # Numbers
            (r'[0-9]+\.[0-9]*(?!\.)', Number.Float),
            (r'\.[0-9]+(?!\.)', Number.Float),
            (r'[0-9]+', Number.Integer),
            (r'\$[0-9a-f]+', Number.Hex),
            (r'\%[10]+', Number), # Binary
            # Native data types
            (r'\b%s\b' % keyword_type, Keyword.Type),
            # Exception handling
            (r'(?i)\b(?:Try|Catch|Throw)\b', Keyword.Reserved),
            (r'Throwable', Name.Exception),
            # Builtins
            (r'(?i)\b(?:Null|True|False)\b', Name.Builtin),
            (r'(?i)\b(?:Self|Super)\b', Name.Builtin.Pseudo),
            (r'\b(?:HOST|LANG|TARGET|CONFIG)\b', Name.Constant),
            # Keywords
            (r'(?i)^(Import)(\s+)(.*)(\n)', bygroups(Keyword.Namespace, Text, Name.Namespace, Text)),
            (r'(?i)^Strict\b.*\n', Keyword.Reserved),
            (r'(?i)(Const|Local|Global|Field)(\s+)', bygroups(Keyword.Declaration, Text), 'variables'),
            (r'(?i)(New|Class|Interface|Extends|Implements)(\s+)', bygroups(Keyword.Reserved, Text), 'classname'),
            (r'(?i)(Function|Method)(\s+)', bygroups(Keyword.Reserved, Text), 'funcname'),
            (r'(?i)(?:End|Return|Public|Private|Extern|Property|Final|Abstract)\b', Keyword.Reserved),
            # Flow Control stuff
            (r'(?i)(?:If|Then|Else|ElseIf|EndIf|'
             r'Select|Case|Default|'
             r'While|Wend|'
             r'Repeat|Until|Forever|'
             r'For|To|Until|Step|EachIn|Next|'
             r'Exit|Continue)\s+', Keyword.Reserved),
            # not used yet
            (r'(?i)\b(?:Module|Inline)\b', Keyword.Reserved),
            # Other
            (r'<=|>=|<>|[*]=|/=|[+]=|-=|&=|~=|[|]=|[-&*/^+=<>]', Operator),
            (r'Not|Mod|Shl|Shr|And|Or', Operator.Word),
            (r'[\]]', Punctuation, "array"),
            (r'[\(\){}!#,.:]', Punctuation),
            # catch the rest
            (r'%s\b' % name_constant, Name.Constant),
            (r'%s\b' % name_function, Name.Function),
            (r'%s\b' % name_variable, Name.Variable),
        ],
        'funcname': [
            (r'(?i)%s\b' % name_function, Name.Function),
            (r':', Punctuation, 'classname'),
            (r'\s+', Text),
            (r'\(', Punctuation, 'variables'),
            (r'\)', Punctuation, '#pop') 
        ],
        'classname': [
            (r'%s\.' % name_module, Name.Namespace), 
            (r'%s\b' % keyword_type, Keyword.Type),
            (r'%s\b' % name_class, Name.Class),
            (r'\s+(?!<)', Text,'#pop'),
            (r'<', Punctuation),
            (r'>', Punctuation),
            (r'\[', Punctuation, 'array'),
            (r'\n', Punctuation, '#pop'),
            (r'', Text, '#pop')
        ],
        'array' : [
            # direct access myArray[3] and size definition myArray:String[100]
            (r'[0-9]+', Number.Integer),
            # slicing myArray[1..2]
            (r'\.\.', Punctuation),
            (r'\]', Punctuation, '#pop'),
            (r'\[', Punctuation),
        ],
        'variables': [
            (r'%s\b' % name_constant, Name.Constant),
            (r'%s\b' % name_variable, Name.Variable),
            (r'%s' % keyword_type_special, Keyword.Type),
            (r'\s+', Text),
            (r':', Punctuation, 'classname'),
            (r',', Punctuation, '#push'),
            (r'', Text, '#pop')
        ],
        'string': [
            (r'""', String.Double),
            (r'"C?', String.Double, '#pop'),
            (r'[^"]+', String.Double)
        ],
        'comment' : [
            (r'\n', Comment.Multiline),
            (r'^[^#].*', Comment.Multiline),
            (r'(?i)^#End.*?', Comment.Multiline, "#pop"),
            (r'^#\w+\b.*', Comment.Multiline, "#push"),
            (r'.*', Comment.Multiline),
        ],
    }