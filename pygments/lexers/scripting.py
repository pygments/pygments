# -*- coding: utf-8 -*-
"""
    pygments.lexers.scripting
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for scripting and embedded languages.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, default, combined
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation
from pygments.util import get_bool_opt, get_list_opt, iteritems

__all__ = ['LuaLexer', 'MoonScriptLexer', 'ChaiscriptLexer']


class LuaLexer(RegexLexer):
    """
    For `Lua <http://www.lua.org>`_ source code.

    Additional options accepted:

    `func_name_highlighting`
        If given and ``True``, highlight builtin function names
        (default: ``True``).
    `disabled_modules`
        If given, must be a list of module names whose function names
        should not be highlighted. By default all modules are highlighted.

        To get a list of allowed modules have a look into the
        `_luabuiltins` module:

        .. sourcecode:: pycon

            >>> from pygments.lexers._luabuiltins import MODULES
            >>> MODULES.keys()
            ['string', 'coroutine', 'modules', 'io', 'basic', ...]
    """

    name = 'Lua'
    aliases = ['lua']
    filenames = ['*.lua', '*.wlua']
    mimetypes = ['text/x-lua', 'application/x-lua']

    tokens = {
        'root': [
            # lua allows a file to start with a shebang
            (r'#!(.*?)$', Comment.Preproc),
            default('base'),
        ],
        'base': [
            (r'(?s)--\[(=*)\[.*?\]\1\]', Comment.Multiline),
            ('--.*$', Comment.Single),

            (r'(?i)(\d*\.\d+|\d+\.\d*)(e[+-]?\d+)?', Number.Float),
            (r'(?i)\d+e[+-]?\d+', Number.Float),
            ('(?i)0x[0-9a-f]*', Number.Hex),
            (r'\d+', Number.Integer),

            (r'\n', Text),
            (r'[^\S\n]', Text),
            # multiline strings
            (r'(?s)\[(=*)\[.*?\]\1\]', String),

            (r'(==|~=|<=|>=|\.\.\.|\.\.|[=+\-*/%^<>#])', Operator),
            (r'[\[\]\{\}\(\)\.,:;]', Punctuation),
            (r'(and|or|not)\b', Operator.Word),

            ('(break|do|else|elseif|end|for|if|in|repeat|return|then|until|'
             r'while)\b', Keyword),
            (r'(local)\b', Keyword.Declaration),
            (r'(true|false|nil)\b', Keyword.Constant),

            (r'(function)\b', Keyword, 'funcname'),

            (r'[A-Za-z_]\w*(\.[A-Za-z_]\w*)?', Name),

            ("'", String.Single, combined('stringescape', 'sqs')),
            ('"', String.Double, combined('stringescape', 'dqs'))
        ],

        'funcname': [
            (r'\s+', Text),
            ('(?:([A-Za-z_]\w*)(\.))?([A-Za-z_]\w*)',
             bygroups(Name.Class, Punctuation, Name.Function), '#pop'),
            # inline function
            ('\(', Punctuation, '#pop'),
        ],

        # if I understand correctly, every character is valid in a lua string,
        # so this state is only for later corrections
        'string': [
            ('.', String)
        ],

        'stringescape': [
            (r'''\\([abfnrtv\\"']|\d{1,3})''', String.Escape)
        ],

        'sqs': [
            ("'", String, '#pop'),
            include('string')
        ],

        'dqs': [
            ('"', String, '#pop'),
            include('string')
        ]
    }

    def __init__(self, **options):
        self.func_name_highlighting = get_bool_opt(
            options, 'func_name_highlighting', True)
        self.disabled_modules = get_list_opt(options, 'disabled_modules', [])

        self._functions = set()
        if self.func_name_highlighting:
            from pygments.lexers._luabuiltins import MODULES
            for mod, func in iteritems(MODULES):
                if mod not in self.disabled_modules:
                    self._functions.update(func)
        RegexLexer.__init__(self, **options)

    def get_tokens_unprocessed(self, text):
        for index, token, value in \
                RegexLexer.get_tokens_unprocessed(self, text):
            if token is Name:
                if value in self._functions:
                    yield index, Name.Builtin, value
                    continue
                elif '.' in value:
                    a, b = value.split('.')
                    yield index, Name, a
                    yield index + len(a), Punctuation, u'.'
                    yield index + len(a) + 1, Name, b
                    continue
            yield index, token, value


class MoonScriptLexer(LuaLexer):
    """
    For `MoonScript <http://moonscript.org>`_ source code.

    .. versionadded:: 1.5
    """

    name = "MoonScript"
    aliases = ["moon", "moonscript"]
    filenames = ["*.moon"]
    mimetypes = ['text/x-moonscript', 'application/x-moonscript']

    tokens = {
        'root': [
            (r'#!(.*?)$', Comment.Preproc),
            default('base'),
        ],
        'base': [
            ('--.*$', Comment.Single),
            (r'(?i)(\d*\.\d+|\d+\.\d*)(e[+-]?\d+)?', Number.Float),
            (r'(?i)\d+e[+-]?\d+', Number.Float),
            (r'(?i)0x[0-9a-f]*', Number.Hex),
            (r'\d+', Number.Integer),
            (r'\n', Text),
            (r'[^\S\n]+', Text),
            (r'(?s)\[(=*)\[.*?\]\1\]', String),
            (r'(->|=>)', Name.Function),
            (r':[a-zA-Z_]\w*', Name.Variable),
            (r'(==|!=|~=|<=|>=|\.\.\.|\.\.|[=+\-*/%^<>#!.\\:])', Operator),
            (r'[;,]', Punctuation),
            (r'[\[\]\{\}\(\)]', Keyword.Type),
            (r'[a-zA-Z_]\w*:', Name.Variable),
            (r"(class|extends|if|then|super|do|with|import|export|"
             r"while|elseif|return|for|in|from|when|using|else|"
             r"and|or|not|switch|break)\b", Keyword),
            (r'(true|false|nil)\b', Keyword.Constant),
            (r'(and|or|not)\b', Operator.Word),
            (r'(self)\b', Name.Builtin.Pseudo),
            (r'@@?([a-zA-Z_]\w*)?', Name.Variable.Class),
            (r'[A-Z]\w*', Name.Class),  # proper name
            (r'[A-Za-z_]\w*(\.[A-Za-z_]\w*)?', Name),
            ("'", String.Single, combined('stringescape', 'sqs')),
            ('"', String.Double, combined('stringescape', 'dqs'))
        ],
        'stringescape': [
            (r'''\\([abfnrtv\\"']|\d{1,3})''', String.Escape)
        ],
        'sqs': [
            ("'", String.Single, '#pop'),
            (".", String)
        ],
        'dqs': [
            ('"', String.Double, '#pop'),
            (".", String)
        ]
    }

    def get_tokens_unprocessed(self, text):
        # set . as Operator instead of Punctuation
        for index, token, value in LuaLexer.get_tokens_unprocessed(self, text):
            if token == Punctuation and value == ".":
                token = Operator
            yield index, token, value


class ChaiscriptLexer(RegexLexer):
    """
    For `ChaiScript <http://chaiscript.com/>`_ source code.

    .. versionadded:: 2.0
    """

    name = 'ChaiScript'
    aliases = ['chai', 'chaiscript']
    filenames = ['*.chai']
    mimetypes = ['text/x-chaiscript', 'application/x-chaiscript']

    flags = re.DOTALL
    tokens = {
        'commentsandwhitespace': [
            (r'\s+', Text),
            (r'//.*?\n', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline),
            (r'^\#.*?\n', Comment.Single)
        ],
        'slashstartsregex': [
            include('commentsandwhitespace'),
            (r'/(\\.|[^[/\\\n]|\[(\\.|[^\]\\\n])*])+/'
             r'([gim]+\b|\B)', String.Regex, '#pop'),
            (r'(?=/)', Text, ('#pop', 'badregex')),
            default('#pop')
        ],
        'badregex': [
            ('\n', Text, '#pop')
        ],
        'root': [
            include('commentsandwhitespace'),
            (r'\n', Text),
            (r'[^\S\n]+', Text),
            (r'\+\+|--|~|&&|\?|:|\|\||\\(?=\n)|\.\.'
             r'(<<|>>>?|==?|!=?|[-<>+*%&\|\^/])=?', Operator, 'slashstartsregex'),
            (r'[{(\[;,]', Punctuation, 'slashstartsregex'),
            (r'[})\].]', Punctuation),
            (r'[=+\-*/]', Operator),
            (r'(for|in|while|do|break|return|continue|if|else|'
             r'throw|try|catch'
             r')\b', Keyword, 'slashstartsregex'),
            (r'(var)\b', Keyword.Declaration, 'slashstartsregex'),
            (r'(attr|def|fun)\b', Keyword.Reserved),
            (r'(true|false)\b', Keyword.Constant),
            (r'(eval|throw)\b', Name.Builtin),
            (r'`\S+`', Name.Builtin),
            (r'[$a-zA-Z_]\w*', Name.Other),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+', Number.Integer),
            (r'"', String.Double, 'dqstring'),
            (r"'(\\\\|\\'|[^'])*'", String.Single),
        ],
        'dqstring': [
            (r'\${[^"}]+?}', String.Iterpol),
            (r'\$', String.Double),
            (r'\\\\', String.Double),
            (r'\\"', String.Double),
            (r'[^\\\\\\"$]+', String.Double),
            (r'"', String.Double, '#pop'),
        ],
    }
