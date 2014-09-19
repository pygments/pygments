# -*- coding: utf-8 -*-
"""
    pygments.lexers.configs
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for configuration file formats.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, default, words, bygroups
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

__all__ = ['KconfigLexer', 'Cfengine3Lexer']


def _rx_indent(level):
    # Kconfig *always* interprets a tab as 8 spaces, so this is the default.
    # Edit this if you are in an environment where KconfigLexer gets expanded
    # input (tabs expanded to spaces) and the expansion tab width is != 8,
    # e.g. in connection with Trac (trac.ini, [mimeviewer], tab_width).
    # Value range here is 2 <= {tab_width} <= 8.
    tab_width = 8
    # Regex matching a given indentation {level}, assuming that indentation is
    # a multiple of {tab_width}. In other cases there might be problems.
    return r'(?:\t| {1,%s}\t| {%s}){%s}.*\n' % (tab_width-1, tab_width, level)


class KconfigLexer(RegexLexer):
    """
    For Linux-style Kconfig files.

    .. versionadded:: 1.6
    """

    name = 'Kconfig'
    aliases = ['kconfig', 'menuconfig', 'linux-config', 'kernel-config']
    # Adjust this if new kconfig file names appear in your environment
    filenames = ['Kconfig', '*Config.in*', 'external.in*',
                 'standard-modules.in']
    mimetypes = ['text/x-kconfig']
    # No re.MULTILINE, indentation-aware help text needs line-by-line handling
    flags = 0

    def call_indent(level):
        # If indentation >= {level} is detected, enter state 'indent{level}'
        return (_rx_indent(level), String.Doc, 'indent%s' % level)

    def do_indent(level):
        # Print paragraphs of indentation level >= {level} as String.Doc,
        # ignoring blank lines. Then return to 'root' state.
        return [
            (_rx_indent(level), String.Doc),
            (r'\s*\n', Text),
            default('#pop:2')
        ]

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'#.*?\n', Comment.Single),
            (words((
                'mainmenu', 'config', 'menuconfig', 'choice', 'endchoice',
                'comment', 'menu', 'endmenu', 'visible if', 'if', 'endif',
                'source', 'prompt', 'select', 'depends on', 'default',
                'range', 'option'), suffix=r'\b'),
             Keyword),
            (r'(---help---|help)[\t ]*\n', Keyword, 'help'),
            (r'(bool|tristate|string|hex|int|defconfig_list|modules|env)\b',
             Name.Builtin),
            (r'[!=&|]', Operator),
            (r'[()]', Punctuation),
            (r'[0-9]+', Number.Integer),
            (r"'(''|[^'])*'", String.Single),
            (r'"(""|[^"])*"', String.Double),
            (r'\S+', Text),
        ],
        # Help text is indented, multi-line and ends when a lower indentation
        # level is detected.
        'help': [
            # Skip blank lines after help token, if any
            (r'\s*\n', Text),
            # Determine the first help line's indentation level heuristically(!).
            # Attention: this is not perfect, but works for 99% of "normal"
            # indentation schemes up to a max. indentation level of 7.
            call_indent(7),
            call_indent(6),
            call_indent(5),
            call_indent(4),
            call_indent(3),
            call_indent(2),
            call_indent(1),
            ('', Text, '#pop'),  # for incomplete help sections without text
        ],
        # Handle text for indentation levels 7 to 1
        'indent7': do_indent(7),
        'indent6': do_indent(6),
        'indent5': do_indent(5),
        'indent4': do_indent(4),
        'indent3': do_indent(3),
        'indent2': do_indent(2),
        'indent1': do_indent(1),
    }


class Cfengine3Lexer(RegexLexer):
    """
    Lexer for `CFEngine3 <http://cfengine.org>`_ policy files.

    .. versionadded:: 1.5
    """

    name = 'CFEngine3'
    aliases = ['cfengine3', 'cf3']
    filenames = ['*.cf']
    mimetypes = []

    tokens = {
        'root': [
            (r'#.*?\n', Comment),
            (r'(body)(\s+)(\S+)(\s+)(control)',
             bygroups(Keyword, Text, Keyword, Text, Keyword)),
            (r'(body|bundle)(\s+)(\S+)(\s+)(\w+)(\()',
             bygroups(Keyword, Text, Keyword, Text, Name.Function, Punctuation),
             'arglist'),
            (r'(body|bundle)(\s+)(\S+)(\s+)(\w+)',
             bygroups(Keyword, Text, Keyword, Text, Name.Function)),
            (r'(")([^"]+)(")(\s+)(string|slist|int|real)(\s*)(=>)(\s*)',
             bygroups(Punctuation, Name.Variable, Punctuation,
                      Text, Keyword.Type, Text, Operator, Text)),
            (r'(\S+)(\s*)(=>)(\s*)',
             bygroups(Keyword.Reserved, Text, Operator, Text)),
            (r'"', String, 'string'),
            (r'(\w+)(\()', bygroups(Name.Function, Punctuation)),
            (r'([\w.!&|\(\)]+)(::)', bygroups(Name.Class, Punctuation)),
            (r'(\w+)(:)', bygroups(Keyword.Declaration, Punctuation)),
            (r'@[\{\(][^\)\}]+[\}\)]', Name.Variable),
            (r'[(){},;]', Punctuation),
            (r'=>', Operator),
            (r'->', Operator),
            (r'\d+\.\d+', Number.Float),
            (r'\d+', Number.Integer),
            (r'\w+', Name.Function),
            (r'\s+', Text),
        ],
        'string': [
            (r'\$[\{\(]', String.Interpol, 'interpol'),
            (r'\\.', String.Escape),
            (r'"', String, '#pop'),
            (r'\n', String),
            (r'.', String),
        ],
        'interpol': [
            (r'\$[\{\(]', String.Interpol, '#push'),
            (r'[\}\)]', String.Interpol, '#pop'),
            (r'[^\$\{\(\)\}]+', String.Interpol),
        ],
        'arglist': [
            (r'\)', Punctuation, '#pop'),
            (r',', Punctuation),
            (r'\w+', Name.Variable),
            (r'\s+', Text),
        ],
    }
