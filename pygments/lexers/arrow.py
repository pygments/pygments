# -*- coding: utf-8 -*-
"""
    pygments.lexers.tnt
    ~~~~~~~~~~~~~~~~~~~

    Lexer for Typographic Number Theory.

    :copyright: Copyright 2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Text, Operator, Keyword, Punctuation, Name, \
     Whitespace, String, Number

__all__ = ['ArrowLexer']

TYPES = r'\b(int|bool|char)((?:\[\])*)(?=\s+)'
IDENT = r'([a-zA-Z_][a-zA-Z0-9_]*)'
DECL = TYPES + '(\s+)' + IDENT

class ArrowLexer(RegexLexer):
    """
    Lexer for Arrow: https://pypi.org/project/py-arrow-lang/
    Example code:
    https://github.com/Kenny2github/py-arrow-lang#example-arrow-program
    """

    name = 'Arrow'
    aliases = ['arrow']
    filenames = ['*.arw']

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'^[|\s]+', Punctuation),
            include('blocks'),
            include('statements'),
            include('expressions'),
        ],
        'blocks': [
            (r'(function)(\n+)(/-->)(\s*)'
             + DECL # 4 groups
             + r'(\()', bygroups(
                 Keyword.Reserved, Text, Punctuation,
                 Text, Keyword.Type, Punctuation, Text,
                 Name.Function, Punctuation
             ), 'fparams'),
            (r'/-->$|\\-->$|/--<|\\--<|\^', Punctuation),
        ],
        'statements': [
            (DECL, bygroups(Keyword.Type, Punctuation, Text, Name.Variable)),
            (r'\[', Punctuation, 'index'),
            (r'=', Operator),
            (r'require|main', Keyword.Reserved),
            (r'print', Keyword.Reserved, 'print'),
        ],
        'expressions': [
            (r'\s+', Text),
            (r'[0-9]+', Number.Integer),
            (r'true|false', Keyword.Constant),
            (r"'", String.Char, 'char'),
            (r'"', String.Double, 'string'),
            (r'{', Punctuation, 'array'),
            (r'==|!=|<|>|\+|-|\*|/|%', Operator),
            (r'and|or|not|length', Operator.Word),
            (r'(input)(\s+)(int|char\[\])', bygroups(
                Keyword.Reserved, Text, Keyword.Type
            )),
            (IDENT + r'(\()', bygroups(
                Name.Function, Punctuation
            ), 'fargs'),
            (IDENT, Name.Variable),
            (r'\[', Punctuation, 'index'),
            (r'\(', Punctuation, 'expressions'),
            (r'\)', Punctuation, '#pop'),
        ],
        'print': [
            include('expressions'),
            (r',', Punctuation),
            (r'', Text, '#pop'),
        ],
        'fparams': [
            (DECL, bygroups(Keyword.Type, Punctuation, Text, Name.Variable)),
            (r',', Punctuation),
            (r'\)', Punctuation, '#pop'),
        ],
        'escape': [
            (r'\\(["\\/abfnrtv]|[0-9]{1,3}|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4})',
             String.Escape),
        ],
        'char': [
            (r"'", String.Char, '#pop'),
            include('escape'),
            (r"[^'\\]", String.Char),
        ],
        'string': [
            (r'"', String.Double, '#pop'),
            include('escape'),
            (r'[^"\\]+', String.Double),
        ],
        'array': [
            include('expressions'),
            (r'\}', Punctuation, '#pop'),
            (r',', Punctuation),
        ],
        'fargs': [
            include('expressions'),
            (r'\)', Punctuation, '#pop'),
            (r',', Punctuation),
        ],
        'index': [
            include('expressions'),
            (r'\]', Punctuation, '#pop'),
        ],
    }
