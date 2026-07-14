"""
    pygments.lexers.kumir
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for Kumir.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, words
from pygments.token import Text, Comment, Keyword, Name, String, Whitespace, Operator, Number

__all__ = ['KumirLexer']


class KumirLexer(RegexLexer):
    """
    Pygments Lexer for Kumir source files (.kum).
    """

    name = 'Kumir'
    aliases = ['kumir', 'кумир']
    filenames = ['*.kum']
    mimetypes = ['text/kumir']
    url = 'https://www.niisi.ru/kumir/'
    version_added = '2.0'

    flags = re.IGNORECASE | re.MULTILINE

    flowControl = (
        'если', 'то', 'иначе', 'все',
        'выбор', 'при',
        'нц', 'кц',  'для', 'от', 'до', 'шаг', 'пока',
    )
    types = (
        'цел', 'вещ', 'лог', 'сим', 'лит', 'таб',
    )
    keywords = (
        'алг', 'нач', 'кон', 'дано', 'надо', 'утв',
        'арг', 'рез', 'знач',
        'нс',
        'использовать', 'пауза',
        'документ',
    )
    operations = (
        'и', 'или', 'не', 'да', 'нет',
    )
    functions = (
        # Mathematical
        'sqrt', 'abs', 'sin', 'cos', 'tg', 'exp', 'ln',
        'int', 'окр', 'rnd', 'mod', 'div', 'max', 'min',
        # String
        'длин', 'asc', 'chr', 'mid', 'left', 'right',
        'str', 'val', 'pos',
        # Arrays
        'таб', 'копия', 'конкат',
        # Input/Output
        'ввод', 'вывод', 'нс',
        # System
        'пауза', 'случай', 'выход',
        # Type Conversion
        'цел', 'вещ', 'лог'
    )
    operators = r'(\+|-|\*|/|=|>|<|==|<=|>=|!=|:=|\.\.|,|;|:|\(|\)|\[|\]|\^|%)'

    tokens = {
        'root': [
            (r'\|.*?$', Comment.Single),
            # Flow Control.
            (words(flowControl, prefix=r'\b', suffix=r'\b'), Keyword),
            # Types.
            (words(types, prefix=r'\b', suffix=r'\b'), Keyword.Type),
            # Keywords.
            (words(keywords, prefix=r'\b', suffix=r'\b'), Keyword.Reserved),
            # Built-in operations.
            (words(operations, prefix=r'\b', suffix=r'\b'), Name.Class),
            # Built-in functions.
            (words(functions, prefix=r'\b', suffix=r'\b'), Name.Function),
            (operators, Operator),
            (r'"(""|[^"])*"', String),
            (r'\d+(_\d+)*\.?\d*([eE][+-]?\d+)?', Number),
            (r'[а-яА-Яa-zA-Z_][а-яА-Яa-zA-Z0-9_]*', Name.Variable),
            (r'\s+', Text.Whitespace)
        ],
    }
