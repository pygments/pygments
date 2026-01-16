"""
    pygments.lexers.swan
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for the Swan language.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, default, words
from pygments import token
from pygments.token import Keyword, Name, Text

__all__ = ['SwanLexer']


class SwanLexer(RegexLexer):
    """
    Lexer for the Swan programming language.
    """

    name = 'Swan'
    url = 'https://github.com/ansys/DevRelPublic/blob/main/Downloads/ScadeOne/SwanPrimer_Rev_2_1.pdf'
    aliases = ['swan', 'scade one']
    filenames = ['*.swan', '*.swani', '*.swant']
    version_added = '2.20'

    tokens = {
        'root': [
            (r'[^\S\n]+', Text),
            (words((
                'activate',
                'elsif', 'end', 'every',
                'fold', 'foldi', 'forward',
                'map', 'mapfold', 'mapfoldi', 'mapi', 'match',
                'restart', 'resume',
                'unless', 'until',
                'when',
            ), suffix=r'\b'), token.Keyword.Builtin),
            (words((
                'assert', 'assume', 'automaton',
                'block',
                'const',
                'def', 'diagram',
                'emit', 'expr',
                'function',
                'group', 'guarantee',
                'let',
                'node',
                'returns',
                'sensor', 'specialize', 'state',
                'type',
                'var',
                'where', 'wire',
            ), suffix=r'\b'), token.Keyword.Declaration),
            (
                r'^\s*(use)(\s*)((?:\w+::)*)(\w+)',
                bygroups(Keyword.Namespace, Text.Whitespace, Name.Namespace, Name),
            ),
            (words((
                'clock',
                'initial', 'inline',
            ), suffix=r'\b'), token.Keyword.Namespace),
            (words((
                'false',
                'true',
            ), suffix=r'\b'), token.Name.Builtin),
            (words((
                'and', 'as',
                'byname', 'bypos',
                'case',
                'default',
                'else',
                'flatten',
                'if',
                'land', 'last', 'lnot', 'lor', 'lsl', 'lsr', 'lxor',
                'merge',
                'mod',
                'not',
                'of', 'or',
                'pack', 'pre',
                'reverse',
                'self',
                'then', 'transpose',
                'window', 'with',
                'xor',
            ), suffix=r'\b'), token.Operator.Word),
            (words((
                'bool',
                'char',
                'enum',
                'float', 'float32', 'float64',
                'int8', 'int16', 'int32', 'int64', 'integer',
                'numeric',
                'uint8', 'uint16', 'uint32', 'uint64', 'unsigned',
                'signed',
            ), suffix=r'\b'), token.Keyword.Type),
            (words((
                'abstract',
                'do',
                'fby', 'final', 'foldw', 'foldwi', 'forwardi',
                'imported', 'is',
                'make',
                'mapfoldw', 'mapfoldwi', 'mapw', 'mapwi',
                'onreset', 'open',
                'package', 'parameter', 'probe', 'private', 'public',
                'repeat', 'repeati',
                'sig', 'synchro',
                'tel', 'times',
            ), suffix=r'\b'), token.Keyword.Reserved),
            (words((
                '_harness', '_oracle', '_sensor', '_source'
            ), suffix=r'\b'), token.Keyword.Pseudo),
            (r'#pragma', token.Comment.Preproc, 'pragma'), # must be before LUID
            (r'/\*', token.Comment.Multiline, 'comment'),  # must be before operators
            (r'<<', token.Generic.Strong, 'chevron'),      # must be before operators
            (r'--.*$', token.Comment.Singleline),          # must be before operators
            (r"\{[a-zA-Z0-9_]*%", token.Comment.Special, 'markup'),
            (r'\+|->|-|\*|\/|\^|=|<>|<=?|>=?|@|:>', token.Operator),
            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?\d+)?', token.Number.Float, 'number_lit'), # TYPED_FLOAT
            (r'0b[01]+', token.Number.Bin, 'number_lit'),        # TYPED_INTEGER
            (r'0o[0-7]+', token.Number.Oct, 'number_lit'),       # TYPED_INTEGER
            (r'0x[0-9A-Fa-f]+', token.Number.Hex, 'number_lit'), # TYPED_INTEGER
            (r'\d+', token.Number.Integer, 'number_lit'),        # TYPED_INTEGER
            (r'#[\w\-]+', token.Name.Variable),                  # LUID
            (r'\'[\w]+', token.Name.Label),                      # NAME
            (
                r'([a-zA-Z]\w*::)?([a-zA-Z]\w*)',
                bygroups(token.Name.Namespace, token.Name),      # path_id
            ),
            (r':|\(|\)|\.|,|;|\{|\}', token.Punctuation),
            (r'\s', token.Text),
            (r'\S', token.Text),
        ],
        'number_lit': [
            (r'_u?i(8|16|32|64)', token.Keyword, '#pop'),
            (r'_f(32|64)', token.Keyword, '#pop'),
            default('#pop'),
        ],
        'comment': [
            (r'[^*/]+', token.Comment.Multiline),
            (r'/\*', token.Comment.Multiline, '#push'),
            (r'\*/', token.Comment.Multiline, '#pop'),
            (r'[*/]', token.Comment.Multiline),
        ],
        'pragma': [
            (r'[^#]+', token.Comment.Preproc),
            (r'##', token.Comment.Preproc),
            (r'#end', token.Comment.Preproc, '#pop'),
            (r'#', token.Comment.Preproc),
        ],
        'chevron': [
            (r'>>', token.Generic.Strong, '#pop'),
            (r'.', token.Generic.Strong),
        ],
        'markup': [
            (r'%[a-zA-Z0-9_]*\}', token.Comment.Special, '#pop'),
            (r'[^%]+', token.Comment.Special),
            (r'.', token.Comment.Special),
        ],
    }
