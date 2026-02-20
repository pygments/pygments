"""
    pygments.lexers.linkerscript
    ~~~~~~~~~~~~~~~~~~~

    Lexers for linker scripts.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include
from pygments.token import Text, Name, Number, Comment, Punctuation, \
    Keyword, Operator, Whitespace

__all__ = ['LinkerScriptLexer']


class LinkerScriptLexer(RegexLexer):
    """
    For Linker scripts
    """
    name = 'LinkerScript'
    aliases = ['ld', 'linkerscript', 'lds']
    filenames = ['*.ld', '*.lds']
    mimetypes = ['text/x-lds']
    url = 'https://www.gnu.org/software/binutils'
    version_added = ''

    arg_keywords = ['ENTRY', 'OUTPUT_FORMAT', 'STARTUP', 'SEARCH_DIR', 'INPUT', 'OUTPUT']
    section_keywords = ['KEEP']
    keywords = ['MEMORY', 'SECTIONS']
    memory_attrs = ['ORIGIN', 'LENGTH']
    sections = r'\.\w+'

    tokens = {
        'root': [
            include('comments'),
            ('|'.join(arg_keywords), Name.Function, 'arguments'),
            ('|'.join(keywords), Name.Function),
            (r'[\r\n]+', Text),
            (r'\{', Punctuation, 'block'),
            include('whitespace'),
            include('punctuation'),
        ],
        'arguments': [
            (r'\(', Punctuation),
            (r'\)', Punctuation, '#pop'),
            (r'[_\w./\\-]+', Name.Attribute),
            include('whitespace'),
            include('comments'),
        ],
        'block': [
            include('comments'),
            (r'\b(' + '|'.join(memory_attrs) + r')\b', Keyword.Reserved),
            (sections, Name.Label, 'section_def'),
            (r'[A-Z_][A-Z0-9_]*', Name.Variable),
            (r'\.', Operator),
            (r'=', Operator),
            (r',', Punctuation),
            (r':', Punctuation),
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+[KMG]?', Number.Integer),
            (r'\}', Punctuation, '#pop'),
            (r';', Punctuation),
            include('whitespace'),
        ],
        'section_def': [
            include('comments'),
            (r':', Punctuation, 'section_block'),
            include('whitespace'),
        ],
        'section_block': [
            include('comments'),
            (r'\{', Punctuation, 'section_content'),
            include('whitespace'),
        ],
        'section_content': [
            include('comments'),
            ('|'.join(section_keywords), Name.Function, 'nested_args'),
            (r'\*', Keyword),
            (r'\(', Punctuation),
            (sections, Name.Label),
            (r'\)', Punctuation),
            (r'\}', Punctuation, '#pop:3'),
            include('whitespace'),
        ],
        'nested_args': [
            (r'\(', Punctuation),
            (r'\)', Punctuation, '#pop'),
            (r'\*', Keyword),
            (sections, Name.Label),
            include('whitespace'),
            include('comments'),
        ],
        'comments': [
            (r'/\*', Comment.Multiline, 'comment'),
            (r'//.*?$', Comment.Single),
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
        'whitespace': [
            (r'\n', Whitespace),
            (r'\s+', Whitespace),
        ],
        'punctuation': [
            (r'[-,.()\[\]!]+', Punctuation)
        ]
    }
