from pygments.lexer import RegexLexer, bygroups, include, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Whitespace, Punctuation

__all__ = ['FuncLexer']


class FuncLexer(RegexLexer):
    name = 'FunC'
    aliases = ['func', 'fc']
    filenames = ['*.fc', '*.func']

    identifier_allowed_symbols = r'([^;,\[\]\(\)\s~.\{\}]+)'
    # 1. Does not start from "
    # 2. Can start from ` and end with `, containing any character
    # 3. Starts with underscore 
    # 4. Starts with letter, contains letters, numbers and underscores
    identifier = '(?!")(`([^`]+)`|(?=_)(_{})|(?![_`]){})'.format(identifier_allowed_symbols, identifier_allowed_symbols)

    tokens = {
        'root': [
            (r'\n', Whitespace),
            (r'\s+', Whitespace),

            include('keywords'),
            include('strings'),
            include('directives'),
            include('numeric'),
            include('comments'),
            include('storage'),
            include('functions'),
            include('variables'),
        ],
        'keywords': [
            (words((
                '<=>', '>=', '<=', '!=', '==', '^>>', '~>>',
                '>>', '<<', '/%', '^%', '~%', '^/', '~/', '+=',
                '-=', '*=', '/=', '~/=', '^/=', '%=', '^%=', '<<=',
                '>>=', '~>>=', '^>>=', '&=', '|=', '^=', '^', '=', 
                '~', '/', '%', '-', '*', '+','>', 
                '<', '&', '|', ':', '?'), prefix=r'(?<=\s)', suffix=r'(?=\s)'), 
             Operator),
            (words((
                'if', 'ifnot', 
                'else', 'elseif', 'elseifnot', 
                'while', 'do', 'until', 'repeat', 
                'return', 'impure', 'method_id', 
                'forall', 'asm', 'inline', 'inline_ref'), prefix=r'\b', suffix=r'\b'), 
             Keyword),
            (words(('true', 'false'), prefix=r'\b', suffix=r'\b'), Keyword.Constant),
        ],
        'directives': [
            (r'#include|#pragma', Keyword, 'directive'),
        ],
        'directive': [
            include('strings'),
            (r'version|not-version', Keyword),
            (r'(>=|<=|=|>|<|\^)?([0-9]+)(.[0-9]+)?(.[0-9]+)?', Number), # version
            (r';', Text, '#pop')
        ],
        'strings': [
            (r'\"([^\n\"]+)\"[Hhcusa]?', String),
        ],
        'numeric': [
            (r'\b(-?(?!_)([\d_]+|0x[\d_a-fA-F]+)|0b[1_0]+)(?<!_)\b', Number)
        ],
        'comments': [
            (r';;([^\n]*)', Comment.Singleline),
            (r'{-', Comment.Multiline, 'comment'),
        ],
        'comment': [
            (r'[^-}]+', Comment.Multiline),
            (r'{-', Comment.Multiline, '#push'),
            (r'-}', Comment.Multiline, '#pop'),
            (r'[-}]', Comment.Multiline),
        ],
        'storage': [
            (words((
                'var', 'int', 'slice', 'tuple', 
                'cell', 'builder', 'cont', '_'), prefix=r'\b', suffix=r'(?=[\s\(\),\[\]])'), 
             Keyword.Type),
            (words(('global', 'const'), prefix='\b', suffix='\b'), Keyword.Constant),
        ],
        'variables': [
            (identifier, Name.Variable),
        ],
        'functions': [
            # identifier followed by (
            (identifier + r'(?=[\(])', Name.Function),
        ]
    }
