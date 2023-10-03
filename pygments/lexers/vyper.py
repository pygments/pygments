"""
    pygments.lexers.vyper
    ~~~~~~~~~~~~~~~~~~~~~
    
    Lexer for the Vyper Smart Contract language.

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups
from pygments.token import (Comment, String, Name, Keyword, Number,
                            Operator, Punctuation, Text, Error)

class VyperLexer(RegexLexer):
    name = 'Vyper'
    aliases = ['vyper']
    filenames = ['*.vy']
    url = "https://vyper.readthedocs.io/"


    tokens = {
        'root': [
            # Whitespace
            (r'\s+', Text.Whitespace),

            # Comments
            (r'#.*$', Comment.Single),
            (r'\"\"\"', Comment.Multiline, 'multiline-comment'),

            # Strings
            (r"'([^'\\]*(?:\\.[^'\\]*)*)'", String.Single),
            (r'"([^"\\]*(?:\\.[^"\\]*)*)"', String.Double),

            # Numeric Literals
            (r'\b0x[0-9a-fA-F]+\b', Number.Hex),
            (r'\b\d{1,3}(?:_\d{3})*\b', Number.Integer),
            (r'\b\d+\.\d*\b', Number.Float),

            # Keywords
            (r'\b(def|event|pass|return|for|while|if|elif|else|assert|raise|import|in|struct|implements|interface|from)\b', Keyword),

            # Visibility and State Mutability
            (r'\b(public|private|view|pure|constant|immutable)\b', Keyword.Declaration),

            # Operators and Punctuation
            (r'(\+|\-|\*|\/|<=?|>=?|==|!=|=)', Operator),
            (r'(?<=[^\s:]):(?=[^\s:])', Operator),  # Match colons surrounded by non-space characters
            (r':', Operator),  # Match standalone colons
            (r'[,:;()\[\]{}]', Punctuation),

            # Built-in Functions
            (r'\b(bitwise_and|bitwise_not|bitwise_or|bitwise_xor|shift|'
             r'create_minimal_proxy_to|create_copy_of|create_from_blueprint|'
             r'ecadd|ecmul|ecrecover|keccak256|sha256|'
             r'concat|convert|uint2str|extract32|slice|'
             r'abs|ceil|floor|max|max_value|min|min_value|pow_mod256|sqrt|isqrt|'
             r'uint256_addmod|uint256_mulmod|unsafe_add|unsafe_sub|unsafe_mul|unsafe_div|'
             r'as_wei_value|blockhash|empty|len|method_id|_abi_encode|_abi_decode|print|range)\b', Name.Builtin),

            # Built-in Variables and Attributes
            (r'\b(msg\.sender|msg\.value|block\.timestamp|block\.number|msg\.gas)\b', Name.Builtin.Pseudo),

            # Other variable names and types
            (r'@internal', Name.Decorator),
            (r'@[\w.]+', Name.Decorator),
            (r'__\w+__', Name.Magic),  # Matches double underscores followed by word characters
            (r'EMPTY_BYTES32', Name.Constant),
            (r'self\.', Name.Attribute),
            (r'ERC20', Name.Class),
            (r'log', Keyword),

            (r'\b(?:u?int\d+|bool|decimal|bytes\d{1,4}|string|String|address|bytes|enum|struct)\b', Keyword.Type),

            (r'\binterface\b', Keyword.Declaration),
            (r'\bfrom\b', Keyword.Namespace, 'importfrom'),

            # Match function and event signatures
            (r'\b[a-zA-Z_]\w*\b\([a-zA-Z_]\w*: [a-zA-Z_]\w*\):', Name.Function),
            (r'\b(?:event|struct)\b \w+', Name.Class, 'body'),

            # Generic names and variables
            (r'\b[a-zA-Z_]\w*\b:', Name.Variable),
            (r'\b[a-zA-Z_]\w*\b', Name),

            # Error for unmatched sequences
            (r'.', Text)
        ],

        'multiline-comment': [
            (r'\"\"\"', Comment.Multiline, '#pop'),
            (r'.|\n', Comment.Multiline)
        ],

        'importfrom': [
            (r'\bimport\b', Keyword.Namespace, 'import'),
            (r'\s+', Text.Whitespace),
            (r'[a-zA-Z_][\w.]*', Name.Namespace),
            (r'\s*as\s*', Keyword.Namespace),
            (r'[a-zA-Z_]\w*', Name.Namespace, '#pop')
        ],

        'import': [
            (r'([a-zA-Z_][\w.]*)(\s+as\s+)?([a-zA-Z_]\w*)?',
            bygroups(Name.Namespace, Keyword.Namespace, Name.Namespace)),
            (r'\s+', Text.Whitespace),
            (r',', Punctuation),
            (r';?', Punctuation, '#pop')
        ],

        'body': [
            (r'\(', Punctuation, 'funcparams'),
            (r':', Punctuation),
            (r';', Punctuation, '#pop'),
            (r'\s+', Text.Whitespace),
            (r'#.*$', Comment.Single)
        ],

        'funcparams': [
            (r'\)', Punctuation, '#pop'),
            (r'\w+', Name.Variable),
            (r':', Punctuation),
            (r'\w+', Keyword.Type),
            (r',', Punctuation),
            (r'\s+', Text.Whitespace),
        ]
    }

__all__ = ['VyperLexer']
