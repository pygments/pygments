from pygments.lexer import RegexLexer, bygroups
from pygments.token import *
from pygments.formatters import Terminal256Formatter


class VyperLexer(RegexLexer):
    name = 'Vyper'
    aliases = ['vyper']
    filenames = ['*.vy']

    tokens = {
        'root': [
            # Comments
            (r'#.*$', Comment.Single),                 # Single-line comment
            (r'\"\"\"', Comment.Multiline, 'multiline-comment'),  # Start multiline comment

            # Single- and Double-quoted String
            (r"'([^'\\]*(?:\\.[^'\\]*)*)'", String.Single),
            (r'"([^"\\]*(?:\\.[^"\\]*)*)"', String.Double),

            # Built-in Functions
            (r'\b(bitwise_and|bitwise_not|bitwise_or|bitwise_xor|shift|'
             r'create_minimal_proxy_to|create_copy_of|create_from_blueprint|'
             r'ecadd|ecmul|ecrecover|keccak256|sha256|'
             r'concat|convert|uint2str|extract32|slice|'
             r'abs|ceil|floor|max|max_value|min|min_value|pow_mod256|sqrt|isqrt|uint256_addmod|'
             r'uint256_mulmod|unsafe_add|unsafe_sub|unsafe_mul|unsafe_div|'
             r'as_wei_value|blockhash|empty|len|method_id|_abi_encode|_abi_decode|print)\b', Name.Builtin),

            # Decorators
            (r'@\w+\(\'[^\']*\'\)', Name.Decorator),    # Decorators with arguments
            (r'@\w+', Name.Decorator),                  # Simple decorators

            # Keywords
            (r'\b(def|event|pass|return|for|while|if|elif|else|assert|raise|import|in|struct|implements|interface|from)\b', Keyword),

            # Visibility and State Mutability
            (r'\b(public|private|view|pure|constant|immutable)\b', Keyword.Declaration),

            # Constants and Attributes
            (r'\bEMPTY_BYTES32\b', Name.Constant),
            (r'\bself\.\b', Name.Attribute),

            # Numeric Literals
            (r'\b0x[0-9a-fA-F]+\b', Number.Hex),       # Hexadecimal literals
            (r'\b\d+\.\d*\b', Number.Float),           # Floating point numbers
            (r'\b\d+\b', Number.Integer),              # Integers

            # Types and Declarations
            (r'\b(u?int\d+)\b', Keyword.Type),
            (r'\bbool\b', Keyword.Type),
            (r'\bdecimal\b', Keyword.Type),
            (r'\bbytes\d{1,4}\b', Keyword.Type),
            (r'\b(?:string|String)\[\d+\]\b', Keyword.Type),
            (r'\b(?:string|String)\b', Keyword.Type),
            (r'\b(u?int\d+)\[\d*\]\b', Keyword.Type),
            (r'\bmap\b', Keyword.Type),
            (r'\baddress\b', Keyword.Type),
            (r'\bbytes\b', Keyword.Type),
            (r'\benum\b', Keyword.Type),
            (r'\bstruct\b', Keyword.Type),

            # Other declarations
            (r'\binterface\b', Keyword.Declaration),

            # Operators and Punctuation
            (r'(\+|\-|\*|\/|<=?|>=?|==|!=)', Operator),
            (r'[,:;()\[\]{}]', Punctuation),

            # Built-in Variables and Attributes
            (r'\b(msg\.sender|msg\.value|block\.timestamp|block\.number|msg\.gas)\b', Name.Builtin.Pseudo),

            # Special Constructs
            (r'__\w+__', Name.Magic),
            (r'\bevent\b \w+', Name.Namespace),
            (r'\bstruct\b \w+', Name.Class),

            # Identifiers
            (r'\b[a-zA-Z_]\w*\b', Name),

            # Whitespace
            (r'\s+', Text.Whitespace),
        ],

        'multiline-comment': [
            (r'\"\"\"', Comment.Multiline, '#pop'),  # End multiline comment
            (r'.|\n', Comment.Multiline)              # Multiline comment content
        ]
    }

__all__ = ['VyperLexer']
