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
            (r'(?<=[^\s:]):(?=[^\s:])', Operator),  # Match colons surrounded by non-space characters
            (r':', Operator),  # Match standalone colons
            
            # Comments
            (r'#.*$', Comment.Single),                 
            (r'\"\"\"', Comment.Multiline, 'multiline-comment'),  


            # Importing interfaces
            (r'\bfrom\b', Keyword.Namespace, 'importfrom'),

            # Additional tokens
            (r'@internal', Name.Decorator),
            (r'ERC20', Name.Class),
            (r'log', Keyword),

            # Events and structs
            (r'\bevent\b \w+', Name.Namespace, 'body'),
            (r'\bstruct\b \w+', Name.Class, 'body'),

            # Other variable names
            (r'\b[a-zA-Z_][a-zA-Z0-9_]*\b(?!:)', Name.Variable),
            (r'_\w+', Name.Variable),  # Fallback rule for underscores followed by characters


            (r'\b[a-zA-Z_][a-zA-Z0-9_]*\b', Name.Variable),
            (r'\b[a-zA-Z_][a-zA-Z_0-9]*\b', Name),

            # Variables with types
            (r'\b[a-zA-Z_][a-zA-Z0-9_]*\b', Name.Variable),
            (r'\b\w+\s*:', Name.Variable, 'type'),


            # Strings
            (r"'([^'\\]*(?:\\.[^'\\]*)*)'", String.Single),
            (r'"([^"\\]*(?:\\.[^"\\]*)*)"', String.Double),

            # Decorators
            (r'@\w+\(\'[^\']*\'\)', Name.Decorator),    
            (r'@\w+', Name.Decorator),  

            # Special Constructs
            (r'__\w+__', Name.Magic),  # Matches double underscores followed by word characters
                

            # Keywords
            (r'\b(def|event|pass|return|for|while|if|elif|else|assert|raise|import|in|struct|implements|interface|from)\b', Keyword),

            # Visibility and State Mutability
            (r'\b(public|private|view|pure|constant|immutable)\b', Keyword.Declaration),

            # Constants and Attributes
            (r'\bEMPTY_BYTES32\b', Name.Constant),
            (r'\bself\.\b', Name.Attribute),

            # Numeric Literals
            (r'\b0x[0-9a-fA-F]+\b', Number.Hex),
            (r'\b\d{1,3}(?:_\d{3})*\b', Number.Integer),
            (r'\b\d+\.\d*\b', Number.Float),

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
            (r'(\+|\-|\*|\/|<=?|>=?|==|!=|=)', Operator),
            (r'[,:;()\[\]{}]', Punctuation),

            # Built-in Functions
            (r'\b(bitwise_and|bitwise_not|bitwise_or|bitwise_xor|shift|'
             r'create_minimal_proxy_to|create_copy_of|create_from_blueprint|'
             r'ecadd|ecmul|ecrecover|keccak256|sha256|'
             r'concat|convert|uint2str|extract32|slice|'
             r'abs|ceil|floor|max|max_value|min|min_value|pow_mod256|sqrt|isqrt|uint256_addmod|'
             r'uint256_mulmod|unsafe_add|unsafe_sub|unsafe_mul|unsafe_div|'
             r'as_wei_value|blockhash|empty|len|method_id|_abi_encode|_abi_decode|print|range)\b', Name.Builtin),

            # Built-in Variables and Attributes
            (r'\b(msg\.sender|msg\.value|block\.timestamp|block\.number|msg\.gas)\b', Name.Builtin.Pseudo),

            # Special Constructs
            (r'__\w+__', Name.Magic),

            # Identifiers
            (r'\b[a-zA-Z_]\w*\b', Name),
            (r'\b[a-zA-Z_]\w*\b:', Name),

            (r'\b[a-zA-Z_]\w*\b\([a-zA-Z_]\w*: [a-zA-Z_]\w*\):', Name.Function), 
            (r'\b[a-zA-Z_]\w*\b', Name),

            # Whitespace
            (r'\s+', Text.Whitespace),

            # Unrecognized characters
            (r'.', Text),

            # Error for unmatched sequences
            (r'(?P<error>.{1,10})', bygroups(Error)),
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
        ],

        'type': [
            (r'\b(u?int\d+|bool|decimal|bytes\d{1,4}|string|String|address|bytes)\b', 
            Keyword.Type, '#pop'),
            (r'\s+', Text.Whitespace)
        ]
    }

__all__ = ['VyperLexer']