"""
    pygments.lexers.vyper
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Vyper Smart Contract language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import (Comment, String, Name, Keyword, Number,
                            Operator, Punctuation, Text, Whitespace)

__all__ = ['VyperLexer']


class VyperLexer(RegexLexer):
    """For the Vyper smart contract language.
    """
    name = 'Vyper'
    aliases = ['vyper']
    filenames = ['*.vy']
    url = "https://docs.vyperlang.org"
    version_added = '2.17'

    tokens = {
        'root': [
            # Whitespace
            (r'\s+', Whitespace),

            # Line continuations
            (r'(\\)(\n|\r\n|\r)', bygroups(Text, Whitespace)),

            # Pragma
            (r'#\s*pragma\b.*$', Comment.Preproc),

            # Comments - inline and multiline
            (r'#.*$', Comment.Single),
            (r'\"\"\"', Comment.Multiline, 'multiline-comment'),

            # Strings - single and double
            (r"b'", String.Single, 'single-string'),
            (r'b"', String.Double, 'double-string'),
            (r"'", String.Single, 'single-string'),
            (r'"', String.Double, 'double-string'),

            # Hex string literals
            (r'0x"[0-9a-fA-F]*"', String),

            # Functions
            (r'(def)(\s+)([a-zA-Z_][a-zA-Z0-9_]*)',
             bygroups(Keyword, Whitespace, Name.Function)),

            # Event, Struct, Interface, Flag, Enum definitions and log statements
            (r'(event|struct|interface|flag|enum|log)(\s+)([a-zA-Z_][a-zA-Z0-9_]*)',
             bygroups(Keyword, Whitespace, Name.Class)),

            # Imports - general form
            (r'(from)(\s+)([\w.]+)(\s+)(import)(\s+)(\w+)',
             bygroups(Keyword, Whitespace, Name.Namespace, Whitespace,
                      Keyword, Whitespace, Name.Class)),

            # Numeric Literals
            (r'\b0x[0-9a-fA-F_]+\b', Number.Hex),
            (r'\b0b[01_]+\b', Number.Bin),
            (r'\b0o[0-7_]+\b', Number.Oct),
            (r'\b\d[\d_]*\.\d[\d_]*\b', Number.Float),
            (r'\b\d[\d_]*\b', Number.Integer),

            # Boolean and None constants
            (words(('True', 'False'), prefix=r'\b', suffix=r'\b'),
             Keyword.Constant),

            # Keywords
            (words((
                'def', 'event', 'pass', 'return', 'for', 'while',
                'if', 'elif', 'else', 'assert', 'raise',
                'import', 'from', 'as', 'in', 'not',
                'and', 'or',
                'struct', 'interface', 'flag', 'enum',
                'implements', 'initializes', 'uses', 'exports',
                'indexed', 'log',
                'extcall', 'staticcall', 'delegatecall',
                'break', 'continue',
                'UNREACHABLE',
            ), prefix=r'\b', suffix=r'\b'), Keyword),

            # Visibility, State Mutability and Storage
            (words((
                'public', 'external', 'internal', 'deploy',
                'view', 'pure', 'payable', 'nonpayable',
                'constant', 'immutable', 'transient',
            ), prefix=r'\b', suffix=r'\b'), Keyword.Declaration),

            # Built-in Functions
            (words((
                # Crypto
                'keccak256', 'sha256', 'ecrecover', 'ecadd', 'ecmul',
                'method_id',
                # Type conversion
                'convert', 'as_wei_value', 'uint2str',
                # Array/String
                'len', 'slice', 'concat', 'extract32',
                # Math
                'floor', 'ceil', 'sqrt', 'isqrt', 'abs',
                'min', 'max', 'shift',
                'min_value', 'max_value', 'epsilon',
                'pow_mod256', 'uint256_addmod', 'uint256_mulmod',
                # Unsafe math
                'unsafe_add', 'unsafe_sub', 'unsafe_mul', 'unsafe_div',
                # ABI
                'abi_encode', 'abi_decode',
                '_abi_encode', '_abi_decode',
                # Contract creation
                'raw_create', 'create_minimal_proxy_to',
                'create_forwarder_to', 'create_copy_of',
                'create_from_blueprint',
                # Raw operations
                'raw_call', 'raw_log', 'raw_revert',
                # EVM
                'blockhash', 'blobhash', 'send', 'selfdestruct',
                # Utilities
                'empty', 'range', 'print', 'breakpoint',
            ), prefix=r'\b', suffix=r'\b'), Name.Builtin),

            # Built-in Variables and Attributes
            (words((
                'msg.sender', 'msg.value', 'msg.data',
                'msg.gas', 'msg.mana',
                'block.timestamp', 'block.number',
                'block.coinbase', 'block.difficulty',
                'block.prevrandao', 'block.gaslimit',
                'block.basefee', 'block.blobbasefee',
                'block.prevhash',
                'tx.origin', 'tx.gasprice',
                'chain.id',
            ), prefix=r'\b', suffix=r'\b'), Name.Builtin.Pseudo),

            # Types
            (words((
                # Unsigned integers (all sizes)
                'uint8', 'uint16', 'uint24', 'uint32', 'uint40', 'uint48',
                'uint56', 'uint64', 'uint72', 'uint80', 'uint88', 'uint96',
                'uint104', 'uint112', 'uint120', 'uint128', 'uint136',
                'uint144', 'uint152', 'uint160', 'uint168', 'uint176',
                'uint184', 'uint192', 'uint200', 'uint208', 'uint216',
                'uint224', 'uint232', 'uint240', 'uint248', 'uint256',
                # Signed integers (all sizes)
                'int8', 'int16', 'int24', 'int32', 'int40', 'int48',
                'int56', 'int64', 'int72', 'int80', 'int88', 'int96',
                'int104', 'int112', 'int120', 'int128', 'int136',
                'int144', 'int152', 'int160', 'int168', 'int176',
                'int184', 'int192', 'int200', 'int208', 'int216',
                'int224', 'int232', 'int240', 'int248', 'int256',
                # Fixed bytes
                'bytes1', 'bytes2', 'bytes3', 'bytes4', 'bytes5',
                'bytes6', 'bytes7', 'bytes8', 'bytes9', 'bytes10',
                'bytes11', 'bytes12', 'bytes13', 'bytes14', 'bytes15',
                'bytes16', 'bytes17', 'bytes18', 'bytes19', 'bytes20',
                'bytes21', 'bytes22', 'bytes23', 'bytes24', 'bytes25',
                'bytes26', 'bytes27', 'bytes28', 'bytes29', 'bytes30',
                'bytes31', 'bytes32',
                # Other primitives
                'bool', 'decimal', 'address', 'bytes', 'string', 'String',
                # Composite types
                'DynArray', 'HashMap',
            ), prefix=r'\b', suffix=r'\b'), Keyword.Type),

            # Parameterized type: Bytes[N], String[N]
            (r'\b(Bytes|String)(\[)(\d+)(\])',
             bygroups(Keyword.Type, Punctuation, Number.Integer, Punctuation)),

            # indexed keyword with type
            (r'\b(indexed)\b(\s*)(\()(\s*)(\w+)(\s*)(\))',
             bygroups(Keyword, Whitespace, Punctuation, Whitespace,
                      Keyword.Type, Punctuation)),

            # Operators and Punctuation
            (r'(\*\*|//|<<|>>|:=|<=|>=|==|!=|[+\-*/%&|^~<>=!])', Operator),
            (r'[.,:;()\[\]{}]', Punctuation),

            # Decorators
            (r'@[\w.]+', Name.Decorator),

            # Dunder methods
            (r'__\w+__', Name.Magic),

            # self
            (r'\bself\b', Name.Builtin.Pseudo),

            # Generic names and variables
            (r'\b[a-zA-Z_]\w*\b:', Name.Variable),
            (r'\b[A-Z][A-Z_0-9]*\b', Name.Constant),
            (r'\b[a-zA-Z_]\w*\b', Name),
        ],

        'multiline-comment': [
            (r'\"\"\"', Comment.Multiline, '#pop'),
            (r'[^"]+', Comment.Multiline),
            (r'\"', Comment.Multiline)
        ],

        'single-string': [
            (r"[^\\']+", String.Single),
            (r"'", String.Single, '#pop'),
            (r'\\.', String.Escape),
        ],

        'double-string': [
            (r'[^\\"]+', String.Double),
            (r'"', String.Double, '#pop'),
            (r'\\.', String.Escape),
        ]
    }
