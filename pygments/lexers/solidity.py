"""
    pygments.lexers.solidity
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Solidity.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace

__all__ = ['SolidityLexer']

DECIMAL_DIGITS = r'[0-9](?:_?[0-9])*'

class SolidityLexer(RegexLexer):
    """
    For Solidity source code.
    """

    name = 'Solidity'
    aliases = ['solidity']
    filenames = ['*.sol']
    mimetypes = []
    url = 'https://soliditylang.org'
    version_added = '2.5'

    datatype = (
        r'\b('
        r'address(?:\s+payable)?|'
        r'bool|'
        r'bytes(?:1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|'
        r'17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|32)?|'
        r'int(?:8|16|24|32|40|48|56|64|72|80|88|96|104|112|120|128|'
        r'136|144|152|160|168|176|184|192|200|208|216|224|232|240|248|256)?|'
        r'uint(?:8|16|24|32|40|48|56|64|72|80|88|96|104|112|120|128|'
        r'136|144|152|160|168|176|184|192|200|208|216|224|232|240|248|256)?|'
        r'string'
        r')\b'
    )

    tokens = {
        'root': [
            include('whitespace'),
            include('comments'),
            (r'\bpragma\s+solidity\b', Keyword, 'pragma'),
            (r'\b(contract)(\s+)([a-zA-Z_]\w*)',
             bygroups(Keyword, Whitespace, Name.Entity)),
            (datatype + r'(\s+)((?:external|public|internal|private)\s+)?' +
             r'([a-zA-Z_]\w*)',
             bygroups(Keyword.Type, Whitespace, Keyword, Name.Variable)),
            (r'\b(enum|event|function|struct)(\s+)([a-zA-Z_]\w*)',
             bygroups(Keyword.Type, Whitespace, Name.Variable)),
            (r'\b(msg|block|tx)\.([A-Za-z_][a-zA-Z0-9_]*)\b', Keyword),
            (words((
                'abstract',
                'address',
                'after',
                'alias',
                'anonymous',
                'apply',
                'as',
                'assembly',
                'auto',
                'bool',
                'break',
                'byte',
                'bytes',
                'calldata',
                'case',
                'catch',
                'constant',
                'constructor',
                'continue',
                'contract',
                'copyof',
                'days',
                'default',
                'define',
                'delete',
                'do',
                'else',
                'emit',
                'enum',
                'ether',
                'event',
                'external',
                'fallback',
                'false',
                'final',
                'for',
                'function',
                'gwei',
                'hex',
                'hours',
                'if',
                'immutable',
                'implements',
                'import',
                'in',
                'indexed',
                'inline',
                'interface',
                'internal',
                'is',
                'let',
                'library',
                'macro',
                'mapping',
                'match',
                'memory',
                'minutes',
                'modifier',
                'mutable',
                'new',
                'null',
                'of',
                'override',
                'partial',
                'payable',
                'pragma',
                'private',
                'promise',
                'public',
                'pure',
                'receive',
                'reference',
                'relocatable',
                'return',
                'returns',
                'sealed',
                'seconds',
                'sizeof',
                'static',
                'storage',
                'string',
                'struct',
                'supports',
                'switch',
                'true',
                'try',
                'type',
                'typedef',
                'typeof',
                'unchecked',
                'unicode',
                'using',
                'var',
                'view',
                'virtual',
                'weeks',
                'wei',
                'while',
                'years'               
             ), prefix=r'\b', suffix=r'\b'),
             Keyword.Type),
            (words(('keccak256',), prefix=r'\b', suffix=r'\b'), Name.Builtin),
            (datatype, Keyword.Type),
            include('constants'),
            (r'[a-zA-Z_]\w*', Text),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'[.;{}(),\[\]]', Punctuation)
        ],
        'comments': [
            (r'//(\n|[\w\W]*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?[*][\w\W]*?[*](\\\n)?/', Comment.Multiline),
            (r'/(\\\n)?[*][\w\W]*', Comment.Multiline)
        ],
        'constants': [
            (r'("(\\"|.)*?")', String.Double),
            (r"('(\\'|.)*?')", String.Single),
            (r'\b0[xX][0-9a-fA-F](?:_?[0-9a-fA-F])*\b', Number.Hex),
            (
                r'\b' + DECIMAL_DIGITS +
                r'(?:\.' + DECIMAL_DIGITS + r')?' +
                r'(?:[eE]-?' + DECIMAL_DIGITS + r')?\b',
                Number.Decimal,
            )
        ],
        'pragma': [
            include('whitespace'),
            include('comments'),
            (r'(\^|>=|<)(\s*)(\d+\.\d+\.\d+)',
             bygroups(Operator, Whitespace, Keyword)),
            (r';', Punctuation, '#pop')
        ],
        'whitespace': [
            (r'\s+', Whitespace),
            (r'\n', Whitespace)
        ]
    }
