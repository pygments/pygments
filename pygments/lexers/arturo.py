import re

from pygments.lexer import bygroups, default, DelegatingLexer, do_insertions,\
    include, RegexLexer, this, using, words
from pygments.token import Comment, Generic, Keyword, Name, Number, Operator,\
    Other, Punctuation, String, Text

from pygments.util import ClassNotFound, get_bool_opt

class ArturoLexer(RegexLexer):

    name = 'Arturo'
    aliases = ['Arturo', 'Art', 'arturo', 'art']
    filenames = ['*.art']


    def string_end(regex_pattern: str, string_type) -> list[tuple]:
        """
        Usage:
            >>> string_end(r'"', String.Single)
        """
        return (regex_pattern, string_type, '#pop')

    def handle_annotated_strings(self, match):
        """Adds syntax from another languages inside annotated strings

        match args:
            1:open_string,
            2:exclamation_mark,
            3:lang_name,
            4:space_or_newline,
            5:code,
            6:close_string
        """
        from pygments.lexers import get_lexer_by_name

        # Header's section
        yield match.start(1),  String.Double  ,  match.group(1)
        yield match.start(2),  String.Interpol,  match.group(2)
        yield match.start(3),  String.Interpol,  match.group(3)
        yield match.start(4),  Text.Whitespace,  match.group(4)

        lexer = None
        if self.handle_annotateds:
            try:
                lexer = get_lexer_by_name( match.group(3).strip() )
            except ClassNotFound:
                pass
        code = match.group(5)

        if lexer is None:
            yield match.group(5), String, code
        else:
            yield from do_insertions([], lexer.get_tokens_unprocessed(code))

        yield match.start(6), String.Double, match.group(6)



    tokens = {
        'root': [
            include('comments'),
            include('constants'),
            include('builtin_functions'),
            include('operators'),
            (r'.', Name),
        ],

        'comments': [
            (r'(;)(.*)$', Comment.Single),
            include('shebang'),
        ],
            'shebang': [
                (r'^((\s#!)|(#!))(.*)$',
                        Comment.Hashbang)
            ],

        'operators': [
            include('punctuation'),
            include('sugar'),

            (r'<:|:>|:<|:>',    Operator),
            (r'-:|:-',          Operator),

            (r'ø|∞', Operator),
            (r'\@|\#|\$|\%|\&|\_|\!|\!\!',  Operator),
            (r'\+|\-|\*|\~|\=|\>|\<',       Operator),

            (r'==>|<=>|<==>',       Operator),
            (r'=>>|<<=>>|<<==>>',   Operator),
            (r'-->|<->|<-->',       Operator),
            (r'<\\|\<|\>',          Operator),
            (r'\=\||\|\=',          Operator),
            (r'\./|\^|\.\.|\.',     Operator),
            (r'\\',                 Operator),

        ],
            'punctuation': [
                (r'[()[\],]',   Punctuation),
            ],
            'sugar':[
                (r'->', Name.Decorator),
                (r'=>', Name.Decorator),
                (r'\|', Name.Decorator),
                (r'::', Name.Decorator),
            ],

        'constants': [
            include('boolean'),
            include('character'),
            include('color'),
            include('float'),
            include('integer'),
            include('label'),
            include('literal'),
            include('strings'),
            include('type'),
        ],

            'boolean': [
                (words(('false', 'true', 'maybe'), suffix=r'\b'),
                    Name.Constant)
            ],
            'character': [
                (r'`.`', String.Char)
            ],
            'color': [
                (r'#\w+',
                        Name.Constant)
            ],
            'float': [
                (r'[0-9]+\.[0-9]+',
                        Number.Float)
            ],
            'integer': [
                (r'[0-9]+',
                        Number.Integer)
            ],
            'label': [
                (r'\w+\b\??:',
                        Name.Label)
            ],
            'literal': [
                (r'\'(?:\w+\b\??:?)',
                        Keyword.Declaration)
            ],
            'type': [
                (r'\:\w+',
                        Keyword.Type)
            ],
            'attributes': [
                (r'\.\w+',
                        Name.Attribute)
            ],


        'strings': [
            # Single Line Strings
            (r'"',   String.Double, 'inside-simple-string'),
            (r'»',   String.Single, 'inside-smart-string' ),
            (r'«««', String.Double, 'inside-safe-string'  ),
            (r'\{\/', String.Single, 'inside-regex-string'),

            # Multi Line Strings
            (r'\{\:',   String.Double, 'inside-curly-verb-string'),
            (r'(\{)(\!)(\w+)(\s|\n)([\w\W]*?)(^\})',
                                         handle_annotated_strings),
            (r'\{',     String.Single, 'inside-curly-string'     ),
            (r'\-{3,}', String.Single, 'inside-eof-string'       ),
        ],
            'string-basics': [
                include('string-interpol'),
                include('string-escape'),
                include('string-templates'),
            ],
                'string-interpol': [
                    (r'\|',
                        String.Interpol, 'inside-interpol'), # Interpolation
                ],
                    'inside-interpol': [
                        (r'\|', String.Interpol, '#pop'),
                        (r'.', using(this)),
                    ],
                'string-templates': [
                    (r'\<\|\|',
                        String.Interpol, 'inside-template'), # Templates
                ],
                    'inside-template': [
                        (r'\|\|\>',
                            String.Interpol, '#pop'),
                        (r'.', using(this)),
                    ],
                'string-escape': [
                    (r'\\\\', String.Escape), # Escaping backslash
                    (r'\\n',  String.Escape), # Escaping NewLine control
                    (r'\\t',  String.Escape), # Escaping Tabulation control
                    (r'\\"',  String.Escape), # Escaping Quote Character
                ],
            'string-content-single-line': [
                (r'.', String)
            ],
            'string-content-multi-line': [
                (r'[\s\S]', String)
            ],

        'inside-simple-string': [
            include('string-basics'),
            string_end(r'"', String.Double),        # Closing Quote
            include('string-content-single-line')   # String Content
        ],

        'inside-smart-string': [
            include('string-basics'),
            string_end(r'\n', String.Single),       # Closing Quote
            include('string-content-single-line')   # String Content
        ],

        'inside-safe-string': [
            include('string-basics'),
            string_end(r'»»»', String.Double),      # Closing Quote
            include('string-content-single-line')   # String Content
        ],

        'inside-regex-string': [
            include('regex-escapes'),
            include('string-interpol'),
            string_end(r'\/\}', String.Single),     # Closing Quote
            (r'.',              String.Regex ),    # String Content
        ],
            'regex-escapes': [
                (r'\\[sSwWdDbBZApPxucItnvfr0]+',
                                String.Escape),
            ],

        'inside-curly-verb-string': [
            include('string-basics'),
            string_end(r'\:\}', String.Double),     # Closing Quote
            include('string-content-multi-line')    # String Content
        ],

        'inside-curly-string': [
            include('string-basics'),
            string_end(r'\}', String.Single),       # Closing Quote
            include('string-content-multi-line')    # String Content
        ],

        'inside-eof-string': [
            include('string-basics'),
            string_end(r'\Z$', String.Single),       # Closing Quote
            include('string-content-multi-line')    # String Content
        ],

        'builtin_functions': [
            include('builtin-predicate-functions'),
            (words((
                'abs', 'acos', 'acosh', 'acsec', 'acsech', 'actan', 'actanh',
                'add', 'after', 'alphabet', 'and', 'angle', 'append', 'arg',
                'args', 'arity', 'array', 'as', 'asec', 'asech', 'asin',
                'asinh', 'atan', 'atan2', 'atanh', 'attr', 'attrs', 'average',
                'before', 'benchmark', 'blend', 'break', 'builtins1',
                'builtins2', 'call', 'capitalize', 'case', 'ceil', 'chop',
                'chunk', 'clear', 'close', 'cluster', 'color', 'combine',
                'conj', 'continue', 'copy', 'cos', 'cosh', 'couple', 'csec',
                'csech', 'ctan', 'ctanh', 'cursor', 'darken', 'dec', 'decode',
                'decouple', 'define', 'delete', 'desaturate', 'deviation',
                'dictionary', 'difference', 'digest', 'digits', 'div', 'do',
                'download', 'drop', 'dup', 'e', 'else', 'empty', 'encode',
                'ensure', 'env', 'epsilon', 'escape', 'execute', 'exit', 'exp',
                'extend', 'extract', 'factors', 'false', 'fdiv', 'filter',
                'first', 'flatten', 'floor', 'fold', 'from', 'function',
                'gamma', 'gcd', 'get', 'goto', 'hash', 'help', 'hypot', 'if',
                'in', 'inc', 'indent', 'index', 'infinity', 'info', 'input',
                'insert', 'inspect', 'intersection', 'invert', 'join', 'keys',
                'kurtosis', 'last', 'let', 'levenshtein', 'lighten', 'list',
                'ln', 'log', 'loop', 'lower', 'mail', 'map', 'match', 'max',
                'maybe', 'median', 'min', 'mod', 'module', 'mul', 'nand',
                'neg', 'new', 'nor', 'normalize', 'not', 'now', 'null', 'open',
                'or', 'outdent', 'pad', 'panic', 'path', 'pause',
                'permissions', 'permutate', 'pi', 'pop', 'pow', 'powerset',
                'powmod', 'prefix', 'print', 'prints', 'process', 'product',
                'query', 'random', 'range', 'read', 'relative', 'remove',
                'rename', 'render', 'repeat', 'replace', 'request', 'return',
                'reverse', 'round', 'sample', 'saturate', 'script', 'sec',
                'sech', 'select', 'serve', 'set', 'shl', 'shr', 'shuffle',
                'sin', 'sinh', 'size', 'skewness', 'slice', 'sort', 'split',
                'sqrt', 'squeeze', 'stack', 'strip', 'sub', 'suffix', 'sum',
                'switch', 'symbols', 'symlink', 'sys', 'take', 'tan', 'tanh',
                'terminal', 'to', 'true', 'truncate', 'try', 'type', 'union',
                'unique', 'unless', 'until', 'unzip', 'upper', 'values', 'var',
                'variance', 'volume', 'webview', 'while', 'with', 'wordwrap',
                'write', 'xnor', 'xor', 'zip'
            ), prefix=r'\b', suffix=r'\b'), Name.Builtin)
        ],
            'builtin-predicate-functions': [
                (
                    r'\b('
                    r'all|and|any|ascii|attr|attribute|attributeLabel|binary|block'
                    r'|char|contains|database|date|dictionary|empty|equal|even'
                    r'|every|exists|false|floatin|function|greater|'
                    r'|greaterOrEqual|if|in|inline|integer|is|'
                    r'|key|label|leap|less|lessOrEqual|literal|'
                    r'|logical|lower|nand|negative|nor|not'
                    r'|notEqual|null|numeric|odd|or|path'
                    r'|pathLabel|positive|prefix|prime|set|some'
                    r'|sorted|standalone|string|subset|suffix'
                    r'|superset|ymbol|true|try|type|unless|upper'
                    r'|when|whitespace|word|xnor|xor|zero'
                    r')\b\?', Name.Builtin
                ),
            ],

    }

    def __init__(self, **options):
        self.handle_annotateds = get_bool_opt(options, 'handle_annotateds', True)
        RegexLexer.__init__(self, **options)

__all__ = [
    'ArturoLexer'
]