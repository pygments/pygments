"""
    pygments.lexers.cue
    ~~~~~~~~~~~~~~~~~~

    Lexer for the Cuelang language.
"""

from pygments.lexer import RegexLexer, bygroups, words, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace, Error

__all__ = ['CueLexer']


class CueLexer(RegexLexer):
    """
    For Cuelang source.
    """
    name = 'Cue'
    url = 'https://cuelang.org/'
    filenames = ['*.cue']
    aliases = ['cue', 'cuelang', 'cue.mod']
    mimetypes = ['text/x-cuesrc']
    version_added = '1.0'

    tokens = {
        'root': [
            include('comment'),
            include('whitespace'),
            (r'\b(package)([ \t]+)([a-zA-Z\$\#][\w\$\#]*)\b', bygroups(Keyword.Namespace, Whitespace, Name.Namespace)),
            (r'\b(import)([ \t]+)(\()', bygroups(Keyword.Namespace, Whitespace, Punctuation.Marker), 'import'),
            (r'\b(import)([ \t]+)(?:([a-zA-Z\$\#][\w\$\#]*)([ \t]+))?(")([^:"]+)(?:(:)([a-zA-Z\$\#][\w\$\#]*))?(")', bygroups(Keyword.Namespace, Whitespace, Name.Namespace, Whitespace, Punctuation.Marker, String, Punctuation.Marker, Name.Namespace, Punctuation.Marker)),
            include('punctuation_comma'),
            include('declaration'),
            include('invalid_in_braces')
        ],
        'declaration': [
            (r'(@)([a-zA-Z\$\#][\w\$\#]*|_[\w\$\#]+)(\()', bygroups(Punctuation.Marker, Name, Punctuation.Marker), 'parameters'),
            (r'(?<!:)::(?!:)', Punctuation),
            include('punctuation_colon'),
            (r'\?', Punctuation),
            (r'(?<![=!><])=(?![=~])', Punctuation),
            (r'<-', Punctuation),
            include('expression')
        ],
        'parameters': [
            (r'\)', Punctuation.Marker, '#pop'),
            include('punctuation_comma'),
            include('whitespace'),
            include('attribute_element'),
        ],
        'expression': [
            #for - loop syntax
            (r'\b(for)([ \t]+)([a-zA-Z\$\#][\w\$\#]*|_[\w\$\#]+)(?:([ \t]*)(,)([ \t]*)([a-zA-Z\$\#][\w\$\#]*|_[\w\$\#]+))?([ \t]+)(in)\b',
             bygroups(Keyword, Whitespace, Name, Whitespace, Punctuation.Marker, Whitespace, Name, Whitespace, Keyword)),
            #for - standalone keyword
            (r'\bfor\b', Keyword),
            #if
            (r'\bif\b', Keyword),
            #in - standalone keyword 
            (r'\bin\b', Keyword),
            #let - assignment syntax
            (r'\b(let)([ \t]+)([a-zA-Z\$\#][\w\$\#]*|_[\w\$\#]+)([ \t]*)(=)(?![=])', bygroups(Keyword, Whitespace, Name, Whitespace, Punctuation)),
            #let - standalone keyword
            (r'\blet\b', Keyword),
            #Arithmetic
            (r'[\+\-\*]|/(?![/*])', Operator),
            #Arithmetic
            (r'\b(?:div|mod|quo|rem)\b', Operator.Word),
            #Comparison
            (r'=[=~]|![=~]|<=|>=|[<](?![<-=])|[>](?![>=])', Operator),
            #Logical
            (r'&{2}|\|{2}|!(?![=~])', Operator),
            #Set
            (r'&(?!&)|\|(?!\|)', Operator),
            #Accessor
            (r'(?<!\.)(\.)([a-zA-Z\$][\w\$]*)\b', bygroups(Operator, Name.Property)),
            (r'(?<!\.)(\.)(\#[\w\$\#]*)\b', bygroups(Operator, Name.Entity)),
            (r'(?<!\.)(\.)(_[\w\$\#]+)\b', bygroups(Operator, Name.Variable.Magic)),
            # _
            (r'\b_(?!\|)\b', Operator),
            # _|_
            (r'\b_\|_\b', Operator),
            #null
            (r'\bnull\b', Keyword.Constant),
            #booléen
            (r'\b(?:true|false)\b', Keyword.Constant),
            #float
            (r'(?<![\w\.])[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?(?:[eE][\+\-]?[0-9](?:_?[0-9])*)?(?![\w\.])', Number.Float),
            (r'(?<![\w\.])[0-9](?:_?[0-9])*[eE][\+\-]?[0-9](?:_?[0-9])*(?![\w\.])', Number.Float),
            (r'(?<![\w\.])\.[0-9](?:_?[0-9])*(?:[eE][\+\-]?[0-9](?:_?[0-9])*)?(?![\w\.])', Number.Float),
            #integer other
            (r'(?<![\w\.])(?:0|[1-9](?:_?[0-9])*)(?:\.[0-9](?:_?[0-9])*)?(?:[KMGTPEYZ]i?)(?![\w\.])', Number.Integer),
            (r'(?<![\w\.])\.[0-9](?:_?[0-9])*(?:[KMGTPEYZ]i?)(?![\w\.])', Number.Integer),
            (r'(?<![\w\.])(?:0|[1-9](?:_?[0-9])*)(?![\w\.])', Number.Integer),
            (r'(?<![\w\.])0b[0-1](?:_?[0-1])*(?![\w\.])', Number.Bin),
            (r'(?<![\w\.])0[xX][0-9a-fA-F](?:_?[0-9a-fA-F])*(?![\w\.])', Number.Hex),
            (r'(?<![\w\.])0o?[0-7](?:_?[0-7])*(?![\w\.])', Number.Oct),
            #string
            include('string'),
            #types supportés
            (r'\b(?:bool|u?int(?:8|16|32|64|128)?|float(?:32|64)?|string|bytes|number|rune)\b', Keyword.Type),
            #functions du langage
            (r'\b(len|close|and|or)(\()', bygroups(Name.Function, Punctuation.Marker), 'function'),
            (r'([a-zA-Z\$\#][\w\$\#]*)(\.)([A-Z][\w\$\#]*)(\()', bygroups(Name.Namespace, Punctuation.Marker, Name.Namespace, Punctuation.Marker), 'function'),
            #variables
            (r'([a-zA-Z\$][\w\$]*)\b', Name.Property),
            #definitions
            (r'(\#[\w]+)\b', Name.Entity),
            #hidden fields
            (r'(_[\w]+)\b', Name.Variable.Magic),
            #block
            (r'\{', Punctuation.Marker, 'block'),
            #markup entangled
            (r'(<<)([^>]+)(>>)', bygroups(Punctuation.Marker, Name.Label, Punctuation.Marker)),
            #brackets
            (r'\[', Punctuation.Marker, 'bracket'),
            #brackets
            (r'\(', Punctuation.Marker, 'parenthesis'),
        ],
        'function': [
            (r'\)', Punctuation.Marker, '#pop'),
            include('whitespace'),
            include('comment'),
            include('punctuation_comma'),
            include('expression'),
            include('invalid_in_parens')
        ],
        'block': [
            (r'\}', Punctuation.Marker, '#pop'),
            include('whitespace'),
            include('comment'),
            include('punctuation_comma'),
            include('punctuation_ellipsis'),
            include('declaration'),
            include('invalid_in_braces')
        ],
        'bracket': [
            (r'\]', Punctuation.Marker, '#pop'),
            include('whitespace'),
            include('comment'),
            include('punctuation_colon'),
            include('punctuation_comma'),
            include('punctuation_ellipsis'),
            (r'([a-zA-Z\$\#][\w\$\#]*|_[\w\$\#]+)[ \t]*(=)', bygroups(Name.Variable, Punctuation)),
            include('expression'),
            (r'[^\]]+', Error)
        ],
        'parenthesis': [
            (r'\)', Punctuation.Marker, '#pop'),
            include('whitespace'),
            include('comment'),
            include('punctuation_comma'),
            include('expression'),
            include('invalid_in_parens')
        ],
        'attribute_element': [
            (r'([a-zA-Z\$\#][\w\$\#]*|_[\w\$\#]+)(=)', bygroups(Name.Variable, Punctuation), 'attribute_element_content'),
            (r'([a-zA-Z\$\#][\w\$\#]*|_[\w\$\#]+)(\()', bygroups(Name.Variable, Punctuation), 'attribute_element_content2'),
            include('attribute_string')
        ],
        'attribute_element_content': [
            (r'(?=[,\)])', bygroups(None), '#pop'),
            include('attribute_string')
        ],
        'attribute_element_content2': [
            (r'\)', Punctuation, '#pop'),
            include('punctuation_comma'),
            include('attribute_element'),
        ],
        'attribute_string': [
            include('string'),
            (r'[^\n,\"\'#=\(\)]+', String),
            (r'[^,\)]+', Error)
        ],
        'whitespace': [
            (r'[ \t\r\n]+', Whitespace),
        ],
        'comment': [
            (r'//(.*?)$', Comment.Single),
        ],
        'invalid_in_parens': [
            (r'[^)]+', Error),
        ],
        'invalid_in_braces': [
            (r'[^}]+', Error),
        ],
        'punctuation_comma': [
            (r',', Punctuation),
        ],
        'punctuation_colon': [
            (r'(?<!:):(?!:)', Punctuation.Marker),
        ],
        'punctuation_ellipsis': [
            (r'(?<!\.)\.{3}(?!\.)', Punctuation),
        ],
        'string': [
            (r'#"""', String.Delimiter, 'string_sharp_triple_double_quote'),
            (r'#"', String.Delimiter, 'string_sharp_double_quote'),
            (r"#'''", String.Delimiter, 'string_sharp_triple_single_quote'),
            (r"#'", String.Delimiter, 'string_sharp_single_quote'),
            (r'"""', String.Delimiter, 'string_triple_double_quote'),
            (r'"', String.Delimiter, 'string_double_quote'),
            (r"'''", String.Delimiter, 'string_triple_single_quote'),
            (r"'", String.Delimiter, 'string_single_quote'),
            (r"`[^`]*`", String.Backtick),
        ],
        'string_sharp_triple_double_quote': [
            (r'"""#', String.Delimiter, '#pop'),
            (r'\\#(?:"""|/|\\|[abfnrtv]|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})', String.Escape),
            (r'\\#(?:[0-7]{3}|x[0-9A-Fa-f]{2})', Error),
            (r'\\#\(', Punctuation, 'interpolation'),
            (r'\\#.', Error),
            (r'[^"\\]+|"(?!""#)', String.Double)
        ],
        'string_sharp_double_quote': [
            (r'"#', String.Delimiter, '#pop'),
            (r'\\#(?:"|/|\\|[abfnrtv]|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})', String.Escape),
            (r'\\#(?:[0-7]{3}|x[0-9A-Fa-f]{2})', Error),
            (r'\\#\(', Punctuation, 'interpolation'),
            (r'\\#.', Error),
            (r'[^"\\]+|"(?!#)', String.Double)
        ],
        'string_sharp_triple_single_quote': [
            (r"'''#", String.Delimiter, '#pop'),
            (r"\\#(?:'''|/|\\|[abfnrtv]|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})", String.Escape),
            (r'\\#(?:[0-7]{3}|x[0-9A-Fa-f]{2})', Error),
            (r'\\#\(', Punctuation, 'interpolation'),
            (r'\\#.', Error),
            (r"[^'\\]+|'(?!''#)", String.Single)
        ],
        'string_sharp_single_quote': [
            (r"'#", String.Delimiter, '#pop'),
            (r"\\#(?:'|/|\\|[abfnrtv]|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})", String.Escape),
            (r'\\#(?:[0-7]{3}|x[0-9A-Fa-f]{2})', Error),
            (r'\\#\(', Punctuation, 'interpolation'),
            (r'\\#.', Error),
            (r'[^\'\\]+', String.Single)
        ],
        'string_triple_double_quote': [
            (r'"""', String.Delimiter, '#pop'),
            (r'\\(?:"""|/|\\|[abfnrtv]|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})', String.Escape),
            (r'\\(?:[0-7]{3}|x[0-9A-Fa-f]{2})', Error),
            (r'\\\(', Punctuation, 'interpolation'),
            (r'\\.', Error),
            (r'[^"\\]+|"(?!"")', String.Double)
        ],
        'string_double_quote': [
            (r'"', String.Delimiter, '#pop'),
            (r'\\(?:"|/|\\|[abfnrtv]|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})', String.Escape),
            (r'\\(?:[0-7]{3}|x[0-9A-Fa-f]{2})', Error),
            (r'\\\(', Punctuation, 'interpolation'),
            (r'\\.', Error),
            (r'[^"\\]+', String.Double)
        ],
        'string_triple_single_quote': [
            (r"'''", String.Delimiter, '#pop'),
            (r"\\(?:'''|/|\\|[abfnrtv]|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})", String.Escape),
            (r'\\(?:[0-7]{3}|x[0-9A-Fa-f]{2})', Error),
            (r'\\\(', Punctuation, 'interpolation'),
            (r'\\.', Error),
            (r"[^'\\]+|'(?!'')", String.Single)
        ],
        'string_single_quote': [
            (r"'", String.Delimiter, '#pop'),
            (r"\\(?:'|/|\\|[abfnrtv]|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})", String.Escape),
            (r'\\(?:[0-7]{3}|x[0-9A-Fa-f]{2})', Error),
            (r'\\\(', Punctuation, 'interpolation'),
            (r'\\.', Error),
            (r"[^'\\]+", String.Single)
        ],

        'interpolation': [
            (r'\)', Punctuation, '#pop'),
            include('whitespace'),
            include('expression'),
            include('invalid_in_parens')
        ],

        'import': [
            (r'\)', Punctuation.Marker, '#pop'),
            include('whitespace'),
            include('comment'),
            include('punctuation_comma'),
            (r'((?:[a-zA-Z\$\#][\w\$\#]*)[ \t]+)?(")([^:"]+)(?:(:)([a-zA-Z\$\#][\w\$\#]*))?(")', bygroups(Name.Namespace, Punctuation.Marker, String, Punctuation.Marker, Name.Namespace, Punctuation.Marker)),
            (r';', Punctuation),
            include('invalid_in_parens'),
        ]
    }
