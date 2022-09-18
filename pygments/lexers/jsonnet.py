from pygments.lexer import RegexLexer
from pygments.token import (
    Comment,
    Keyword,
    Name,
    Number,
    Operator,
    Punctuation,
    String,
    Whitespace,
)

__all__ = ['JsonnetLexer']

jsonnet_token_chars = r'[_A-Za-z0-9]'
jsonnet_token = r'[_A-Za-z]' + jsonnet_token_chars + '*'
jsonnet_function_token = jsonnet_token + r'(?=\()'


comments = [
    (r'//.*\n', Comment.Single),
    (r'#.*\n', Comment.Single),
    (r'/\*\*([^/]|/(?!\*))*\*/', String.Doc),
    (r'/\*([^/]|/(?!\*))*\*/', Comment),
]


whitespace = (r'[\n ]+', Whitespace)


keywords = '|'.join([
    'assert', 'else', 'error', 'false', 'for', 'if', 'import', 'importstr',
    'in', 'null', 'tailstrict', 'then', 'self', 'super', 'true',
])


rvalues = comments + [
    (r"@'.*'", String),
    (r'@".*"', String),
    (r"'", String, 'singlestring'),
    (r'"', String, 'doublestring'),
    (r'(?s:\|\|\|.*\|\|\|)', String),
    (r'[+-]?[0-9]+(.[0-9])?', Number.Float),
    # Omit : despite spec because it appears to be used as a field separator
    (r'[!$~+\-&|^=<>*/%]', Operator),
    (r'[{]', Punctuation, 'object'),
    (r'\[', Punctuation, 'array'),
    (r'local', Keyword, ('local_name')),
    (r'assert', Keyword, 'assert'),
    (fr'({keywords})(?!{jsonnet_token_chars})', Keyword),
    whitespace,
    (r'function(?=\()', Keyword, 'function_params'),
    (r'std\.' + jsonnet_function_token, Name.Builtin, 'function_args'),
    (jsonnet_function_token, Name.Function, 'function_args'),
    (jsonnet_token, Name.Variable),
    (r'[\.()]', Punctuation),
]


def string_rules(quote_mark):
    return [
        (r"[^{}\\]".format(quote_mark), String),
        (r"\\.", String.Escape),
        (quote_mark, String, '#pop'),
    ]


def quoted_field_name(quote_mark):
    return [
        (r'([^{quote}\\]|\\.)*{quote}'.format(quote=quote_mark),
         Name.Variable, 'field_separator')
    ]


class JsonnetLexer(RegexLexer):
    name = 'Jsonnet'
    aliases = ['jsonnet']
    filenames = ['*.jsonnet', '*.libsonnet']
    tokens = {
        'root': rvalues,
        'singlestring': string_rules("'"),
        'doublestring': string_rules('"'),
        'array': [
            (r',', Punctuation),
            (r'\]', Punctuation, '#pop'),
        ] + rvalues,
        'local_name': [
            (jsonnet_function_token, Name.Function, 'function_params'),
            (jsonnet_token, Name.Variable),
            whitespace,
            ('(?==)', Whitespace, ('#pop', 'local_value')),
        ],
        'local_value': [
            (r'=', Operator),
            (r';', Punctuation, '#pop'),
        ] + rvalues,
        'assert': [
            (r':', Punctuation),
            (r';', Punctuation, '#pop'),
        ] + rvalues,
        'function_params': [
            (jsonnet_token, Name.Variable),
            (r'\(', Punctuation),
            (r'\)', Punctuation, '#pop'),
            (r',', Punctuation),
            whitespace,
            (r'=', Operator, 'function_param_default'),
        ],
        'function_args': [
            (r'\(', Punctuation),
            (r'\)', Punctuation, '#pop'),
            (r',', Punctuation),
            whitespace,
        ] + rvalues,
        'object': [
            whitespace,
            (r'local', Keyword, 'object_local_name'),
            ('assert', Keyword, 'object_assert'),
            (r'\[', Operator, 'field_name_expr'),
            (fr'(?={jsonnet_token})', Name.Variable, 'field_name'),
            (r'}', Punctuation, '#pop'),
            (r'"', Name.Variable, 'double_field_name'),
            (r"'", Name.Variable, 'single_field_name'),
        ] + comments,
        'field_name': [
            (jsonnet_function_token, Name.Function,
                ('field_separator', 'function_params')
             ),
            (jsonnet_token, Name.Variable, 'field_separator'),
        ],
        'double_field_name': quoted_field_name('"'),
        'single_field_name': quoted_field_name("'"),
        'field_name_expr': [
            (r'\]', Operator, 'field_separator'),
        ] + rvalues,
        'function_param_default': [
            (r'(?=[,\)])', Whitespace, '#pop')
        ] + rvalues,
        'field_separator': [
            whitespace,
            (r'\+?::?:?', Punctuation, ('#pop', '#pop', 'field_value')),
        ] + comments,
        'field_value': [
            (r',', Punctuation, '#pop'),
            (r'}', Punctuation, '#pop:2'),
        ] + rvalues,
        'object_assert': [
            (r':', Punctuation),
            (r',', Punctuation, '#pop'),
        ] + rvalues,
        'object_local_name': [
            (jsonnet_token, Name.Variable, ('#pop', 'object_local_value')),
            whitespace,
        ],
        'object_local_value': [
            (r'=', Operator),
            (r',', Punctuation, '#pop'),
            (r'}', Punctuation, '#pop:2'),
        ] + rvalues,
    }
