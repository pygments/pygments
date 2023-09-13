"""
    pygments.lexers.prql
    ~~~~~~~~~~~~~~~~~~~

    Lexer for the PRQL query language.

    :copyright: Copyright 2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, combined, words, include, bygroups
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Text, Whitespace

__all__ = ['PrqlLexer']


class PrqlLexer(RegexLexer):
    """
    For PRQL source code.

    .. versionadded:: 2.16.2
    
    grammar: https://github.com/PRQL/prql/tree/main/grammars
    """

    name = 'PRQL'
    url = 'https://prql-lang.org/'
    aliases = ['prql']
    filenames = ['*.prql']
    mimetypes = ['application/prql', 'application/x-prql']
    
    builtinTypes = words((
        "bool",
        "int",
        "int8", "int16", "int32", "int64",
        "float",
        "text",
        "set"), suffix=r'\b')

    def innerstring_rules(ttype):
        return [
            # the new style '{}'.format(...) string formatting
            (r'\{'
             r'((\w+)((\.\w+)|(\[[^\]]+\]))*)?'  # field name
             r'(\![sra])?'                       # conversion
             r'(\:(.?[<>=\^])?[-+ ]?#?0?(\d+)?,?(\.\d+)?[E-GXb-gnosx%]?)?'
             r'\}', String.Interpol),

            (r'[^\\\'"%{\n]+', ttype),
            (r'[\'"\\]', ttype),
            (r'%|(\{{1,2})', ttype)
        ]

    def fstring_rules(ttype):
        return [
            (r'\}', String.Interpol),
            (r'\{', String.Interpol, 'expr-inside-fstring'),
            (r'[^\\\'"{}\n]+', ttype),
            (r'[\'"\\]', ttype),
        ]

    tokens = {
        'root': [

            # Comments
            (r'#!.*', Comment.Comment.Special),
            (r'#.*', Comment.Single),

            # Whitespace
            (r'\s+', Whitespace),

            # Modules
            (r'^(\s*)(module)(\s*)', bygroups(Whitespace, Keyword.Namespace,
                Whitespace), 'imports'),
            
            (builtinTypes, Keyword.Type),

            # Main
            (r'^prql ', Keyword.Reserved),

            # Variable Names
            (r'^[A-Za-z_][a-zA-Z0-9_]*', Keyword),

            (r'[-~+/*%=<>&^|.@]', Operator),
            (r'->|=>|==|!=|>=|<=|~=|&&|\|\||\?\?|\/\/', Operator),
            (r'[]{}:(),;[]', Punctuation),

            include('keywords'),
            include('expr'),
        ],
        'expr': [
            # non-raw f-strings
            ('(f)(""")', bygroups(String.Affix, String.Double),
             combined('fstringescape', 'tdqf')),
            ("(f)(''')", bygroups(String.Affix, String.Single),
             combined('fstringescape', 'tsqf')),
            ('(f)(")', bygroups(String.Affix, String.Double),
             combined('fstringescape', 'dqf')),
            ("(f)(')", bygroups(String.Affix, String.Single),
             combined('fstringescape', 'sqf')),

            # non-raw s-strings
            ('(s)(""")', bygroups(String.Affix, String.Double),
             combined('stringescape', 'tdqf')),
            ("(s)(''')", bygroups(String.Affix, String.Single),
             combined('stringescape', 'tsqf')),
            ('(s)(")', bygroups(String.Affix, String.Double),
             combined('stringescape', 'dqf')),
            ("(s)(')", bygroups(String.Affix, String.Single),
             combined('stringescape', 'sqf')),

            # raw strings
            ('(?i)(r)(""")',
             bygroups(String.Affix, String.Double), 'tdqs'),
            ("(?i)(r)(''')",
             bygroups(String.Affix, String.Single), 'tsqs'),
            ('(?i)(r)(")',
             bygroups(String.Affix, String.Double), 'dqs'),
            ("(?i)(r)(')",
             bygroups(String.Affix, String.Single), 'sqs'),

            # non-raw strings
            ('"""', String.Double, combined('stringescape', 'tdqs')),
            ("'''", String.Single, combined('stringescape', 'tsqs')),
            ('"', String.Double, combined('stringescape', 'dqs')),
            ("'", String.Single, combined('stringescape', 'sqs')),

            (r'[^\S\n]+', Text),
            include('functions'),
            include('numbers'),

            # Variable Names
            (r'[A-Za-z_][a-zA-Z0-9_]*', Name.Variable),
        ],
        'numbers': [
            (r'(\d(?:_?\d)*\.(?:\d(?:_?\d)*)?|(?:\d(?:_?\d)*)?\.\d(?:_?\d)*)'
             r'([eE][+-]?\d(?:_?\d)*)?', Number.Float),
            (r'\d(?:_?\d)*[eE][+-]?\d(?:_?\d)*j?', Number.Float),
            (r'0[oO](?:_?[0-7])+', Number.Oct),
            (r'0[bB](?:_?[01])+', Number.Bin),
            (r'0[xX](?:_?[a-fA-F0-9])+', Number.Hex),
            (r'\d(?:_?\d)*', Number.Integer),
        ],
        'fstringescape': [
            include('stringescape'),
        ],
        'bytesescape': [
            (r'\\([\\abfnrtv"\']|\n|x[a-fA-F0-9]{2}|[0-7]{1,3})', String.Escape)
        ],
        'stringescape': [
            (r'\\(N\{.*?\}|u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8})', String.Escape),
            include('bytesescape')
        ],
        'fstrings-single': fstring_rules(String.Single),
        'fstrings-double': fstring_rules(String.Double),
        'strings-single': innerstring_rules(String.Single),
        'strings-double': innerstring_rules(String.Double),
        'dqf': [
            (r'"', String.Double, '#pop'),
            (r'\\\\|\\"|\\\n', String.Escape),  # included here for raw strings
            include('fstrings-double')
        ],
        'sqf': [
            (r"'", String.Single, '#pop'),
            (r"\\\\|\\'|\\\n", String.Escape),  # included here for raw strings
            include('fstrings-single')
        ],
        'dqs': [
            (r'"', String.Double, '#pop'),
            (r'\\\\|\\"|\\\n', String.Escape),  # included here for raw strings
            include('strings-double')
        ],
        'sqs': [
            (r"'", String.Single, '#pop'),
            (r"\\\\|\\'|\\\n", String.Escape),  # included here for raw strings
            include('strings-single')
        ],
        'tdqf': [
            (r'"""', String.Double, '#pop'),
            include('fstrings-double'),
            (r'\n', String.Double)
        ],
        'tsqf': [
            (r"'''", String.Single, '#pop'),
            include('fstrings-single'),
            (r'\n', String.Single)
        ],
        'tdqs': [
            (r'"""', String.Double, '#pop'),
            include('strings-double'),
            (r'\n', String.Double)
        ],
        'tsqs': [
            (r"'''", String.Single, '#pop'),
            include('strings-single'),
            (r'\n', String.Single)
        ],
        
        'expr-inside-fstring': [
            (r'[{([]', Punctuation, 'expr-inside-fstring-inner'),
            # without format specifier
            (r'(=\s*)?'         # debug (https://bugs.python.org/issue36817)
             r'(\![sraf])?'     # conversion
             r'\}', String.Interpol, '#pop'),
            # with format specifier
            # we'll catch the remaining '}' in the outer scope
            (r'(=\s*)?'         # debug (https://bugs.python.org/issue36817)
             r'(\![sraf])?'     # conversion
             r':', String.Interpol, '#pop'),
            (r'\s+', Whitespace),  # allow new lines
            include('expr'),
        ],
        'expr-inside-fstring-inner': [
            (r'[{([]', Punctuation, 'expr-inside-fstring-inner'),
            (r'[])}]', Punctuation, '#pop'),
            (r'\s+', Whitespace),  # allow new lines
            include('expr'),
        ],
        'keywords': [
            (words((
                'let', 'into', 'case', 'prql', 'type', 'module', 'internal',
                ), suffix=r'\b'),
             Keyword),
            (words(('true', 'false', 'null'), suffix=r'\b'), Keyword.Constant),
        ],
        'functions': [
            (words((
                "min", "max","sum", "average", "stddev", "every", "any",
                "concat_array", "count", "lag", "lead", "first", "last",
                "rank", "rank_dense", "row_number", "round", "as", "in",
                "tuple_every", "tuple_map", "tuple_zip", "_eq", "_is_null",
                "from_text", "lower", "upper", "read_parquet", "read_csv"), suffix=r'\b'),
             Name.Function),
        ],

        'comment': [
            (r'-(?!\})', Comment.Multiline),
            (r'\{-', Comment.Multiline, 'comment'),
            (r'[^-}]', Comment.Multiline),
            (r'-\}', Comment.Multiline, '#pop'),
        ],

        'imports': [
            (r'\w+(\.\w+)*', Name.Class, '#pop'),
        ],
    }
