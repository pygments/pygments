"""
    pygments.lexers.jq
    ~~~~~~~~~~~~~~~~~~

    Lexer for jq, the command-line JSON processor.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import (Comment, Keyword, Name, Number, Operator,
                             Punctuation, String, Whitespace)

__all__ = ['JqLexer']


class JqLexer(RegexLexer):
    """
    For `jq <https://jqlang.github.io/jq/>`_ programs.

    jq is a lightweight command-line JSON processor with its own
    functional expression language.

    .. versionadded:: 2.21
    """

    name = 'jq'
    url = 'https://jqlang.github.io/jq/'
    aliases = ['jq']
    filenames = ['*.jq']
    mimetypes = ['application/x-jq']
    version_added = '2.21'

    # Built-in functions
    _BUILTINS = (
        # Core
        'empty', 'error', 'halt', 'halt_error',
        'path', 'paths', 'leaf_paths', 'getpath', 'setpath', 'delpaths',
        'to_entries', 'from_entries', 'with_entries',
        'keys', 'keys_unsorted', 'values', 'has', 'in', 'contains',
        'map', 'map_values', 'select', 'arrays', 'objects', 'iterables',
        'booleans', 'numbers', 'strings', 'nulls', 'scalars', 'values',
        'add', 'any', 'all', 'flatten', 'range', 'floor', 'ceil',
        'round', 'sqrt', 'pow', 'fabs', 'nan', 'infinite', 'isinfinite',
        'isnan', 'isnormal', 'isfinite',
        'sort', 'sort_by', 'group_by', 'unique', 'unique_by',
        'min', 'max', 'min_by', 'max_by', 'reverse', 'indices', 'index',
        'rindex', 'inside', 'limit', 'first', 'last', 'nth', 'until',
        'recurse', 'recurse_down', 'walk', 'env', 'transpose',
        'input', 'inputs', 'debug', 'stderr', 'input_filename',
        'input_line_number', 'modulemeta',
        # String
        'ltrimstr', 'rtrimstr', 'startswith', 'endswith', 'split', 'join',
        'test', 'match', 'capture', 'scan', 'sub', 'gsub', 'ascii_downcase',
        'ascii_upcase', 'explode', 'implode', 'ascii', 'tojson', 'fromjson',
        'tostring', 'tonumber', 'todate', 'fromdate', 'now', 'dateadd',
        'datesub', 'strftime', 'strptime', 'gmtime', 'mktime',
        'tocsv', 'fromcsv', 'tsv', 'fromtsv',
        # Type
        'type', 'infinite', 'length', 'utf8bytelength', 'not',
        # I/O
        'builtins', 'paths',
    )

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'#.*$', Comment.Single),
            # String interpolation  "foo \(expr) bar"
            (r'\"', String.Double, 'string'),
            # Format strings  @base64, @uri, @csv, @json, @text, @sh, @html
            (r'@(?:base64d?|uri|csv|tsv|json|text|sh|html)\b', Name.Builtin),
            # Keywords
            (words(('if', 'then', 'elif', 'else', 'end',
                    'and', 'or', 'not',
                    'try', 'catch', 'label', 'break',
                    'as', 'def', 'import', 'include', 'module',
                    'reduce', 'foreach',
                    'null', 'true', 'false'), suffix=r'\b'), Keyword),
            # Built-in functions
            (words(_BUILTINS, suffix=r'(?=\s*[;|(\[\]]|$)'), Name.Builtin),
            # Numbers
            (r'[+-]?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?', Number),
            # Variables  $name  $__loc__
            (r'\$[a-zA-Z_]\w*|\$__loc__', Name.Variable),
            # Path expression  .foo  .["foo"]  .[0]  .
            (r'\.(?:[a-zA-Z_]\w*)?', Name.Attribute),
            # Recursive descent
            (r'\.\.', Operator),
            # Operators
            (r'//=?|\|=?|::|[+\-*/%]=?|==|!=|<=?|>=?|\?\/\/', Operator),
            (r'[?|,;:\[\]{}()]', Punctuation),
            (r'[a-zA-Z_]\w*', Name.Function),
        ],
        'string': [
            (r'[^"\\\\]+', String.Double),
            (r'\\\\\\(', String.Interpol, 'interpolation'),
            (r'\\\\.', String.Escape),
            (r'\"', String.Double, '#pop'),
        ],
        'interpolation': [
            (r'\)', String.Interpol, '#pop'),
            include('root'),
        ],
    }
