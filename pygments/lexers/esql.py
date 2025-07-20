"""
    pygments.lexers.esql
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for Elasticsearch Query Language (ES|QL).

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, words
from pygments.token import (Comment, Keyword, Literal, Name, Number, Operator,
                            Punctuation, String, Whitespace)

__all__ = ['ESQLLexer']

# Reference: https://www.elastic.co/guide/en/elasticsearch/reference/8.17/esql-time-spans.html#esql-time-spans-table
ESQL_TEMPORAL_UNITS = [
    'year', 'y', 'yr', 'years',
    'quarter', 'q', 'quarters',
    'month', 'mo', 'months',
    'week', 'w', 'weeks',
    'day', 'd', 'days',
    'hour', 'h', 'hours',
    'minute', 'min', 'minutes',
    'second', 's', 'sec', 'seconds',
    'millisecond', 'ms', 'milliseconds'
]

# Reference: https://www.elastic.co/guide/en/elasticsearch/reference/8.17/esql-commands.html
ESQL_COMMANDS = [
    'FROM', 'ROW', 'SHOW', 'DISSECT', 'DROP', 'ENRICH', 'EVAL', 'GROK', 'KEEP', 'LIMIT',
    'MV_EXPAND', 'RENAME', 'SORT', 'STATS', 'WHERE',
    # Commands extensions
    'METADATA', 'APPEND_SEPARATOR', 'ON', 'WITH', 'AS', 'ASC', 'DESC', 'NULLS FIRST',
    'NULLS LAST', 'BY'
]

# Reference: https://www.elastic.co/guide/en/elasticsearch/reference/8.17/esql-functions-operators.html
ESQL_FUNCTIONS = [
    # Aggregate functions
    'AVG', 'COUNT', 'COUNT_DISTINCT', 'MAX', 'MEDIAN', 'MEDIAN_ABSOLUTE_DEVIATION',
    'MIN', 'PERCENTILE', 'ST_CENTROID_AGG', 'SUM', 'TOP', 'VALUES', 'WEIGHTED_AVG',
    # Grouping functions
    'BUCKET',
    # Conditional functions and expressions
    'CASE', 'COALESCE', 'GREATEST', 'LEAST',
    # Date and time functions
    'DATE_DIFF', 'DATE_EXTRACT', 'DATE_FORMAT', 'DATE_PARSE', 'DATE_TRUNC', 'NOW',
    # IP functions
    'CIDR_MATCH', 'IP_PREFIX',
    # Math functions
    'ABS', 'ACOS', 'ASIN', 'ATAN', 'ATAN2', 'CBRT', 'CEIL', 'COS', 'COSH', 'E',
    'EXP', 'FLOOR', 'HYPOT', 'LOG', 'LOG10', 'PI', 'POW', 'ROUND', 'SIGNUM',
    'SIN', 'SINH', 'SQRT', 'TAN', 'TANH', 'TAU',
    # Search functions
    'MATCH', 'QSTR',
    # Spatial functions
    'ST_DISTANCE', 'ST_INTERSECTS', 'ST_DISJOINT', 'ST_CONTAINS', 'ST_WITHIN',
    'ST_X', 'ST_Y',
    # String functions
    'BIT_LENGTH', 'BYTE_LENGTH', 'CONCAT', 'ENDS_WITH', 'FROM_BASE64', 'LEFT',
    'LENGTH', 'LOCATE', 'LTRIM', 'REPEAT', 'REPLACE', 'REVERSE', 'RIGHT', 'RTRIM',
    'SPACE', 'SPLIT', 'STARTS_WITH', 'SUBSTRING', 'TO_BASE64', 'TO_LOWER',
    'TO_UPPER', 'TRIM',
    # Type conversion functions
    'TO_BOOLEAN', 'TO_CARTESIANPOINT', 'TO_CARTESIANSHAPE', 'TO_DATEPERIOD',
    'TO_DATETIME', 'TO_DEGREES', 'TO_DOUBLE', 'TO_GEOPOINT', 'TO_GEOSHAPE',
    'TO_INTEGER', 'TO_IP', 'TO_LONG', 'TO_RADIANS', 'TO_STRING', 'TO_TIMEDURATION',
    'TO_UNSIGNED_LONG', 'TO_VERSION',
    # Multi value functions
    'MV_APPEND', 'MV_AVG', 'MV_CONCAT', 'MV_COUNT', 'MV_DEDUPE', 'MV_FIRST',
    'MV_LAST', 'MV_MAX', 'MV_MEDIAN', 'MV_MEDIAN', 'MV_MEDIAN_ABSOLUTE_DEVIATION',
    'MV_MIN', 'MV_PERCENTILE', 'MV_PSERIES_WEIGHTED_SUM', 'MV_SORT', 'MV_SLICE',
    'MV_SUM', 'MV_ZIP'
]
ESQL_OPERATORS = [
    # Binary operators
    '==', '!=', '<', '<=', '>', '>=', '+', '-', '*', '/', '%',
    # Unary operators
    # '-', # Already taken into account in binary operators
    # Other operators
    '::', ':', '='
]
ESQL_KEYWORD_OPERATORS = [
    # Logical operators
    'AND', 'OR', 'NOT',
    # Operators highlighted as commands in Kibana
    'IS NULL', 'IS NOT NULL', 'IN', 'LIKE', 'RLIKE'
]

class ESQLLexer(RegexLexer):
    """Lexer for Elasticsearch Query Language (ES|QL). Based on Elasticsearch Guide 8.17.
    """

    name = "Elasticsearch Query Language"
    aliases = ["esql", "es|ql"]
    filenames = ["*.es", "*.esql"]
    mimetypes = []
    url = "https://www.elastic.co/guide/en/elasticsearch/reference/current/esql.html"
    version_added = '2.20'

    flags = re.IGNORECASE

    # https://pygments.org/docs/tokens/
    tokens = {
        "root": [
            (r"\s+", Whitespace),

            # Comments
            # Reference: https://www.elastic.co/guide/en/elasticsearch/reference/8.17/esql-syntax.html#esql-comments
            (r"//.*\n?", Comment.Single),
            (r"/\*", Comment.Multiline, 'multiline_comments'), # C++ style comments

            # Exception - Special case for 'FROM <source> |' that could contains minus ('-')
            # and wildcard ('*') without any string literal delimiter (e.g. 'FROM log-*')
            (r"FROM", Keyword, 'from-source'),

            # Timespan literals (a combination of a number and a temporal unit)
            # Reference: https://www.elastic.co/guide/en/elasticsearch/reference/8.17/esql-syntax.html#esql-timespan-literals
            (words(ESQL_TEMPORAL_UNITS, prefix=r"\b\d+\s*", suffix=r"\b"), Literal.Date),

            (words(ESQL_COMMANDS, suffix=r"\b"), Keyword),
            (words(ESQL_FUNCTIONS, suffix=r"\b"), Name.Function),

            (words(ESQL_OPERATORS), Operator),
            (words(ESQL_KEYWORD_OPERATORS, suffix=r"\b"), Operator.Word),

            # String literals
            # Reference: https://www.elastic.co/guide/en/elasticsearch/reference/8.17/esql-syntax.html#esql-string-literals
            (r'"""', String, 'triple_double_string'),
            (r'"', String, 'double_string'),

            # Numerical literals
            # Reference: https://www.elastic.co/guide/en/elasticsearch/reference/8.17/esql-syntax.html#esql-numeric-literals
            (r"-?\d+([.]\d*)*[eE]-?\d+", Number),
            (r"\d+[.]\d*|[.]\d+", Number.Float),
            (r"\d+", Number.Integer),

            # null literal
            (r"\bnull\b", Keyword.Constant),

            # Identifiers
            # Reference: https://www.elastic.co/guide/en/elasticsearch/reference/8.17/esql-syntax.html#esql-identifiers
            (r"`", Name, 'backtick'),
            (r"[a-z_@]\w*", Name), # Full regex is "[a-zA-Z_@]\w*" but not required as re.IGNORECASE is set

            # Processing
            # Reference: https://www.elastic.co/guide/en/elasticsearch/reference/8.17/esql-syntax.html#esql-basic-syntax
            (r"[|(),\.]+", Punctuation),
        ],
        'from-source': [
            (r"\s+", Whitespace),
            (r"/\*", Comment.Multiline, 'multiline_comments'),
            (r",", Punctuation),
            (r"[^\s,\(/\*\)\|]+", Name),
            (r"\*", Name), # Force '*' wildcard to be part of the Name
            (r"\|", Punctuation, "#pop")
        ],
        'backtick': [
            (r"``", String.Escape),
            (r"[^`\(``\)]+", Name),
            (r"[()]", Name), # Force inner [()] within backticks to be considered as Name
            (r"`", Name, "#pop")
        ],
        'double_string': [
            (r'\\"', String.Escape),
            (r'\\\\', String), # Inner '\\' string
            (r'[^"\(\\"\)]+', String),
            (r'"', String, "#pop")
        ],
        'triple_double_string': [
            (r'"[^"]', String), # Inner single-quoted string
            (r'[^\("""\)]+', String),
            (r'"""', String, "#pop")
        ],
        'multiline_comments': [
            (r"/\*", Comment.Multiline),
            (r"[^\(\*/\)]+", Comment.Multiline),
            (r"\*/", Comment.Multiline, '#pop')
        ]
    }
