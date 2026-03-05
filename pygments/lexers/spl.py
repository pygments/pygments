"""
    pygments.lexers.spl
    ~~~~~~~~~~~~~~~~~~~

    Lexer for Splunk Search Processing Language (SPL).

    :copyright: Copyright 2026 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, words
from pygments.token import Comment, Keyword, Literal, Name, Number, \
    Operator, Punctuation, String, Text, Whitespace

__all__ = ['SplLexer']


class SplLexer(RegexLexer):
    """
    For Splunk Search Processing Language (SPL) queries.
    """

    name = 'SPL'
    url = 'https://help.splunk.com/en/splunk-enterprise/search/spl-search-reference/'
    aliases = ['spl', 'splunk']
    filenames = ['*.spl', '*.splunk']
    mimetypes = ['text/x-spl']
    version_added = ''

    flags = re.IGNORECASE | re.MULTILINE

    # ------------------------------------------------------------------ #
    # SPL commands — complete list from the Splunk 9.4 Search Reference  #
    # "Command quick reference" page.                                    #
    # ------------------------------------------------------------------ #
    _commands = (
        'abstract', 'accum', 'addcoltotals', 'addinfo', 'addtotals',
        'analyzefields', 'anomalies', 'anomalousvalue', 'anomalydetection',
        'append', 'appendcols', 'appendpipe', 'arules', 'associate',
        'autoregress', 'awssnsalert',
        'bin', 'bucket', 'bucketdir',
        'chart', 'cluster', 'cofilter', 'collect', 'concurrency',
        'contingency', 'convert', 'correlate', 'ctable',
        'datamodel', 'datamodelsimple', 'dbinspect', 'dbxquery',
        'dedup', 'delete', 'delta', 'diff',
        'entitymerge', 'erex', 'eval', 'eventcount', 'eventstats',
        'extract',
        'fieldformat', 'fields', 'fieldsummary', 'filldown', 'fillnull',
        'findtypes', 'folderize', 'foreach', 'format', 'from', 'fromjson',
        'gauge', 'gentimes', 'geom', 'geomfilter', 'geostats',
        'head', 'highlight', 'history',
        'iconify', 'inputcsv', 'inputintelligence', 'inputlookup',
        'iplocation',
        'join',
        'kmeans', 'kvform',
        'loadjob', 'localize', 'localop', 'lookup',
        'makecontinuous', 'makemv', 'makeresults', 'map',
        'mcollect', 'metadata', 'metasearch', 'meventcollect',
        'mpreview', 'msearch', 'mstats', 'multikv', 'multisearch',
        'mvcombine', 'mvexpand',
        'nomv',
        'outlier', 'outputcsv', 'outputlookup', 'outputtext', 'overlap',
        'pivot', 'predict',
        'rangemap', 'rare', 'redistribute', 'regex', 'reltime', 'rename',
        'replace', 'require', 'rest', 'return', 'reverse', 'rex',
        'rtorder', 'run',
        'savedsearch', 'script', 'scrub', 'search', 'searchtxn',
        'selfjoin', 'sendalert', 'sendemail', 'set', 'setfields',
        'sichart', 'sirare', 'sistats', 'sitimechart', 'sitop',
        'snowevent', 'snoweventstream', 'snowincident',
        'snowincidentstream',
        'sort', 'spath', 'stats', 'strcat', 'streamstats',
        'table', 'tags', 'tail', 'timechart', 'timewrap', 'tojson',
        'top', 'transaction', 'transpose', 'trendline',
        'tscollect', 'tstats', 'typeahead', 'typelearner', 'typer',
        'union', 'uniq', 'untable',
        'walklex', 'where',
        'x11', 'xmlkv', 'xmlunescape', 'xpath', 'xyseries',
    )

    # ------------------------------------------------------------------ #
    # Evaluation functions — from the Splunk 9.4 "Evaluation functions"  #
    # reference.  Categories: Bitwise, Comparison/Conditional,           #
    # Conversion, Cryptographic, Date/Time, Informational, JSON,         #
    # Mathematical, Multivalue, Statistical, Text, Trig/Hyperbolic.      #
    # ------------------------------------------------------------------ #
    _eval_functions = (
        # Bitwise
        'bit_and', 'bit_not', 'bit_or', 'bit_shift_left',
        'bit_shift_right', 'bit_xor',
        # Comparison and Conditional
        'case', 'cidrmatch', 'coalesce', 'false', 'if', 'in',
        'like', 'lookup', 'match', 'null', 'nullif',
        'searchmatch', 'true', 'validate',
        # Conversion
        'ipmask', 'printf', 'toarray', 'tobool', 'todouble', 'toint',
        'tomv', 'tonumber', 'toobject', 'tostring',
        # Cryptographic
        'md5', 'sha1', 'sha256', 'sha512',
        # Date and Time
        'now', 'relative_time', 'strftime', 'strptime', 'time',
        # Informational
        'isarray', 'isbool', 'isdouble', 'isint', 'ismv', 'isnotnull',
        'isnull', 'isnum', 'isobject', 'isstr', 'typeof',
        # JSON
        'json', 'json_append', 'json_array', 'json_array_to_mv',
        'json_delete', 'json_entries', 'json_extend', 'json_extract',
        'json_extract_exact', 'json_has_key_exact', 'json_keys',
        'json_object', 'json_set', 'json_set_exact', 'json_valid',
        # Mathematical
        'abs', 'ceiling', 'exact', 'exp', 'floor', 'ln', 'log', 'pi',
        'pow', 'round', 'sigfig', 'sqrt', 'sum',
        # Multivalue
        'commands', 'mvappend', 'mvcount', 'mvdedup', 'mvfilter',
        'mvfind', 'mvindex', 'mvjoin', 'mvmap', 'mvrange', 'mvsort',
        'mvzip', 'mv_to_json_array', 'split',
        # Statistical eval
        'avg', 'max', 'min', 'random',
        # Text
        'len', 'lower', 'ltrim', 'replace', 'rtrim', 'spath', 'substr',
        'trim', 'upper', 'urldecode',
        # Trig and Hyperbolic
        'acos', 'acosh', 'asin', 'asinh', 'atan', 'atan2', 'atanh',
        'cos', 'cosh', 'hypot', 'sin', 'sinh', 'tan', 'tanh',
    )

    # ------------------------------------------------------------------ #
    # Statistical / charting functions (stats, chart, timechart, etc.)   #
    # From "Statistical and charting functions" reference.               #
    # ------------------------------------------------------------------ #
    _stats_functions = (
        # Aggregate
        'avg', 'count', 'dc', 'distinct_count', 'estdc', 'estdc_error',
        'exactperc', 'max', 'mean', 'median', 'min', 'mode',
        'perc', 'percentile', 'range', 'stdev', 'stdevp',
        'sum', 'sumsq', 'upperperc', 'var', 'varp',
        # Event order
        'first', 'last',
        # Multivalue stats
        'list', 'values',
        # Time
        'earliest', 'earliest_time', 'latest', 'latest_time',
        'per_day', 'per_hour', 'per_minute', 'per_second',
        'rate', 'rate_avg', 'rate_sum',
    )

    # ------------------------------------------------------------------ #
    # Built-in field names / special variables                           #
    # ------------------------------------------------------------------ #
    _builtin_fields = (
        '_indextime', '_raw', '_serial', '_si', '_sourcetype',
        '_subsecond', '_time',
        'host', 'index', 'linecount', 'punct', 'source',
        'sourcetype', 'splunk_server', 'splunk_server_group',
        'timestamp', 'eventtype', 'tag',
    )

    tokens = {
        'root': [
            include('comments'),
            include('whitespace'),

            # Pipe — central SPL command separator
            (r'\|', Punctuation),

            # Subsearch brackets
            (r'[\[\]]', Punctuation),

            # Strings
            include('strings'),

            # Numbers
            include('numbers'),

            # Time modifiers: earliest=-24h@h, latest=now
            (r'(?:earliest|latest|_index_earliest|_index_latest)'
             r'\s*=\s*(?:"[^"]*"|[^\s\|\]\),]+)', Literal.Date),

            # Boolean operators
            (words(('AND', 'OR', 'NOT', 'XOR'),
                   prefix=r'\b', suffix=r'\b'), Operator.Word),

            # Comparison operators
            (r'[!=<>]=?|==', Operator),

            # Concatenation operator
            (r'\.', Operator),

            # Arithmetic operators
            (r'[+\-*/%]', Operator),

            # Keyword clauses
            (words(('AS', 'BY', 'IN', 'LIKE', 'OVER', 'OUTPUT',
                    'OUTPUTNEW'),
                   prefix=r'\b', suffix=r'\b'), Keyword.Pseudo),

            # Constants
            (r'\b(?:true|false|null)\b', Keyword.Constant),

            # Eval and stats functions (followed by opening paren)
            (words(_eval_functions + _stats_functions,
                   prefix=r'\b', suffix=r'\b(?=\s*\()'), Name.Builtin),

            # SPL commands
            (words(_commands, suffix=r'\b'), Keyword),

            # Built-in fields
            (words(_builtin_fields, prefix=r'\b', suffix=r'\b'),
             Name.Variable.Magic),

            # Parentheses, commas
            (r'[(),]', Punctuation),

            # Equals sign (assignment / comparison)
            (r'=', Operator),

            # Field names and identifiers
            (r'[a-zA-Z_]\w*(?:\.\w+)*', Name.Other),

            # Catch-all
            (r'.', Text),
        ],

        'comments': [
            # SPL triple-backtick comments (can span multiple lines)
            (r'```', Comment, 'comment'),
        ],

        'comment': [
            (r'```', Comment, '#pop'),
            (r'`{1,2}', Comment),
            (r'[^`]+', Comment),
        ],

        'whitespace': [
            (r'\s+', Whitespace),
        ],

        'strings': [
            # Double-quoted strings
            (r'"', String.Double, 'string-double'),
            # Single-quoted strings (field names)
            (r"'", String.Single, 'string-single'),
            # Backtick macros (single backtick, NOT triple)
            (r'`(?!``)', String.Backtick, 'string-backtick'),
        ],

        'string-double': [
            (r'\\.', String.Escape),
            (r'[^"\\]+', String.Double),
            (r'"', String.Double, '#pop'),
        ],

        'string-single': [
            (r'\\.', String.Escape),
            (r"[^'\\]+", String.Single),
            (r"'", String.Single, '#pop'),
        ],

        'string-backtick': [
            (r'[^`]+', String.Backtick),
            (r'`', String.Backtick, '#pop'),
        ],

        'numbers': [
            # Scientific notation
            (r'\b\d+\.?\d*[eE][+-]?\d+\b', Number.Float),
            # Floats
            (r'\b\d+\.\d+\b', Number.Float),
            # Integers
            (r'\b\d+\b', Number.Integer),
        ],
    }

    def analyse_text(self, text):
        """Give a slight boost when the text looks like SPL."""
        result = 0.0
        if '| stats ' in text or '| eval ' in text or '| table ' in text:
            result += 0.3
        if '| search ' in text or '| where ' in text:
            result += 0.2
        if 'index=' in text or 'sourcetype=' in text:
            result += 0.2
        return min(result, 1.0)
