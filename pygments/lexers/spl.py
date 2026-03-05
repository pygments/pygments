"""
    pygments.lexers.spl
    ~~~~~~~~~~~~~~~~~~~

    Lexer for Splunk Processing Language (SPL).

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, include, words
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Text, Whitespace

__all__ = ['SplLexer']


class SplLexer(RegexLexer):
    """
    For Splunk Processing Language (SPL) queries.

    Reference: https://help.splunk.com/en/splunk-enterprise/spl-search-reference/
    """

    name = 'SPL'
    url = 'https://help.splunk.com/en/splunk-enterprise/spl-search-reference/'
    aliases = ['spl']
    filenames = ['*.spl']
    mimetypes = ['text/x-spl']
    version_added = ''

    flags = re.IGNORECASE | re.MULTILINE

    # ------------------------------------------------------------------ #
    # SPL commands — complete alphabetical list from the Splunk 9.x      #
    # Search Reference "Command quick reference" page.                   #
    # https://help.splunk.com/en/splunk-enterprise/spl-search-reference/ #
    #     9.0/quick-reference/command-quick-reference                     #
    # ------------------------------------------------------------------ #
    _commands = (
        'abstract', 'accum', 'addcoltotals', 'addinfo', 'addtotals',
        'analyzefields', 'anomalies', 'anomalousvalue', 'anomalydetection',
        'append', 'appendcols', 'appendpipe', 'arules', 'associate',
        'audit', 'autoregress', 'awssnsalert',
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
        'kmeans', 'kvform', 'kv',
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
        # Internal / undocumented commands commonly seen
        'collapse', 'dump', 'findkeywords', 'makejson', 'mcatalog',
        'noop', 'prjob', 'runshellscript',
    )

    # ------------------------------------------------------------------ #
    # Evaluation functions — from the Splunk "Evaluation functions" page  #
    # Categories: Comparison/Conditional, Conversion, Cryptographic,     #
    # Date/Time, Informational, JSON, Mathematical, Multivalue,          #
    # Statistical, Text, Trig/Hyperbolic.                                #
    # ------------------------------------------------------------------ #
    _eval_functions = (
        # Comparison and Conditional
        'case', 'cidrmatch', 'coalesce', 'false', 'if', 'in',
        'like', 'lookup', 'match', 'null', 'nullif',
        'searchmatch', 'true', 'validate',
        # Conversion
        'printf', 'tonumber', 'tostring',
        # Cryptographic
        'md5', 'sha1', 'sha256', 'sha512',
        # Date and Time
        'now', 'relative_time', 'strftime', 'strptime', 'time',
        # Informational
        'isbool', 'isint', 'isnotnull', 'isnull', 'isnum', 'isstr',
        'typeof',
        # JSON
        'json_object', 'json_append', 'json_array',
        'json_array_to_mv', 'json_extend',
        'json_extract', 'json_extract_exact',
        'json_keys', 'json_set', 'json_set_exact',
        'json_valid',
        # Mathematical
        'abs', 'ceiling', 'ceil', 'exact', 'exp', 'floor', 'ln',
        'log', 'pi', 'pow', 'round', 'sigfig', 'sqrt',
        # Multivalue
        'commands', 'mvappend', 'mvcount', 'mvdedup', 'mvfilter',
        'mvfind', 'mvindex', 'mvjoin', 'mvmap', 'mvrange',
        'mvsort', 'mvzip', 'mv_to_json_array',
        'split',
        # Statistical
        'avg', 'max', 'min', 'random', 'sum', 'sumsq',
        # Text
        'len', 'lower', 'ltrim', 'replace', 'rtrim', 'substr',
        'trim', 'upper', 'urldecode',
        # Trig and Hyperbolic
        'acos', 'acosh', 'asin', 'asinh', 'atan', 'atan2', 'atanh',
        'cos', 'cosh', 'hypot', 'sin', 'sinh', 'tan', 'tanh',
    )

    # ------------------------------------------------------------------ #
    # Statistical / charting functions (used with stats, chart,          #
    # timechart, eventstats, streamstats, etc.)                          #
    # From "Statistical and charting functions" reference.               #
    # ------------------------------------------------------------------ #
    _stats_functions = (
        # Aggregate
        'avg', 'count', 'dc', 'distinct_count', 'estdc', 'estdc_error',
        'exactperc', 'max', 'mean', 'median', 'min', 'mode',
        'percentile', 'perc', 'range', 'stdev', 'stdevp',
        'sum', 'sumsq', 'upperperc', 'var', 'varp',
        # Event order
        'first', 'last',
        # Multivalue stats
        'list', 'values',
        # Time
        'earliest', 'earliest_time', 'latest', 'latest_time',
        'per_day', 'per_hour', 'per_minute', 'per_second', 'rate',
    )

    # ------------------------------------------------------------------ #
    # Common search-time parameters / clauses that act like keywords     #
    # ------------------------------------------------------------------ #
    _keyword_clauses = (
        'as', 'by', 'or', 'and', 'not', 'over', 'where', 'output',
        'outputnew', 'span', 'limit', 'agg', 'allnum', 'delim',
        'partitions', 'maxtime', 'startswith', 'endswith', 'maxspan',
        'maxpause', 'maxevents', 'keepevicted', 'mvlist',
        'connected', 'keeporphans', 'maxopenevents', 'maxopentxn',
        'keepempty', 'nullstr', 'otherstr', 'sep', 'useother',
        'usenull', 'cont', 'notin', 'partial', 'consecutive',
    )

    # ------------------------------------------------------------------ #
    # Built-in field names / special variables                           #
    # ------------------------------------------------------------------ #
    _builtin_fields = (
        '_raw', '_time', '_indextime', '_serial', '_si', '_sourcetype',
        '_subsecond', 'host', 'index', 'linecount', 'punct', 'source',
        'sourcetype', 'splunk_server', 'splunk_server_group',
        'timestamp', 'eventtype', 'tag',
    )

    tokens = {
        'root': [
            include('comments'),
            include('whitespace'),

            # Pipe — the fundamental SPL delimiter
            (r'\|', Punctuation),

            # Subsearch brackets
            (r'[\[\]]', Punctuation),

            # Strings
            include('strings'),

            # Numbers (integers, floats, and scientific notation)
            include('numbers'),

            # Time modifiers: -24h@h, earliest=, latest=, etc.
            (r'[+-]?\d+[smhdwqy](?:@[smhdwqy]\d?)?', Number.Other),

            # Comparison / assignment operators
            (r'[!=<>]=?', Operator),
            (r'\.\.', Operator),

            # Arithmetic operators
            (r'[+\-*/%]', Operator),

            # Parentheses and other punctuation
            (r'[(),]', Punctuation),

            # SPL commands — match at word boundary, case-insensitive
            (words(_commands, suffix=r'\b'), Keyword),

            # Eval / stats functions — followed by opening paren
            (r'(?:' + '|'.join(_eval_functions) + r')\s*(?=\()',
             Name.Function),
            (r'(?:' + '|'.join(_stats_functions) + r')\s*(?=\()',
             Name.Function),

            # Common keyword-like clauses
            (words(_keyword_clauses, prefix=r'\b', suffix=r'\b'),
             Keyword.Pseudo),

            # Boolean literal keywords
            (r'\b(?:true|false|TRUE|FALSE|True|False)\b', Keyword.Constant),

            # Built-in / internal fields
            (words(_builtin_fields, prefix=r'\b', suffix=r'\b'),
             Name.Builtin),

            # Wildcard field patterns (field_name=*)
            (r'[a-zA-Z_]\w*(?:\.\w+)*(?=\s*=)', Name.Variable),

            # Field names and identifiers
            (r'[a-zA-Z_]\w*(?:\.\w+)*', Name.Other),

            # Equals sign (assignment / filter)
            (r'=', Operator),

            # Catch-all for misc characters
            (r'.', Text),
        ],

        'comments': [
            # SPL uses ``` for inline comments (triple backtick)
            (r'```.*?```', Comment),
        ],

        'whitespace': [
            (r'\s+', Whitespace),
        ],

        'strings': [
            # Double-quoted strings
            (r'"', String.Double, 'string-double'),
            # Single-quoted field names
            (r"'", String.Single, 'string-single'),
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

        'numbers': [
            # Scientific notation
            (r'\b\d+\.?\d*[eE][+-]?\d+\b', Number.Float),
            # Floats
            (r'\b\d+\.\d+\b', Number.Float),
            # Integers
            (r'\b\d+\b', Number.Integer),
        ],
    }

    def analyse_text(text):
        """Give a slight boost when the text has a pipe character followed
        by what looks like an SPL command."""
        result = 0.0
        if '| stats ' in text or '| eval ' in text or '| table ' in text:
            result += 0.3
        if '| search ' in text or '| where ' in text:
            result += 0.2
        if 'index=' in text or 'sourcetype=' in text:
            result += 0.2
        return min(result, 1.0)