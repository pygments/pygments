"""
    pygments.lexers.nextflow
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Nextflow workflow language.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, default, include, words
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Whitespace
from pygments.util import shebang_matches

__all__ = ['NextflowLexer']


class NextflowLexer(RegexLexer):
    """
    For Nextflow scripts and config files.
    """

    name = 'Nextflow'
    url = 'https://www.nextflow.io/'
    aliases = ['nextflow', 'nf']
    filenames = ['*.nf', 'nextflow.config']
    mimetypes = ['text/x-nextflow']
    version_added = '2.22'

    flags = re.MULTILINE

    _ident = r'[A-Za-z_]\w*'

    tokens = {
        'root': [
            (r'#!.*?$', Comment.Hashbang, 'base'),
            default('base'),
        ],
        'base': [
            # process and workflow sections, e.g. `input:` on its own line
            (r'^([ \t]*)(input|output|script|shell|exec|stub|stage|topic|'
             r'when|take|main|emit|publish|prompt|onComplete|onError)(:)'
             r'(?=[ \t]*(?://.*)?$)',
             bygroups(Whitespace, Keyword, Punctuation)),
            (r'[^\S\n]+', Whitespace),
            (r'\n', Whitespace),
            (r'//.*?$', Comment.Single),
            (r'/\*[\s\S]*?\*/', Comment.Multiline),

            # declarations
            (r'(process|workflow|agent)(\s+)(' + _ident + ')',
             bygroups(Keyword.Declaration, Whitespace, Name.Function)),
            (r'(workflow)(\s*)(\{)',
             bygroups(Keyword.Declaration, Whitespace, Punctuation)),
            (r'(record|enum)(\s+)(' + _ident + ')',
             bygroups(Keyword.Declaration, Whitespace, Name.Class)),
            (r'^(params|output|process|profiles)(\s*)(\{)',
             bygroups(Keyword.Declaration, Whitespace, Punctuation)),
            (r'(def)(\s+)(' + _ident + r')(\s*)(\()',
             bygroups(Keyword.Declaration, Whitespace, Name.Function,
                      Whitespace, Punctuation)),
            (r'def\b', Keyword.Declaration),
            (r'(include|includeConfig)\b', Keyword.Namespace),
            (r'(\})(\s*)(from)\b',
             bygroups(Punctuation, Whitespace, Keyword.Namespace)),

            (words(('as', 'assert', 'catch', 'else', 'if', 'in', 'instanceof',
                    'new', 'return', 'throw', 'try'), suffix=r'\b'),
             Keyword),
            (words(('true', 'false', 'null'), suffix=r'\b'), Keyword.Constant),
            (words(('baseDir', 'channel', 'launchDir', 'log', 'moduleDir',
                    'nextflow', 'params', 'projectDir', 'secrets', 'task',
                    'workDir', 'workflow'), suffix=r'\b'),
             Name.Builtin),
            # built-in functions, when called
            (words(('env', 'error', 'eval', 'exit', 'file', 'files',
                    'groupKey', 'print', 'printf', 'println', 'record',
                    'sendMail', 'sleep', 'stdout', 'tuple'),
                   suffix=r'\b(?=\s*[(\'"])'),
             Name.Builtin),

            # strings
            (r'"""', String.Double, 'tdqs'),
            (r"'''", String.Single, 'tsqs'),
            (r'"', String.Double, 'dqs'),
            (r"'", String.Single, 'sqs'),
            # slashy strings can only follow an operator or open paren,
            # otherwise `/` is division
            (r'(==~|=~|~)(\s*)(/)(?![/*])',
             bygroups(Operator, Whitespace, String.Regex), 'slashy'),
            (r'([(,])(\s*)(/)(?![/*])',
             bygroups(Punctuation, Whitespace, String.Regex), 'slashy'),

            # numbers
            (r'0[xX][0-9a-fA-F_]+', Number.Hex),
            (r'0[bB][01_]+', Number.Bin),
            (r'\d[\d_]*\.\d[\d_]*([eE][+-]?\d+)?', Number.Float),
            (r'\d[\d_]*[eE][+-]?\d+', Number.Float),
            (r'0[0-7_]+', Number.Oct),
            (r'\d[\d_]*', Number.Integer),

            (r'(\?\.|\*\.|\.)(' + _ident + ')',
             bygroups(Operator, Name.Attribute)),
            # CamelCase names are types, all other names (variables, fields,
            # calls, enum values) are styled like properties
            (r'[A-Z]\w*?[a-z]\w*', Name),
            (_ident, Name.Attribute),
            (r'->|\?:|==~|=~|<=>|\.\.<|\.\.|\*\*|>>>|>>|<<|&&|\|\||[=!<>]=|'
             r'[-+*/%=<>!&|^~?:.]', Operator),
            (r'[{}()\[\],;]', Punctuation),
        ],
        'escape': [
            (r'\\(u[0-9a-fA-F]{4}|[0-7]{1,3}|[\s\S])', String.Escape),
        ],
        'interp': [
            (r'\$\{', String.Interpol, 'interp-expr'),
            (r'\$' + _ident + r'(\.' + _ident + ')*', String.Interpol),
        ],
        'interp-expr': [
            (r'\}', String.Interpol, '#pop'),
            (r'\{', Punctuation, 'braces'),
            include('base'),
        ],
        'braces': [
            (r'\}', Punctuation, '#pop'),
            (r'\{', Punctuation, '#push'),
            include('base'),
        ],
        'dqs': [
            (r'"', String.Double, '#pop'),
            include('escape'),
            include('interp'),
            (r'[^"\\$]+', String.Double),
            (r'\$', String.Double),
        ],
        'tdqs': [
            (r'"""', String.Double, '#pop'),
            include('escape'),
            include('interp'),
            (r'[^"\\$]+', String.Double),
            (r'["$]', String.Double),
        ],
        'sqs': [
            (r"'", String.Single, '#pop'),
            include('escape'),
            (r"[^'\\]+", String.Single),
        ],
        'tsqs': [
            (r"'''", String.Single, '#pop'),
            include('escape'),
            (r"[^'\\]+", String.Single),
            (r"'", String.Single),
        ],
        'slashy': [
            (r'/', String.Regex, '#pop'),
            (r'\\/', String.Escape),
            (r'[^/\\\n]+', String.Regex),
            (r'\\', String.Regex),
            # an unterminated slashy string ends at the newline
            (r'\n', Whitespace, '#pop'),
        ],
    }

    def analyse_text(text):
        if shebang_matches(text, r'nextflow'):
            return 1.0
        if re.search(r'^nextflow\.(enable|preview)\.\w+\s*=', text, re.M):
            return 0.5
        return 0
