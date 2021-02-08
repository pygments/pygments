"""
    pygments.lexers.julia
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for the Julia language.

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer, RegexLexer, bygroups, do_insertions, \
    words, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Generic
from pygments.util import shebang_matches
from pygments.lexers._julia_builtins import OPERATORS_LIST, DOTTED_OPERATORS_LIST, \
    KEYWORD_LIST, BUILTIN_LIST, LITERAL_LIST

__all__ = ['JuliaLexer', 'JuliaConsoleLexer']

allowed_variable = \
    '(?:[a-zA-Z_\u00A1-\U0010ffff]|%s)(?:[a-zA-Z_0-9\u00A1-\U0010ffff])*!*'


class JuliaLexer(RegexLexer):
    """
    For `Julia <http://julialang.org/>`_ source code.

    .. versionadded:: 1.6
    """

    name = 'Julia'
    aliases = ['julia', 'jl']
    filenames = ['*.jl']
    mimetypes = ['text/x-julia', 'application/x-julia']

    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root': [
            (r'\n', Text),
            (r'[^\S\n]+', Text),
            (r'#=', Comment.Multiline, "blockcomment"),
            (r'#.*$', Comment),
            (r'[\[\]{}(),;]', Punctuation),

            # keywords
            (words(KEYWORD_LIST, suffix=r'\b'), Keyword),
            (r'\b(((abstract|primitive)[ \t]+type|(mutable[ \t]+)?struct))\b', Keyword),
            # builtin types
            (words(BUILTIN_LIST, suffix=r'\b'), Keyword.Type),
            # builtin literals
            (words(LITERAL_LIST, suffix=r'\b'), Name.Builtin),

            # symbols --- exclude i.e. a:b, 1:b, <:T, >:T, ::T
            ('(' + allowed_variable + r')(\s*)(:)(' + allowed_variable + ')',
                bygroups(Name, Text, Operator, Name)),
            (r'([\d.]+)(\s*)(:)(' + allowed_variable + ')',
                bygroups(Number, Text, Operator, Name)),
            (r'(?<![<>:]):' + allowed_variable, String.Symbol),

            # operators
            (words([*OPERATORS_LIST, *DOTTED_OPERATORS_LIST]), Operator),
            (words(['.' + o for o in DOTTED_OPERATORS_LIST]), Operator),
            (words(['...', '..', '.']), Operator),

            # NOTE
            # Patterns below work only for definition sites and thus hardly reliable.
            #
            # functions
            # (r'(function)(\s+)(' + allowed_variable + ')',
            #  bygroups(Keyword, Text, Name.Function)),
            #
            # types
            # (r'(type|typealias|abstract|immutable)(\s+)(' + allowed_variable + ')',
            #  bygroups(Keyword, Text, Name.Class)),

            # chars
            (r"'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,3}|\\u[a-fA-F0-9]{1,4}|"
             r"\\U[a-fA-F0-9]{1,6}|[^\\\'\n])'", String.Char),

            # try to match trailing transpose
            (r'(?<=[.\w)\]])\'+', Operator),

            # strings
            (r'"""', String, 'tqstring'),
            (r'"', String, 'string'),

            # regular expressions
            (r'r"""', String.Regex, 'tqregex'),
            (r'r"', String.Regex, 'regex'),

            # backticks
            (r'`', String.Backtick, 'command'),

            # names
            (allowed_variable, Name),
            # macros
            (r'@' + allowed_variable, Name.Decorator),
            (words([*OPERATORS_LIST, '..', '.', *DOTTED_OPERATORS_LIST], prefix='@'), Name.Decorator),

            # numbers
            (r'(\d+(_\d+)+\.(?!\.)\d*|\d*\.\d+(_\d+)+)([eEf][+-]?[0-9]+)?', Number.Float),
            (r'(\d+\.(?!\.)\d*|\d*\.\d+)([eEf][+-]?[0-9]+)?', Number.Float),
            (r'\d+(_\d+)+[eEf][+-]?[0-9]+', Number.Float),
            (r'\d+[eEf][+-]?[0-9]+', Number.Float),
            (r'0b[01]+(_[01]+)+', Number.Bin),
            (r'0b[01]+', Number.Bin),
            (r'0o[0-7]+(_[0-7]+)+', Number.Oct),
            (r'0o[0-7]+', Number.Oct),
            (r'0x[a-fA-F0-9]+(_[a-fA-F0-9]+)+', Number.Hex),
            (r'0x[a-fA-F0-9]+', Number.Hex),
            (r'\d+(_\d+)+', Number.Integer),
            (r'\d+', Number.Integer)
        ],

        "blockcomment": [
            (r'[^=#]', Comment.Multiline),
            (r'#=', Comment.Multiline, '#push'),
            (r'=#', Comment.Multiline, '#pop'),
            (r'[=#]', Comment.Multiline),
        ],

        'string': [
            (r'"', String, '#pop'),
            # FIXME: This escape pattern is not perfect.
            (r'\\([\\"\'$nrbtfav]|(x|u|U)[a-fA-F0-9]+|\d+)', String.Escape),
            # Interpolation is defined as "$" followed by the shortest full
            # expression, which is something we can't parse.
            # Include the most common cases here: $word, and $(paren'd expr).
            (r'\$' + allowed_variable, String.Interpol),
            # (r'\$[a-zA-Z_]+', String.Interpol),
            (r'(\$)(\()', bygroups(String.Interpol, Punctuation), 'in-intp'),
            # @printf and @sprintf formats
            (r'%[-#0 +]*([0-9]+|[*])?(\.([0-9]+|[*]))?[hlL]?[E-GXc-giorsux%]',
             String.Interpol),
            (r'.|\s', String),
        ],

        'tqstring': [
            (r'"""', String, '#pop'),
            (r'\\([\\"\'$nrbtfav]|(x|u|U)[a-fA-F0-9]+|\d+)', String.Escape),
            (r'\$' + allowed_variable, String.Interpol),
            (r'(\$)(\()', bygroups(String.Interpol, Punctuation), 'in-intp'),
            (r'.|\s', String),
        ],

        'regex': [
            (r'"', String.Regex, '#pop'),
            (r'\\"', String.Regex),
            (r'.|\s', String.Regex),
        ],

        'tqregex': [
            (r'"""', String.Regex, '#pop'),
            (r'.|\s', String.Regex),
        ],

        'command': [
            (r'`', String.Backtick, '#pop'),
            (r'\$' + allowed_variable, String.Interpol),
            (r'(\$)(\()', bygroups(String.Interpol, Punctuation), 'in-intp'),
            (r'.|\s', String.Backtick)
        ],

        'in-intp': [
            (r'\(', Punctuation, '#push'),
            (r'\)', Punctuation, '#pop'),
            include('root'),
        ]
    }

    def analyse_text(text):
        return shebang_matches(text, r'julia')


class JuliaConsoleLexer(Lexer):
    """
    For Julia console sessions. Modeled after MatlabSessionLexer.

    .. versionadded:: 1.6
    """
    name = 'Julia console'
    aliases = ['jlcon']

    def get_tokens_unprocessed(self, text):
        jllexer = JuliaLexer(**self.options)
        start = 0
        curcode = ''
        insertions = []
        output = False
        error = False

        for line in text.splitlines(True):
            if line.startswith('julia>'):
                insertions.append((len(curcode), [(0, Generic.Prompt, line[:6])]))
                curcode += line[6:]
                output = False
                error = False
            elif line.startswith('help?>') or line.startswith('shell>'):
                yield start, Generic.Prompt, line[:6]
                yield start + 6, Text, line[6:]
                output = False
                error = False
            elif line.startswith('      ') and not output:
                insertions.append((len(curcode), [(0, Text, line[:6])]))
                curcode += line[6:]
            else:
                if curcode:
                    yield from do_insertions(
                        insertions, jllexer.get_tokens_unprocessed(curcode))
                    curcode = ''
                    insertions = []
                if line.startswith('ERROR: ') or error:
                    yield start, Generic.Error, line
                    error = True
                else:
                    yield start, Generic.Output, line
                output = True
            start += len(line)

        if curcode:
            yield from do_insertions(
                insertions, jllexer.get_tokens_unprocessed(curcode))
