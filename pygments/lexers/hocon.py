"""
    pygments.lexers.hocon
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for the HOCON configuration format.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, default, include, words
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Whitespace

__all__ = ['HoconLexer']


class HoconLexer(RegexLexer):
    """
    Lexer for the `HOCON <https://github.com/lightbend/config/blob/main/HOCON.md>`_
    (Human-Optimized Config Object Notation) configuration format used by the
    Lightbend Config library and tools built on it (Akka/Pekko, Play, Lagom,
    Scala Steward, ...).

    Whether an unquoted token is a key or a string value is positional in
    HOCON, so the lexer tracks a ``value`` state that begins after a
    key/value separator (``=``, ``:``, ``+=``) and ends at a newline or
    a structural delimiter.
    """

    name = 'HOCON'
    aliases = ['hocon']
    filenames = ['*.conf']
    mimetypes = ['application/hocon']
    url = 'https://github.com/lightbend/config/blob/main/HOCON.md'
    version_added = '2.21'

    _duration_size = (
        r'ns|us|ms|s|m|h|d|w|y'
        r'|nano(?:second)?s?|micro(?:second)?s?|milli(?:second)?s?'
        r'|second(?:s)?|minute(?:s)?|hour(?:s)?|day(?:s)?|week(?:s)?'
        r'|month(?:s)?|year(?:s)?|mo'
        r'|[KMGTPEZY]i?B?|[kmgtpezy]B?|B|byte(?:s)?'
    )

    tokens = {
        'whitespace_inline': [
            (r'[ \t]+', Whitespace),
        ],
        'comments': [
            (r'(#|//)[^\r\n]*', Comment.Single),
        ],
        'string': [
            (r'"""', String.Double, 'multistring'),
            (r'"', String.Double, 'qstring'),
        ],
        'multistring': [
            # HOCON multi-line strings do not interpret escapes; the closing
            # delimiter is ``"""`` and any extra trailing ``"`` characters
            # belong to the string value.
            (r'[^"]+', String.Double),
            (r'"""(?!")', String.Double, '#pop'),
            (r'"', String.Double),
        ],
        'qstring': [
            (r'\\.', String.Escape),
            (r'[^"\\\r\n]+', String.Double),
            (r'"', String.Double, '#pop'),
        ],
        'substitution': [
            (r'\$\{\??', String.Interpol, 'substitution_body'),
        ],
        'substitution_body': [
            (r'\}', String.Interpol, '#pop'),
            (r'[^${}"\s.]+', Name.Variable),
            (r'\.', Punctuation),
            (r'[ \t]+', Whitespace),
        ],
        'constants': [
            (words(('true', 'false', 'null', 'yes', 'no', 'on', 'off'),
                   suffix=r'(?![\w-])'),
             Keyword.Constant),
        ],
        'numbers': [
            # Number with an optional duration/size suffix. Recognising the
            # suffix here keeps unquoted strings like ``3 days`` lexing as
            # a number followed by a duration unit.
            (r'(-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?)'
             r'([ \t]*)'
             r'(' + _duration_size + r')(?![\w-])',
             bygroups(Number, Whitespace, Keyword.Type)),
            (r'-?(?:0|[1-9]\d*)\.\d+(?:[eE][+-]?\d+)?', Number.Float),
            (r'-?(?:0|[1-9]\d*)(?:[eE][+-]?\d+)?', Number.Integer),
        ],
        'include_args': [
            (r'[ \t]+', Whitespace),
            (words(('required', 'url', 'file', 'classpath'),
                   suffix=r'(?=\s*\()'),
             Keyword.Namespace),
            (r'[()]', Punctuation),
            include('string'),
            (r'[\r\n]', Whitespace, '#pop'),
        ],
        'root': [
            (r'\s+', Whitespace),
            include('comments'),
            (r'\b(include)\b', Keyword.Namespace, 'include_args'),
            (r'\{', Punctuation, 'object'),
            (r'\}', Punctuation),
            (r'\[', Punctuation, 'array'),
            include('key'),
        ],
        'object': [
            (r'\s+', Whitespace),
            include('comments'),
            (r'\}', Punctuation, '#pop'),
            (r'\{', Punctuation, 'object'),
            include('key'),
        ],
        'array': [
            (r'\s+', Whitespace),
            include('comments'),
            (r'\]', Punctuation, '#pop'),
            # Inside an array each comma- or newline-separated entry is a
            # value, so jump straight into ``value`` state.
            default('value'),
        ],
        'key': [
            (r'"', String.Double, ('value_sep', 'qstring')),
            # Bare path key: dotted identifier-ish run. Stop at whitespace,
            # separators, or structural punctuation so the value rules below
            # take over on the next iteration.
            (r'[^\s${}\[\],:=+#"]+', Name.Attribute, 'value_sep'),
        ],
        'value_sep': [
            (r'[ \t]+', Whitespace),
            (r'\+=|[=:]', Operator, ('#pop', 'value')),
            # ``foo { ... }`` — the separator before ``{`` is optional, and
            # any other unmatched character (e.g. a newline or closing
            # brace) means the key had no value and we should hand back
            # control to the surrounding state.
            default('#pop'),
        ],
        'value': [
            include('whitespace_inline'),
            include('comments'),
            (r'[\r\n]+', Whitespace, '#pop'),
            (r',', Punctuation, '#pop'),
            (r'\{', Punctuation, 'object'),
            (r'\[', Punctuation, 'array'),
            include('substitution'),
            include('string'),
            include('constants'),
            include('numbers'),
            # Unquoted value text. HOCON forbids many characters here;
            # treating each run as ``String`` is good enough for
            # highlighting and folds value concatenation into one token.
            (r'[^\s${}\[\],:=+#"/]+', String),
            (r'/(?!/)', String),
            (r'\+(?!=)', String),
            # Anything else (a closing ``]`` or ``}``) ends the value; let
            # the surrounding state consume it.
            default('#pop'),
        ],
    }
