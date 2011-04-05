"""
    pygments.lexers.postgres
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for PostgreSQL-specific SQL and psql interactive session.

    :copyright: Copyright 2011 by Daniele Varrazzo.
    :license: BSD, see LICENSE for details.
"""

import re
import sys

from pygments.lexer import Lexer, RegexLexer, include, bygroups, using, \
     this, do_insertions
from pygments.token import Error, Punctuation, Literal, Token, \
     Text, Comment, Operator, Keyword, Name, String, Number, Generic
from pygments.lexers import get_lexer_by_name, ClassNotFound

from pygments.lexers._postgres_builtins import KEYWORDS, DATATYPES


__all__ = [ 'PostgresLexer', 'PostgresConsoleLexer' ]

line_re  = re.compile('.*?\n')

language_re = re.compile(r'\s+LANGUAGE\s+(\w+)')

class PostgresLexer(RegexLexer):
    """
    Lexer for the PostgreSQL dialect of SQL.
    """

    name = 'PostgreSQL SQL dialect'
    aliases = ['postgresql', 'postgres']
    mimetypes = ['text/x-postgresql']

    def get_tokens_unprocessed(self, text, *args):
        # Have a copy of the entire text to be used by `language_callback`.
        self.text = text
        for x in RegexLexer.get_tokens_unprocessed(self, text, *args):
            yield x

    def language_callback(self, match):
        lexer = None
        # TODO: the language can also be before the string
        m = language_re.match(self.text[match.end():])
        if m is not None:
            lexer = self._get_lexer(m.group(1))

        if lexer:
            yield (match.start(1), String, match.group(1))
            for x in lexer.get_tokens_unprocessed(match.group(2)):
                yield x
            yield (match.start(3), String, match.group(3))

        else:
            yield (match.start(), String, match.group())

    def _get_lexer(self, lang):
        if lang == 'sql':
            return get_lexer_by_name('postgresql', **self.options)

        tries = [ lang ]
        if lang.startswith('pl'):
            tries.append(lang[2:])
        if lang.endswith('u'):
            tries.append(lang[:-1])
        if lang.startswith('pl') and lang.endswith('u'):
            tries.append(lang[2:-1])

        for l in tries:
            try:
                return get_lexer_by_name(l, **self.options)
            except ClassNotFound:
                pass
        else:
            # TODO: better logging
            print >>sys.stderr, "language not found:", lang
            return None

    flags = re.IGNORECASE
    tokens = {
        'root': [
            (r'\s+', Text),
            (r'--.*?\n', Comment.Single),
            (r'/\*', Comment.Multiline, 'multiline-comments'),
            (r'(' + '|'.join([s.replace(" ", "\s+") for s in DATATYPES])
                  + r')\b', Name.Builtin),
            (r'(' + '|'.join(KEYWORDS) + r')\b', Keyword),
            (r'[+*/<>=~!@#%^&|`?^-]+', Operator),
            (r'::', Operator),  # cast
            (r'\$\d+', Name.Variable),
            (r'([0-9]*\.[0-9]*|[0-9]+)(e[+-]?[0-9]+)?', Number.Float),
            (r'[0-9]+', Number.Integer),
            # TODO: Backslash escapes?
            (r"E?'(''|[^'])*'", String.Single),
            (r'"(""|[^"])*"', String.Name), # quoted identifier
            (r'(?ms)(\$[^\$]*\$)(.*?)(\1)', language_callback),
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name),

            # TODO: consider splitting the regex parser
            (r'\\[^\s]+', Keyword.Pseudo, 'psql-command'),
            # psql variable in SQL
            (r""":(['"]?)[a-z][a-z0-9_]*\b\1""", Name.Variable),

            (r'[;:()\[\],\.]', Punctuation),
        ],
        'multiline-comments': [
            (r'/\*', Comment.Multiline, 'multiline-comments'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[^/\*]+', Comment.Multiline),
            (r'[/*]', Comment.Multiline)
        ],
        'psql-command': [
            (r'\n', Text, 'root'),
            (r'\s+', Text),
            (r""":(['"]?)[a-z][a-z0-9_]*\b\1""", Name.Variable),
            (r"'(''|[^'])*'", String.Single),
            (r"`([^`])*`", String.Backtick),
            (r"[^\s]+", String.Symbol),
        ]
    }

re_prompt = re.compile(r'^([a-zA-Z_][a-zA-Z0-9_]+)?[=\-\(][#>]')
re_psql_command = re.compile(r'(\s*)(\\.+?)(\s+)$')
re_error = re.compile(r'ERROR:')
re_message = re.compile(r'((?:DEBUG|INFO|NOTICE|WARNING|ERROR|HINT|LINE [0-9]+):)(.*?\n)')
re_charhint = re.compile(r'\s*\^\s*\n')

class PostgresConsoleLexer(Lexer):
    """
    Lexer for psql sessions.

    TODO: multiline comments are broken.
    """

    name = 'PostgreSQL console (psql)'
    aliases = ['psql', 'postgresql-console', 'postgres-console']
    mimetypes = ['text/x-postgresql-psql']

    def get_tokens_unprocessed(self, data):
        sql = PostgresLexer(**self.options)

        curcode = ''
        insertions = []
        out_token = Generic.Output
        for match in line_re.finditer(data):
            line = match.group()
            mprompt = re_prompt.match(line)
            if mprompt is not None:
                out_token = Generic.Output
                insertions.append((len(curcode),
                                   [(0, Generic.Prompt, mprompt.group())]))
                curcode += line[len(mprompt.group()):]
            else:
                if curcode:
                    for item in do_insertions(insertions,
                                              sql.get_tokens_unprocessed(curcode)):
                        yield item
                    curcode = ''
                    insertions = []
                mmsg = re_message.match(line)
                if mmsg is not None:
                    if mmsg.group(1).startswith("ERROR"):
                        out_token = Generic.Error
                    yield (mmsg.start(1), Generic.Strong, mmsg.group(1))
                    yield (mmsg.start(2), out_token, mmsg.group(2))
                elif re_charhint.match(line):
                    yield (match.start(), out_token, line)
                else:
                    yield (match.start(), Generic.Output, line)

        if curcode:
            for item in do_insertions(insertions,
                                      sql.get_tokens_unprocessed(curcode)):
                yield item


