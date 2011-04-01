"""
    pygments.lexers.postgres
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for PostgreSQL-specific SQL and psql interactive session.

    :copyright: Copyright 2011 by Daniele Varrazzo.
    :license: BSD, see LICENSE for details.
"""

import re
import urllib2

from pygments.lexer import Lexer, RegexLexer, include, bygroups, using, \
     this, do_insertions
from pygments.token import Error, Punctuation, Literal, Token, \
     Text, Comment, Operator, Keyword, Name, String, Number, Generic

from pygments.lexers._postgres_builtins import KEYWORDS, DATATYPES


__all__ = [ 'PostgresLexer', 'PostgresConsoleLexer' ]

line_re  = re.compile('.*?\n')


class PostgresLexer(RegexLexer):
    """
    Lexer for the PostgreSQL dialect of SQL.
    """

    name = 'PostgreSQL SQL dialect'
    aliases = ['postgresql', 'postgres']
    mimetypes = ['text/x-postgresql']

    flags = re.IGNORECASE
    tokens = {
        'root': [
            (r'\s+', Text),
            (r'--.*?\n', Comment.Single),
            (r'/\*', Comment.Multiline, 'multiline-comments'),
            (r'(' + '|'.join(KEYWORDS) + r')\b', Keyword),
            (r'(' + '|'.join([s.replace(" ", "\s+") for s in DATATYPES])
                  + r')\b', Name.Builtin),
            (r'[+*/<>=~!@#%^&|`?^-]', Operator),
            (r'::', Operator),  # cast
            (r'([0-9]*\.[0-9]*|[0-9]+)(e[+-]?[0-9]+)?', Number.Float),
            (r'[0-9]+', Number.Integer),
            # TODO: Backslash escapes?
            (r"'(''|[^'])*'", String.Single),
            (r'"(""|[^"])*"', String.Name), # quoted identifier
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name),
            (r'[;:()\[\],\.]', Punctuation),
            # psql backslash command.
            # This actually belongs to the console lexer,
            # but putting it here makes things easier.
            (r'\\.*?\n', Name),             # TODO: what is a good token?
        ],
        'multiline-comments': [
            (r'/\*', Comment.Multiline, 'multiline-comments'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[^/\*]+', Comment.Multiline),
            (r'[/*]', Comment.Multiline)
        ]
    }

re_prompt = re.compile(r'^([a-zA-Z_][a-zA-Z0-9_]+)?[=\-\(]#')
re_psql_command = re.compile(r'(\s*)(\\.+?)(\s+)$')
re_error = re.compile(r'ERROR:')
re_message = re.compile(r'(DEBUG|INFO|WARNING|ERROR|HINT|LINE [0-9]+:?)(.*?\n)')
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


