"""
    pygments.lexers.postgres
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for PostgreSQL-specific SQL and psql interactive session.

    :copyright: Copyright 2011 by Daniele Varrazzo.
    :license: BSD, see LICENSE for details.
"""

import re
import sys
from copy import deepcopy

from pygments.lexer import Lexer, RegexLexer, include, bygroups, using, \
     this, do_insertions
from pygments.token import Error, Punctuation, Literal, Token, \
     Text, Comment, Operator, Keyword, Name, String, Number, Generic
from pygments.lexers import get_lexer_by_name, ClassNotFound

from pygments.lexers._postgres_builtins import (
    KEYWORDS, DATATYPES, PSEUDO_TYPES, PLPGSQL_KEYWORDS)


__all__ = [ 'PostgresLexer', 'PlPgsqlLexer', 'PostgresConsoleLexer' ]

line_re  = re.compile('.*?\n')

language_re = re.compile(r"\s+LANGUAGE\s+'?(\w+)'?")

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
            (r'(' + '|'.join([s.replace(" ", "\s+")
                for s in DATATYPES + PSEUDO_TYPES])
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
    }


class PlPgsqlLexer(PostgresLexer):
    """
    Handle the extra syntax in Pl/pgSQL language.
    """
    name = 'PL/pgSQL'
    aliases = ['plpgsql']
    mimetypes = ['text/x-plpgsql']
    tokens = deepcopy(PostgresLexer.tokens)

    # extend the keywords list
    for i, pattern in enumerate(tokens['root']):
        if pattern[1] == Keyword:
            tokens['root'][i] = (
                r'(' + '|'.join(KEYWORDS + PLPGSQL_KEYWORDS) + r')\b',
                Keyword)
            del i
            break
    else:
        assert 0, "SQL keywords not found"

    tokens['root'][:0] = [
        (r'\%[a-z][a-z0-9_]*\b', Name.Builtin),     # actually, a datatype
        (r':=', Operator),
        (r'\<\<[a-z][a-z0-9_]*\>\>', Name.Label),
        (r'\#[a-z][a-z0-9_]*\b', Keyword.Pseudo),   # #variable_conflict
    ]


class PsqlRegexLexer(PostgresLexer):
    """
    Extend the PostgresLexer adding support specific for psql commands.

    This is not a complete psql lexer yet as it lacks prompt support
    and output rendering.
    """
    name = 'PostgreSQL console - regexp based lexer'
    aliases = []    # not public
    tokens = deepcopy(PostgresLexer.tokens)
    tokens['root'].append(
        (r'\\[^\s]+', Keyword.Pseudo, 'psql-command'))
    tokens['psql-command'] = [
        (r'\n', Text, 'root'),
        (r'\s+', Text),
        (r'\\[^\s]+', Keyword.Pseudo),
        (r""":(['"]?)[a-z][a-z0-9_]*\b\1""", Name.Variable),
        (r"'(''|[^'])*'", String.Single),
        (r"`([^`])*`", String.Backtick),
        (r"[^\s]+", String.Symbol),
    ]

re_prompt = re.compile(r'^.*?[=\-\(][#>]')
re_psql_command = re.compile(r'\s*\\')
re_end_command = re.compile(r';\s*(--.*?)?$')
re_psql_command = re.compile(r'(\s*)(\\.+?)(\s+)$')
re_error = re.compile(r'(ERROR|FATAL):')
re_message = re.compile(
    r'((?:DEBUG|INFO|NOTICE|WARNING|ERROR|'
    r'FATAL|HINT|DETAIL|LINE [0-9]+):)(.*?\n)')
re_charhint = re.compile(r'\s*\^\s*\n')

def lookahead(x):
    """Wrap an iterator and allow pushing back an item."""
    for i in x:
        while 1:
            i = yield i
            if i is None:
                break
            yield i


class PostgresConsoleLexer(Lexer):
    """
    Lexer for psql sessions.
    """

    name = 'PostgreSQL console (psql)'
    aliases = ['psql', 'postgresql-console', 'postgres-console']
    mimetypes = ['text/x-postgresql-psql']

    def get_tokens_unprocessed(self, data):
        sql = PsqlRegexLexer(**self.options)

        lines = lookahead(line_re.findall(data))

        # prompt-output cycle
        while 1:

            # consume the lines of the command: start with an optional prompt
            # and continue until the end of command is detected
            curcode = ''
            insertions = []
            while 1:
                try:
                    line = lines.next()
                except StopIteration:
                    # allow the emission of partially collected items
                    # the repl loop will be broken below
                    break

                # Identify a shell prompt in case of psql commandline example
                if line.startswith('$'):
                    lexer = get_lexer_by_name('console', **self.options)
                    for x in lexer.get_tokens_unprocessed(line):
                        yield x
                    break

                # Identify a psql prompt
                mprompt = re_prompt.match(line)
                if mprompt is not None:
                    insertions.append((len(curcode),
                                       [(0, Generic.Prompt, mprompt.group())]))
                    curcode += line[len(mprompt.group()):]
                else:
                    curcode += line

                # Check if this is the end of the command
                # TODO: better handle multiline comments at the end with
                # a lexer with an external state?
                if re_psql_command.match(curcode) \
                or re_end_command.search(curcode):
                    break

            # Emit the combined stream of command and prompt(s)
            for item in do_insertions(insertions,
                    sql.get_tokens_unprocessed(curcode)):
                yield item

            # Emit the output lines
            out_token = Generic.Output
            while 1:
                line = lines.next()
                mprompt = re_prompt.match(line)
                if mprompt is not None:
                    # push the line back to have it processed by the prompt
                    lines.send(line)
                    break

                mmsg = re_message.match(line)
                if mmsg is not None:
                    if mmsg.group(1).startswith("ERROR") \
                    or mmsg.group(1).startswith("FATAL"):
                        out_token = Generic.Error
                    yield (mmsg.start(1), Generic.Strong, mmsg.group(1))
                    yield (mmsg.start(2), out_token, mmsg.group(2))
                elif re_charhint.match(line):
                    yield (0, out_token, line)
                else:
                    yield (0, Generic.Output, line)


