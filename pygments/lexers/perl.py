"""
    pygments.lexers.perl
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for Perl and related languages.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace
from pygments.util import shebang_matches

# Backwards compatibility: Raku used to live here as ``Perl6Lexer``.
from pygments.lexers.raku import RakuLexer as Perl6Lexer  # noqa: F401

__all__ = ['PerlLexer']


class PerlLexer(RegexLexer):
    """
    For Perl source code.
    """

    name = 'Perl'
    url = 'https://www.perl.org'
    aliases = ['perl', 'pl']
    filenames = ['*.pl', '*.pm', '*.t', '*.perl']
    mimetypes = ['text/x-perl', 'application/x-perl']
    version_added = ''

    flags = re.DOTALL | re.MULTILINE
    # TODO: give this to a perl guy who knows how to parse perl...
    tokens = {
        'balanced-regex': [
            (r'/(\\\\|\\[^\\]|[^\\/])*/[egimosx]*', String.Regex, '#pop'),
            (r'!(\\\\|\\[^\\]|[^\\!])*![egimosx]*', String.Regex, '#pop'),
            (r'\\(\\\\|[^\\])*\\[egimosx]*', String.Regex, '#pop'),
            (r'\{(\\\\|\\[^\\]|[^\\}])*\}[egimosx]*', String.Regex, '#pop'),
            (r'<(\\\\|\\[^\\]|[^\\>])*>[egimosx]*', String.Regex, '#pop'),
            (r'\[(\\\\|\\[^\\]|[^\\\]])*\][egimosx]*', String.Regex, '#pop'),
            (r'\((\\\\|\\[^\\]|[^\\)])*\)[egimosx]*', String.Regex, '#pop'),
            (r'@(\\\\|\\[^\\]|[^\\@])*@[egimosx]*', String.Regex, '#pop'),
            (r'%(\\\\|\\[^\\]|[^\\%])*%[egimosx]*', String.Regex, '#pop'),
            (r'\$(\\\\|\\[^\\]|[^\\$])*\$[egimosx]*', String.Regex, '#pop'),
        ],
        'root': [
            (r'\A\#!.+?$', Comment.Hashbang),
            (r'\#.*?$', Comment.Single),
            (r'^=[a-zA-Z0-9]+\s+.*?\n=cut', Comment.Multiline),
            (words((
                'case', 'continue', 'do', 'else', 'elsif', 'for', 'foreach',
                'if', 'last', 'my', 'next', 'our', 'redo', 'reset', 'then',
                'unless', 'until', 'while', 'print', 'new', 'BEGIN',
                'CHECK', 'INIT', 'END', 'return'), suffix=r'\b'),
             Keyword),
            (r'(format)(\s+)(\w+)(\s*)(=)(\s*\n)',
             bygroups(Keyword, Whitespace, Name, Whitespace, Punctuation, Whitespace), 'format'),
            (r'(eq|lt|gt|le|ge|ne|not|and|or|cmp)\b', Operator.Word),
            # common delimiters
            (r's/(\\\\|\\[^\\]|[^\\/])*/(\\\\|\\[^\\]|[^\\/])*/[egimosx]*',
                String.Regex),
            (r's!(\\\\|\\!|[^!])*!(\\\\|\\!|[^!])*![egimosx]*', String.Regex),
            (r's\\(\\\\|[^\\])*\\(\\\\|[^\\])*\\[egimosx]*', String.Regex),
            (r's@(\\\\|\\[^\\]|[^\\@])*@(\\\\|\\[^\\]|[^\\@])*@[egimosx]*',
                String.Regex),
            (r's%(\\\\|\\[^\\]|[^\\%])*%(\\\\|\\[^\\]|[^\\%])*%[egimosx]*',
                String.Regex),
            # balanced delimiters
            (r's\{(\\\\|\\[^\\]|[^\\}])*\}\s*', String.Regex, 'balanced-regex'),
            (r's<(\\\\|\\[^\\]|[^\\>])*>\s*', String.Regex, 'balanced-regex'),
            (r's\[(\\\\|\\[^\\]|[^\\\]])*\]\s*', String.Regex,
                'balanced-regex'),
            (r's\((\\\\|\\[^\\]|[^\\)])*\)\s*', String.Regex,
                'balanced-regex'),

            (r'm?/(\\\\|\\[^\\]|[^\\/\n])*/[gcimosx]*', String.Regex),
            (r'm(?=[/!\\{<\[(@%$])', String.Regex, 'balanced-regex'),
            (r'((?<==~)|(?<=\())\s*/(\\\\|\\[^\\]|[^\\/])*/[gcimosx]*',
                String.Regex),
            (r'\s+', Whitespace),
            (words((
                'abs', 'accept', 'alarm', 'atan2', 'bind', 'binmode', 'bless', 'caller', 'chdir',
                'chmod', 'chomp', 'chop', 'chown', 'chr', 'chroot', 'close', 'closedir', 'connect',
                'continue', 'cos', 'crypt', 'dbmclose', 'dbmopen', 'defined', 'delete', 'die',
                'dump', 'each', 'endgrent', 'endhostent', 'endnetent', 'endprotoent',
                'endpwent', 'endservent', 'eof', 'eval', 'exec', 'exists', 'exit', 'exp', 'fcntl',
                'fileno', 'flock', 'fork', 'format', 'formline', 'getc', 'getgrent', 'getgrgid',
                'getgrnam', 'gethostbyaddr', 'gethostbyname', 'gethostent', 'getlogin',
                'getnetbyaddr', 'getnetbyname', 'getnetent', 'getpeername', 'getpgrp',
                'getppid', 'getpriority', 'getprotobyname', 'getprotobynumber',
                'getprotoent', 'getpwent', 'getpwnam', 'getpwuid', 'getservbyname',
                'getservbyport', 'getservent', 'getsockname', 'getsockopt', 'glob', 'gmtime',
                'goto', 'grep', 'hex', 'import', 'index', 'int', 'ioctl', 'join', 'keys', 'kill', 'last',
                'lc', 'lcfirst', 'length', 'link', 'listen', 'local', 'localtime', 'log', 'lstat',
                'map', 'mkdir', 'msgctl', 'msgget', 'msgrcv', 'msgsnd', 'my', 'next', 'oct', 'open',
                'opendir', 'ord', 'our', 'pack', 'pipe', 'pop', 'pos', 'printf',
                'prototype', 'push', 'quotemeta', 'rand', 'read', 'readdir',
                'readline', 'readlink', 'readpipe', 'recv', 'redo', 'ref', 'rename',
                'reverse', 'rewinddir', 'rindex', 'rmdir', 'scalar', 'seek', 'seekdir',
                'select', 'semctl', 'semget', 'semop', 'send', 'setgrent', 'sethostent', 'setnetent',
                'setpgrp', 'setpriority', 'setprotoent', 'setpwent', 'setservent',
                'setsockopt', 'shift', 'shmctl', 'shmget', 'shmread', 'shmwrite', 'shutdown',
                'sin', 'sleep', 'socket', 'socketpair', 'sort', 'splice', 'split', 'sprintf', 'sqrt',
                'srand', 'stat', 'study', 'substr', 'symlink', 'syscall', 'sysopen', 'sysread',
                'sysseek', 'system', 'syswrite', 'tell', 'telldir', 'tie', 'tied', 'time', 'times', 'tr',
                'truncate', 'uc', 'ucfirst', 'umask', 'undef', 'unlink', 'unpack', 'unshift', 'untie',
                'utime', 'values', 'vec', 'wait', 'waitpid', 'wantarray', 'warn', 'write'), suffix=r'\b'),
             Name.Builtin),
            (r'((__(DATA|DIE|WARN)__)|(STD(IN|OUT|ERR)))\b', Name.Builtin.Pseudo),
            (r'(<<)([\'"]?)([a-zA-Z_]\w*)(\2;?\n.*?\n)(\3)(\n)',
             bygroups(String, String, String.Delimiter, String, String.Delimiter, Whitespace)),
            (r'__END__', Comment.Preproc, 'end-part'),
            (r'\$\^[ADEFHILMOPSTWX]', Name.Variable.Global),
            (r"\$[\\\"\[\]'&`+*.,;=%~?@$!<>(^|/-](?!\w)", Name.Variable.Global),
            (r'[$@%#]+', Name.Variable, 'varname'),
            (r'0_?[0-7]+(_[0-7]+)*', Number.Oct),
            (r'0x[0-9A-Fa-f]+(_[0-9A-Fa-f]+)*', Number.Hex),
            (r'0b[01]+(_[01]+)*', Number.Bin),
            (r'(?i)(\d*(_\d*)*\.\d+(_\d*)*|\d+(_\d*)*\.\d+(_\d*)*)(e[+-]?\d+)?',
             Number.Float),
            (r'(?i)\d+(_\d*)*e[+-]?\d+(_\d*)*', Number.Float),
            (r'\d+(_\d+)*', Number.Integer),
            (r"'(\\\\|\\[^\\]|[^'\\])*'", String),
            (r'"(\\\\|\\[^\\]|[^"\\])*"', String),
            (r'`(\\\\|\\[^\\]|[^`\\])*`', String.Backtick),
            (r'<([^\s>]+)>', String.Regex),
            (r'(q|qq|qw|qr|qx)\{', String.Other, 'cb-string'),
            (r'(q|qq|qw|qr|qx)\(', String.Other, 'rb-string'),
            (r'(q|qq|qw|qr|qx)\[', String.Other, 'sb-string'),
            (r'(q|qq|qw|qr|qx)\<', String.Other, 'lt-string'),
            (r'(q|qq|qw|qr|qx)([\W_])[\s\S]*?\2', String.Other),
            (r'(package)(\s+)([a-zA-Z_]\w*(?:::[a-zA-Z_]\w*)*)',
             bygroups(Keyword, Whitespace, Name.Namespace)),
            (r'(use|require|no)(\s+)([a-zA-Z_]\w*(?:::[a-zA-Z_]\w*)*)',
             bygroups(Keyword, Whitespace, Name.Namespace)),
            (r'(sub)(\s+)', bygroups(Keyword, Whitespace), 'funcname'),
            (words((
                'no', 'package', 'require', 'use'), suffix=r'\b'),
             Keyword),
            (r'(\[\]|\*\*|::|<<|>>|>=|<=>|<=|={3}|!=|=~|'
             r'!~|&&?|\|\||\.{1,3})', Operator),
            (r'[-+/*%=<>&^|!\\~]=?', Operator),
            (r'[()\[\]:;,<>/?{}]', Punctuation),  # yes, there's no shortage
                                                  # of punctuation in Perl!
            (r'(?=\w)', Name, 'name'),
        ],
        'format': [
            (r'\.\n', String.Interpol, '#pop'),
            (r'[^\n]*\n', String.Interpol),
        ],
        'varname': [
            (r'\s+', Whitespace),
            (r'\{', Punctuation, '#pop'),    # hash syntax?
            (r'\)|,', Punctuation, '#pop'),  # argument specifier
            (r'\w+::', Name.Namespace),
            (r'[\w:]+', Name.Variable, '#pop'),
        ],
        'name': [
            (r'[a-zA-Z_]\w*(::[a-zA-Z_]\w*)*(::)?(?=\s*->)', Name.Namespace, '#pop'),
            (r'[a-zA-Z_]\w*(::[a-zA-Z_]\w*)*::', Name.Namespace, '#pop'),
            (r'[\w:]+', Name, '#pop'),
            (r'[A-Z_]+(?=\W)', Name.Constant, '#pop'),
            (r'(?=\W)', Text, '#pop'),
        ],
        'funcname': [
            (r'[a-zA-Z_]\w*[!?]?', Name.Function),
            (r'\s+', Whitespace),
            # argument declaration
            (r'(\([$@%]*\))(\s*)', bygroups(Punctuation, Whitespace)),
            (r';', Punctuation, '#pop'),
            (r'.*?\{', Punctuation, '#pop'),
        ],
        'cb-string': [
            (r'\\[{}\\]', String.Other),
            (r'\\', String.Other),
            (r'\{', String.Other, 'cb-string'),
            (r'\}', String.Other, '#pop'),
            (r'[^{}\\]+', String.Other)
        ],
        'rb-string': [
            (r'\\[()\\]', String.Other),
            (r'\\', String.Other),
            (r'\(', String.Other, 'rb-string'),
            (r'\)', String.Other, '#pop'),
            (r'[^()]+', String.Other)
        ],
        'sb-string': [
            (r'\\[\[\]\\]', String.Other),
            (r'\\', String.Other),
            (r'\[', String.Other, 'sb-string'),
            (r'\]', String.Other, '#pop'),
            (r'[^\[\]]+', String.Other)
        ],
        'lt-string': [
            (r'\\[<>\\]', String.Other),
            (r'\\', String.Other),
            (r'\<', String.Other, 'lt-string'),
            (r'\>', String.Other, '#pop'),
            (r'[^<>]+', String.Other)
        ],
        'end-part': [
            (r'.+', Comment.Preproc, '#pop')
        ]
    }

    def analyse_text(text):
        if shebang_matches(text, r'perl'):
            return True

        result = 0

        if re.search(r'(?:my|our)\s+[$@%(]', text):
            result += 0.9

        if ':=' in text:
            # := is not valid Perl, but it appears in unicon, so we should
            # become less confident if we think we found Perl with :=
            result /= 2

        return result
