# -*- coding: utf-8 -*-
"""
    pygments.lexers.python
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Python and related languages.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer, RegexLexer, include, bygroups, using, \
    default, words, combined, do_insertions
from pygments.util import get_bool_opt, shebang_matches
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Generic, Other, Error
from pygments import unistring as uni

__all__ = ['PythonLexer', 'PythonConsoleLexer', 'PythonTracebackLexer',
           'Python3Lexer', 'Python3TracebackLexer', 'CythonLexer',
           'HyLexer', 'DgLexer']

line_re = re.compile('.*?\n')


class PythonLexer(RegexLexer):
    """
    For `Python <http://www.python.org>`_ source code.
    """

    name = 'Python'
    aliases = ['python', 'py', 'sage']
    filenames = ['*.py', '*.pyw', '*.sc', 'SConstruct', 'SConscript', '*.tac', '*.sage']
    mimetypes = ['text/x-python', 'application/x-python']

    tokens = {
        'root': [
            (r'\n', Text),
            (r'^(\s*)([rRuU]{,2}"""(?:.|\n)*?""")', bygroups(Text, String.Doc)),
            (r"^(\s*)([rRuU]{,2}'''(?:.|\n)*?''')", bygroups(Text, String.Doc)),
            (r'[^\S\n]+', Text),
            (r'#.*$', Comment),
            (r'[]{}:(),;[]', Punctuation),
            (r'\\\n', Text),
            (r'\\', Text),
            (r'(in|is|and|or|not)\b', Operator.Word),
            (r'!=|==|<<|>>|[-~+/*%=<>&^|.]', Operator),
            include('keywords'),
            (r'(def)((?:\s|\\\s)+)', bygroups(Keyword, Text), 'funcname'),
            (r'(class)((?:\s|\\\s)+)', bygroups(Keyword, Text), 'classname'),
            (r'(from)((?:\s|\\\s)+)', bygroups(Keyword.Namespace, Text),
             'fromimport'),
            (r'(import)((?:\s|\\\s)+)', bygroups(Keyword.Namespace, Text),
             'import'),
            include('builtins'),
            include('backtick'),
            ('(?:[rR]|[uU][rR]|[rR][uU])"""', String, 'tdqs'),
            ("(?:[rR]|[uU][rR]|[rR][uU])'''", String, 'tsqs'),
            ('(?:[rR]|[uU][rR]|[rR][uU])"', String, 'dqs'),
            ("(?:[rR]|[uU][rR]|[rR][uU])'", String, 'sqs'),
            ('[uU]?"""', String, combined('stringescape', 'tdqs')),
            ("[uU]?'''", String, combined('stringescape', 'tsqs')),
            ('[uU]?"', String, combined('stringescape', 'dqs')),
            ("[uU]?'", String, combined('stringescape', 'sqs')),
            include('name'),
            include('numbers'),
        ],
        'keywords': [
            (r'(assert|break|continue|del|elif|else|except|exec|'
             r'finally|for|global|if|lambda|pass|print|raise|'
             r'return|try|while|yield(\s+from)?|as|with)\b', Keyword),
        ],
        'builtins': [
            (r'(?<!\.)(__import__|abs|all|any|apply|basestring|bin|bool|buffer|'
             r'bytearray|bytes|callable|chr|classmethod|cmp|coerce|compile|'
             r'complex|delattr|dict|dir|divmod|enumerate|eval|execfile|exit|'
             r'file|filter|float|frozenset|getattr|globals|hasattr|hash|hex|id|'
             r'input|int|intern|isinstance|issubclass|iter|len|list|locals|'
             r'long|map|max|min|next|object|oct|open|ord|pow|property|range|'
             r'raw_input|reduce|reload|repr|reversed|round|set|setattr|slice|'
             r'sorted|staticmethod|str|sum|super|tuple|type|unichr|unicode|'
             r'vars|xrange|zip)\b', Name.Builtin),
            (r'(?<!\.)(self|None|Ellipsis|NotImplemented|False|True'
             r')\b', Name.Builtin.Pseudo),
            (r'(?<!\.)(ArithmeticError|AssertionError|AttributeError|'
             r'BaseException|DeprecationWarning|EOFError|EnvironmentError|'
             r'Exception|FloatingPointError|FutureWarning|GeneratorExit|IOError|'
             r'ImportError|ImportWarning|IndentationError|IndexError|KeyError|'
             r'KeyboardInterrupt|LookupError|MemoryError|NameError|'
             r'NotImplemented|NotImplementedError|OSError|OverflowError|'
             r'OverflowWarning|PendingDeprecationWarning|ReferenceError|'
             r'RuntimeError|RuntimeWarning|StandardError|StopIteration|'
             r'SyntaxError|SyntaxWarning|SystemError|SystemExit|TabError|'
             r'TypeError|UnboundLocalError|UnicodeDecodeError|'
             r'UnicodeEncodeError|UnicodeError|UnicodeTranslateError|'
             r'UnicodeWarning|UserWarning|ValueError|VMSError|Warning|'
             r'WindowsError|ZeroDivisionError)\b', Name.Exception),
        ],
        'numbers': [
            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?j?', Number.Float),
            (r'\d+[eE][+-]?[0-9]+j?', Number.Float),
            (r'0[0-7]+j?', Number.Oct),
            (r'0[bB][01]+', Number.Bin),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+j?', Number.Integer)
        ],
        'backtick': [
            ('`.*?`', String.Backtick),
        ],
        'name': [
            (r'@[\w.]+', Name.Decorator),
            ('[a-zA-Z_]\w*', Name),
        ],
        'funcname': [
            ('[a-zA-Z_]\w*', Name.Function, '#pop')
        ],
        'classname': [
            ('[a-zA-Z_]\w*', Name.Class, '#pop')
        ],
        'import': [
            (r'(?:[ \t]|\\\n)+', Text),
            (r'as\b', Keyword.Namespace),
            (r',', Operator),
            (r'[a-zA-Z_][\w.]*', Name.Namespace),
            default('#pop')  # all else: go back
        ],
        'fromimport': [
            (r'(?:[ \t]|\\\n)+', Text),
            (r'import\b', Keyword.Namespace, '#pop'),
            # if None occurs here, it's "raise x from None", since None can
            # never be a module name
            (r'None\b', Name.Builtin.Pseudo, '#pop'),
            # sadly, in "raise x from y" y will be highlighted as namespace too
            (r'[a-zA-Z_.][\w.]*', Name.Namespace),
            # anything else here also means "raise x from y" and is therefore
            # not an error
            default('#pop'),
        ],
        'stringescape': [
            (r'\\([\\abfnrtv"\']|\n|N{.*?}|u[a-fA-F0-9]{4}|'
             r'U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})', String.Escape)
        ],
        'strings': [
            (r'%(\(\w+\))?[-#0 +]*([0-9]+|[*])?(\.([0-9]+|[*]))?'
             '[hlL]?[diouxXeEfFgGcrs%]', String.Interpol),
            (r'[^\\\'"%\n]+', String),
            # quotes, percents and backslashes must be parsed one at a time
            (r'[\'"\\]', String),
            # unhandled string formatting sign
            (r'%', String)
            # newlines are an error (use "nl" state)
        ],
        'nl': [
            (r'\n', String)
        ],
        'dqs': [
            (r'"', String, '#pop'),
            (r'\\\\|\\"|\\\n', String.Escape),  # included here for raw strings
            include('strings')
        ],
        'sqs': [
            (r"'", String, '#pop'),
            (r"\\\\|\\'|\\\n", String.Escape),  # included here for raw strings
            include('strings')
        ],
        'tdqs': [
            (r'"""', String, '#pop'),
            include('strings'),
            include('nl')
        ],
        'tsqs': [
            (r"'''", String, '#pop'),
            include('strings'),
            include('nl')
        ],
    }

    def analyse_text(text):
        return shebang_matches(text, r'pythonw?(2(\.\d)?)?') or \
            'import ' in text[:1000]


class Python3Lexer(RegexLexer):
    """
    For `Python <http://www.python.org>`_ source code (version 3.0).

    .. versionadded:: 0.10
    """

    name = 'Python 3'
    aliases = ['python3', 'py3']
    filenames = []  # Nothing until Python 3 gets widespread
    mimetypes = ['text/x-python3', 'application/x-python3']

    flags = re.MULTILINE | re.UNICODE

    uni_name = "[%s][%s]*" % (uni.xid_start, uni.xid_continue)

    tokens = PythonLexer.tokens.copy()
    tokens['keywords'] = [
        (r'(assert|break|continue|del|elif|else|except|'
         r'finally|for|global|if|lambda|pass|raise|nonlocal|'
         r'return|try|while|yield(\s+from)?|as|with|True|False|None)\b',
         Keyword),
    ]
    tokens['builtins'] = [
        (r'(?<!\.)(__import__|abs|all|any|bin|bool|bytearray|bytes|'
         r'chr|classmethod|cmp|compile|complex|delattr|dict|dir|'
         r'divmod|enumerate|eval|filter|float|format|frozenset|getattr|'
         r'globals|hasattr|hash|hex|id|input|int|isinstance|issubclass|'
         r'iter|len|list|locals|map|max|memoryview|min|next|object|oct|'
         r'open|ord|pow|print|property|range|repr|reversed|round|'
         r'set|setattr|slice|sorted|staticmethod|str|sum|super|tuple|type|'
         r'vars|zip)\b', Name.Builtin),
        (r'(?<!\.)(self|Ellipsis|NotImplemented)\b', Name.Builtin.Pseudo),
        (r'(?<!\.)(ArithmeticError|AssertionError|AttributeError|'
         r'BaseException|BufferError|BytesWarning|DeprecationWarning|'
         r'EOFError|EnvironmentError|Exception|FloatingPointError|'
         r'FutureWarning|GeneratorExit|IOError|ImportError|'
         r'ImportWarning|IndentationError|IndexError|KeyError|'
         r'KeyboardInterrupt|LookupError|MemoryError|NameError|'
         r'NotImplementedError|OSError|OverflowError|'
         r'PendingDeprecationWarning|ReferenceError|'
         r'RuntimeError|RuntimeWarning|StopIteration|'
         r'SyntaxError|SyntaxWarning|SystemError|SystemExit|TabError|'
         r'TypeError|UnboundLocalError|UnicodeDecodeError|'
         r'UnicodeEncodeError|UnicodeError|UnicodeTranslateError|'
         r'UnicodeWarning|UserWarning|ValueError|VMSError|Warning|'
         r'WindowsError|ZeroDivisionError|'
         # new builtin exceptions from PEP 3151
         r'BlockingIOError|ChildProcessError|ConnectionError|'
         r'BrokenPipeError|ConnectionAbortedError|ConnectionRefusedError|'
         r'ConnectionResetError|FileExistsError|FileNotFoundError|'
         r'InterruptedError|IsADirectoryError|NotADirectoryError|'
         r'PermissionError|ProcessLookupError|TimeoutError)\b',
         Name.Exception),
    ]
    tokens['numbers'] = [
        (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
        (r'0[oO][0-7]+', Number.Oct),
        (r'0[bB][01]+', Number.Bin),
        (r'0[xX][a-fA-F0-9]+', Number.Hex),
        (r'\d+', Number.Integer)
    ]
    tokens['backtick'] = []
    tokens['name'] = [
        (r'@\w+', Name.Decorator),
        (uni_name, Name),
    ]
    tokens['funcname'] = [
        (uni_name, Name.Function, '#pop')
    ]
    tokens['classname'] = [
        (uni_name, Name.Class, '#pop')
    ]
    tokens['import'] = [
        (r'(\s+)(as)(\s+)', bygroups(Text, Keyword, Text)),
        (r'\.', Name.Namespace),
        (uni_name, Name.Namespace),
        (r'(\s*)(,)(\s*)', bygroups(Text, Operator, Text)),
        default('#pop')  # all else: go back
    ]
    tokens['fromimport'] = [
        (r'(\s+)(import)\b', bygroups(Text, Keyword), '#pop'),
        (r'\.', Name.Namespace),
        (uni_name, Name.Namespace),
        default('#pop'),
    ]
    # don't highlight "%s" substitutions
    tokens['strings'] = [
        (r'[^\\\'"%\n]+', String),
        # quotes, percents and backslashes must be parsed one at a time
        (r'[\'"\\]', String),
        # unhandled string formatting sign
        (r'%', String)
        # newlines are an error (use "nl" state)
    ]

    def analyse_text(text):
        return shebang_matches(text, r'pythonw?3(\.\d)?')


class PythonConsoleLexer(Lexer):
    """
    For Python console output or doctests, such as:

    .. sourcecode:: pycon

        >>> a = 'foo'
        >>> print a
        foo
        >>> 1 / 0
        Traceback (most recent call last):
          File "<stdin>", line 1, in <module>
        ZeroDivisionError: integer division or modulo by zero

    Additional options:

    `python3`
        Use Python 3 lexer for code.  Default is ``False``.

        .. versionadded:: 1.0
    """
    name = 'Python console session'
    aliases = ['pycon']
    mimetypes = ['text/x-python-doctest']

    def __init__(self, **options):
        self.python3 = get_bool_opt(options, 'python3', False)
        Lexer.__init__(self, **options)

    def get_tokens_unprocessed(self, text):
        if self.python3:
            pylexer = Python3Lexer(**self.options)
            tblexer = Python3TracebackLexer(**self.options)
        else:
            pylexer = PythonLexer(**self.options)
            tblexer = PythonTracebackLexer(**self.options)

        curcode = ''
        insertions = []
        curtb = ''
        tbindex = 0
        tb = 0
        for match in line_re.finditer(text):
            line = match.group()
            if line.startswith(u'>>> ') or line.startswith(u'... '):
                tb = 0
                insertions.append((len(curcode),
                                   [(0, Generic.Prompt, line[:4])]))
                curcode += line[4:]
            elif line.rstrip() == u'...' and not tb:
                # only a new >>> prompt can end an exception block
                # otherwise an ellipsis in place of the traceback frames
                # will be mishandled
                insertions.append((len(curcode),
                                   [(0, Generic.Prompt, u'...')]))
                curcode += line[3:]
            else:
                if curcode:
                    for item in do_insertions(
                            insertions, pylexer.get_tokens_unprocessed(curcode)):
                        yield item
                    curcode = ''
                    insertions = []
                if (line.startswith(u'Traceback (most recent call last):') or
                        re.match(u'  File "[^"]+", line \\d+\\n$', line)):
                    tb = 1
                    curtb = line
                    tbindex = match.start()
                elif line == 'KeyboardInterrupt\n':
                    yield match.start(), Name.Class, line
                elif tb:
                    curtb += line
                    if not (line.startswith(' ') or line.strip() == u'...'):
                        tb = 0
                        for i, t, v in tblexer.get_tokens_unprocessed(curtb):
                            yield tbindex+i, t, v
                else:
                    yield match.start(), Generic.Output, line
        if curcode:
            for item in do_insertions(insertions,
                                      pylexer.get_tokens_unprocessed(curcode)):
                yield item


class PythonTracebackLexer(RegexLexer):
    """
    For Python tracebacks.

    .. versionadded:: 0.7
    """

    name = 'Python Traceback'
    aliases = ['pytb']
    filenames = ['*.pytb']
    mimetypes = ['text/x-python-traceback']

    tokens = {
        'root': [
            (r'^Traceback \(most recent call last\):\n',
             Generic.Traceback, 'intb'),
            # SyntaxError starts with this.
            (r'^(?=  File "[^"]+", line \d+)', Generic.Traceback, 'intb'),
            (r'^.*\n', Other),
        ],
        'intb': [
            (r'^(  File )("[^"]+")(, line )(\d+)(, in )(.+)(\n)',
             bygroups(Text, Name.Builtin, Text, Number, Text, Name, Text)),
            (r'^(  File )("[^"]+")(, line )(\d+)(\n)',
             bygroups(Text, Name.Builtin, Text, Number, Text)),
            (r'^(    )(.+)(\n)',
             bygroups(Text, using(PythonLexer), Text)),
            (r'^([ \t]*)(\.\.\.)(\n)',
             bygroups(Text, Comment, Text)),  # for doctests...
            (r'^([^:]+)(: )(.+)(\n)',
             bygroups(Generic.Error, Text, Name, Text), '#pop'),
            (r'^([a-zA-Z_]\w*)(:?\n)',
             bygroups(Generic.Error, Text), '#pop')
        ],
    }


class Python3TracebackLexer(RegexLexer):
    """
    For Python 3.0 tracebacks, with support for chained exceptions.

    .. versionadded:: 1.0
    """

    name = 'Python 3.0 Traceback'
    aliases = ['py3tb']
    filenames = ['*.py3tb']
    mimetypes = ['text/x-python3-traceback']

    tokens = {
        'root': [
            (r'\n', Text),
            (r'^Traceback \(most recent call last\):\n', Generic.Traceback, 'intb'),
            (r'^During handling of the above exception, another '
             r'exception occurred:\n\n', Generic.Traceback),
            (r'^The above exception was the direct cause of the '
             r'following exception:\n\n', Generic.Traceback),
            (r'^(?=  File "[^"]+", line \d+)', Generic.Traceback, 'intb'),
        ],
        'intb': [
            (r'^(  File )("[^"]+")(, line )(\d+)(, in )(.+)(\n)',
             bygroups(Text, Name.Builtin, Text, Number, Text, Name, Text)),
            (r'^(  File )("[^"]+")(, line )(\d+)(\n)',
             bygroups(Text, Name.Builtin, Text, Number, Text)),
            (r'^(    )(.+)(\n)',
             bygroups(Text, using(Python3Lexer), Text)),
            (r'^([ \t]*)(\.\.\.)(\n)',
             bygroups(Text, Comment, Text)),  # for doctests...
            (r'^([^:]+)(: )(.+)(\n)',
             bygroups(Generic.Error, Text, Name, Text), '#pop'),
            (r'^([a-zA-Z_]\w*)(:?\n)',
             bygroups(Generic.Error, Text), '#pop')
        ],
    }


class CythonLexer(RegexLexer):
    """
    For Pyrex and `Cython <http://cython.org>`_ source code.

    .. versionadded:: 1.1
    """

    name = 'Cython'
    aliases = ['cython', 'pyx', 'pyrex']
    filenames = ['*.pyx', '*.pxd', '*.pxi']
    mimetypes = ['text/x-cython', 'application/x-cython']

    tokens = {
        'root': [
            (r'\n', Text),
            (r'^(\s*)("""(?:.|\n)*?""")', bygroups(Text, String.Doc)),
            (r"^(\s*)('''(?:.|\n)*?''')", bygroups(Text, String.Doc)),
            (r'[^\S\n]+', Text),
            (r'#.*$', Comment),
            (r'[]{}:(),;[]', Punctuation),
            (r'\\\n', Text),
            (r'\\', Text),
            (r'(in|is|and|or|not)\b', Operator.Word),
            (r'(<)([a-zA-Z0-9.?]+)(>)',
             bygroups(Punctuation, Keyword.Type, Punctuation)),
            (r'!=|==|<<|>>|[-~+/*%=<>&^|.?]', Operator),
            (r'(from)(\d+)(<=)(\s+)(<)(\d+)(:)',
             bygroups(Keyword, Number.Integer, Operator, Name, Operator,
                      Name, Punctuation)),
            include('keywords'),
            (r'(def|property)(\s+)', bygroups(Keyword, Text), 'funcname'),
            (r'(cp?def)(\s+)', bygroups(Keyword, Text), 'cdef'),
            (r'(class|struct)(\s+)', bygroups(Keyword, Text), 'classname'),
            (r'(from)(\s+)', bygroups(Keyword, Text), 'fromimport'),
            (r'(c?import)(\s+)', bygroups(Keyword, Text), 'import'),
            include('builtins'),
            include('backtick'),
            ('(?:[rR]|[uU][rR]|[rR][uU])"""', String, 'tdqs'),
            ("(?:[rR]|[uU][rR]|[rR][uU])'''", String, 'tsqs'),
            ('(?:[rR]|[uU][rR]|[rR][uU])"', String, 'dqs'),
            ("(?:[rR]|[uU][rR]|[rR][uU])'", String, 'sqs'),
            ('[uU]?"""', String, combined('stringescape', 'tdqs')),
            ("[uU]?'''", String, combined('stringescape', 'tsqs')),
            ('[uU]?"', String, combined('stringescape', 'dqs')),
            ("[uU]?'", String, combined('stringescape', 'sqs')),
            include('name'),
            include('numbers'),
        ],
        'keywords': [
            (words((
                'assert', 'break', 'by', 'continue', 'ctypedef', 'del', 'elif',
                'else', 'except', 'except?', 'exec', 'finally', 'for', 'gil',
                'global', 'if', 'include', 'lambda', 'nogil', 'pass', 'print',
                'raise', 'return', 'try', 'while', 'yield', 'as', 'with'), suffix=r'\b'),
             Keyword),
            (r'(DEF|IF|ELIF|ELSE)\b', Comment.Preproc),
        ],
        'builtins': [
            (words((
                '__import__', 'abs', 'all', 'any', 'apply', 'basestring', 'bin',
                'bool', 'buffer', 'bytearray', 'bytes', 'callable', 'chr',
                'classmethod', 'cmp', 'coerce', 'compile', 'complex', 'delattr',
                'dict', 'dir', 'divmod', 'enumerate', 'eval', 'execfile', 'exit',
                'file', 'filter', 'float', 'frozenset', 'getattr', 'globals',
                'hasattr', 'hash', 'hex', 'id', 'input', 'int', 'intern', 'isinstance',
                'issubclass', 'iter', 'len', 'list', 'locals', 'long', 'map', 'max',
                'min', 'next', 'object', 'oct', 'open', 'ord', 'pow', 'property',
                'range', 'raw_input', 'reduce', 'reload', 'repr', 'reversed',
                'round', 'set', 'setattr', 'slice', 'sorted', 'staticmethod',
                'str', 'sum', 'super', 'tuple', 'type', 'unichr', 'unicode',
                'vars', 'xrange', 'zip'), prefix=r'(?<!\.)', suffix=r'\b'),
             Name.Builtin),
            (r'(?<!\.)(self|None|Ellipsis|NotImplemented|False|True|NULL'
             r')\b', Name.Builtin.Pseudo),
            (words((
                'ArithmeticError', 'AssertionError', 'AttributeError',
                'BaseException', 'DeprecationWarning', 'EOFError', 'EnvironmentError',
                'Exception', 'FloatingPointError', 'FutureWarning', 'GeneratorExit', 'IOError',
                'ImportError', 'ImportWarning', 'IndentationError', 'IndexError', 'KeyError',
                'KeyboardInterrupt', 'LookupError', 'MemoryError', 'NameError',
                'NotImplemented', 'NotImplementedError', 'OSError', 'OverflowError',
                'OverflowWarning', 'PendingDeprecationWarning', 'ReferenceError',
                'RuntimeError', 'RuntimeWarning', 'StandardError', 'StopIteration',
                'SyntaxError', 'SyntaxWarning', 'SystemError', 'SystemExit', 'TabError',
                'TypeError', 'UnboundLocalError', 'UnicodeDecodeError',
                'UnicodeEncodeError', 'UnicodeError', 'UnicodeTranslateError',
                'UnicodeWarning', 'UserWarning', 'ValueError', 'Warning',
                'ZeroDivisionError'), prefix=r'(?<!\.)', suffix=r'\b'),
             Name.Exception),
        ],
        'numbers': [
            (r'(\d+\.?\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
            (r'0\d+', Number.Oct),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+', Number.Integer)
        ],
        'backtick': [
            ('`.*?`', String.Backtick),
        ],
        'name': [
            (r'@\w+', Name.Decorator),
            ('[a-zA-Z_]\w*', Name),
        ],
        'funcname': [
            ('[a-zA-Z_]\w*', Name.Function, '#pop')
        ],
        'cdef': [
            (r'(public|readonly|extern|api|inline)\b', Keyword.Reserved),
            (r'(struct|enum|union|class)\b', Keyword),
            (r'([a-zA-Z_]\w*)(\s*)(?=[(:#=]|$)',
             bygroups(Name.Function, Text), '#pop'),
            (r'([a-zA-Z_]\w*)(\s*)(,)',
             bygroups(Name.Function, Text, Punctuation)),
            (r'from\b', Keyword, '#pop'),
            (r'as\b', Keyword),
            (r':', Punctuation, '#pop'),
            (r'(?=["\'])', Text, '#pop'),
            (r'[a-zA-Z_]\w*', Keyword.Type),
            (r'.', Text),
        ],
        'classname': [
            ('[a-zA-Z_]\w*', Name.Class, '#pop')
        ],
        'import': [
            (r'(\s+)(as)(\s+)', bygroups(Text, Keyword, Text)),
            (r'[a-zA-Z_][\w.]*', Name.Namespace),
            (r'(\s*)(,)(\s*)', bygroups(Text, Operator, Text)),
            default('#pop')  # all else: go back
        ],
        'fromimport': [
            (r'(\s+)(c?import)\b', bygroups(Text, Keyword), '#pop'),
            (r'[a-zA-Z_.][\w.]*', Name.Namespace),
            # ``cdef foo from "header"``, or ``for foo from 0 < i < 10``
            default('#pop'),
        ],
        'stringescape': [
            (r'\\([\\abfnrtv"\']|\n|N{.*?}|u[a-fA-F0-9]{4}|'
             r'U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})', String.Escape)
        ],
        'strings': [
            (r'%(\([a-zA-Z0-9]+\))?[-#0 +]*([0-9]+|[*])?(\.([0-9]+|[*]))?'
             '[hlL]?[diouxXeEfFgGcrs%]', String.Interpol),
            (r'[^\\\'"%\n]+', String),
            # quotes, percents and backslashes must be parsed one at a time
            (r'[\'"\\]', String),
            # unhandled string formatting sign
            (r'%', String)
            # newlines are an error (use "nl" state)
        ],
        'nl': [
            (r'\n', String)
        ],
        'dqs': [
            (r'"', String, '#pop'),
            (r'\\\\|\\"|\\\n', String.Escape),  # included here again for raw strings
            include('strings')
        ],
        'sqs': [
            (r"'", String, '#pop'),
            (r"\\\\|\\'|\\\n", String.Escape),  # included here again for raw strings
            include('strings')
        ],
        'tdqs': [
            (r'"""', String, '#pop'),
            include('strings'),
            include('nl')
        ],
        'tsqs': [
            (r"'''", String, '#pop'),
            include('strings'),
            include('nl')
        ],
    }


class HyLexer(RegexLexer):
    """
    Lexer for `Hy <http://hylang.org/>`_ source code.

    .. versionadded:: 2.0
    """
    name = 'Hy'
    aliases = ['hylang']
    filenames = ['*.hy']
    mimetypes = ['text/x-hy', 'application/x-hy']

    special_forms = [
        'cond', 'for', '->', '->>', 'car',
        'cdr', 'first', 'rest', 'let', 'when', 'unless',
        'import', 'do', 'progn', 'get', 'slice', 'assoc', 'with-decorator',
        ',', 'list_comp', 'kwapply', '~', 'is', 'in', 'is-not', 'not-in',
        'quasiquote', 'unquote', 'unquote-splice', 'quote', '|', '<<=', '>>=',
        'foreach', 'while',
        'eval-and-compile', 'eval-when-compile'
    ]

    declarations = [
        'def', 'defn', 'defun', 'defmacro', 'defclass', 'lambda', 'fn', 'setv'
    ]

    hy_builtins = []

    hy_core = [
        'cycle', 'dec', 'distinct', 'drop', 'even?', 'filter', 'inc',
        'instance?', 'iterable?', 'iterate', 'iterator?', 'neg?',
        'none?', 'nth', 'numeric?', 'odd?', 'pos?', 'remove', 'repeat',
        'repeatedly', 'take', 'take_nth', 'take_while', 'zero?'
    ]

    builtins = hy_builtins + hy_core

    # valid names for identifiers
    # well, names can only not consist fully of numbers
    # but this should be good enough for now
    valid_name = r'(?!#)[\w!$%*+<=>?/.#-]+'

    def _multi_escape(entries):
        return '(%s)' % ('|'.join(re.escape(entry) + ' ' for entry in entries))

    tokens = {
        'root': [
            # the comments - always starting with semicolon
            # and going to the end of the line
            (r';.*$', Comment.Single),

            # whitespaces - usually not relevant
            (r'[,\s]+', Text),

            # numbers
            (r'-?\d+\.\d+', Number.Float),
            (r'-?\d+', Number.Integer),
            (r'0[0-7]+j?', Number.Oct),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),

            # strings, symbols and characters
            (r'"(\\\\|\\"|[^"])*"', String),
            (r"'" + valid_name, String.Symbol),
            (r"\\(.|[a-z]+)", String.Char),
            (r'^(\s*)([rRuU]{,2}"""(?:.|\n)*?""")', bygroups(Text, String.Doc)),
            (r"^(\s*)([rRuU]{,2}'''(?:.|\n)*?''')", bygroups(Text, String.Doc)),

            # keywords
            (r'::?' + valid_name, String.Symbol),

            # special operators
            (r'~@|[`\'#^~&@]', Operator),

            include('py-keywords'),
            include('py-builtins'),

            # highlight the special forms
            (_multi_escape(special_forms), Keyword),

            # Technically, only the special forms are 'keywords'. The problem
            # is that only treating them as keywords means that things like
            # 'defn' and 'ns' need to be highlighted as builtins. This is ugly
            # and weird for most styles. So, as a compromise we're going to
            # highlight them as Keyword.Declarations.
            (_multi_escape(declarations), Keyword.Declaration),

            # highlight the builtins
            (_multi_escape(builtins), Name.Builtin),

            # the remaining functions
            (r'(?<=\()' + valid_name, Name.Function),

            # find the remaining variables
            (valid_name, Name.Variable),

            # Hy accepts vector notation
            (r'(\[|\])', Punctuation),

            # Hy accepts map notation
            (r'(\{|\})', Punctuation),

            # the famous parentheses!
            (r'(\(|\))', Punctuation),

        ],
        'py-keywords': PythonLexer.tokens['keywords'],
        'py-builtins': PythonLexer.tokens['builtins'],
    }

    def analyse_text(text):
        if '(import ' in text or '(defn ' in text:
            return 0.9


class DgLexer(RegexLexer):
    """
    Lexer for `dg <http://pyos.github.com/dg>`_,
    a functional and object-oriented programming language
    running on the CPython 3 VM.

    .. versionadded:: 1.6
    """
    name = 'dg'
    aliases = ['dg']
    filenames = ['*.dg']
    mimetypes = ['text/x-dg']

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'#.*?$', Comment.Single),

            (r'(?i)0b[01]+', Number.Bin),
            (r'(?i)0o[0-7]+', Number.Oct),
            (r'(?i)0x[0-9a-f]+', Number.Hex),
            (r'(?i)[+-]?[0-9]+\.[0-9]+(e[+-]?[0-9]+)?j?', Number.Float),
            (r'(?i)[+-]?[0-9]+e[+-]?\d+j?', Number.Float),
            (r'(?i)[+-]?[0-9]+j?', Number.Integer),

            (r"(?i)(br|r?b?)'''", String, combined('stringescape', 'tsqs', 'string')),
            (r'(?i)(br|r?b?)"""', String, combined('stringescape', 'tdqs', 'string')),
            (r"(?i)(br|r?b?)'", String, combined('stringescape', 'sqs', 'string')),
            (r'(?i)(br|r?b?)"', String, combined('stringescape', 'dqs', 'string')),

            (r"`\w+'*`", Operator),
            (r'\b(and|in|is|or|where)\b', Operator.Word),
            (r'[!$%&*+\-./:<-@\\^|~;,]+', Operator),

            (r"(?<!\.)(bool|bytearray|bytes|classmethod|complex|dict'?|"
             r"float|frozenset|int|list'?|memoryview|object|property|range|"
             r"set'?|slice|staticmethod|str|super|tuple'?|type)"
             r"(?!['\w])", Name.Builtin),
            (r'(?<!\.)(__import__|abs|all|any|bin|bind|chr|cmp|compile|complex|'
             r'delattr|dir|divmod|drop|dropwhile|enumerate|eval|exhaust|'
             r'filter|flip|foldl1?|format|fst|getattr|globals|hasattr|hash|'
             r'head|hex|id|init|input|isinstance|issubclass|iter|iterate|last|'
             r'len|locals|map|max|min|next|oct|open|ord|pow|print|repr|'
             r'reversed|round|setattr|scanl1?|snd|sorted|sum|tail|take|'
             r"takewhile|vars|zip)(?!['\w])", Name.Builtin),
            (r"(?<!\.)(self|Ellipsis|NotImplemented|None|True|False)(?!['\w])",
             Name.Builtin.Pseudo),

            (r"(?<!\.)[A-Z]\w*(Error|Exception|Warning)'*(?!['\w])",
             Name.Exception),
            (r"(?<!\.)(Exception|GeneratorExit|KeyboardInterrupt|StopIteration|"
             r"SystemExit)(?!['\w])", Name.Exception),

            (r"(?<![\.\w])(except|finally|for|if|import|not|otherwise|raise|"
             r"subclass|while|with|yield)(?!['\w])", Keyword.Reserved),

            (r"[A-Z_]+'*(?!['\w])", Name),
            (r"[A-Z]\w+'*(?!['\w])", Keyword.Type),
            (r"\w+'*", Name),

            (r'[()]', Punctuation),
            (r'.', Error),
        ],
        'stringescape': [
            (r'\\([\\abfnrtv"\']|\n|N{.*?}|u[a-fA-F0-9]{4}|'
             r'U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})', String.Escape)
        ],
        'string': [
            (r'%(\(\w+\))?[-#0 +]*([0-9]+|[*])?(\.([0-9]+|[*]))?'
             '[hlL]?[diouxXeEfFgGcrs%]', String.Interpol),
            (r'[^\\\'"%\n]+', String),
            # quotes, percents and backslashes must be parsed one at a time
            (r'[\'"\\]', String),
            # unhandled string formatting sign
            (r'%', String),
            (r'\n', String)
        ],
        'dqs': [
            (r'"', String, '#pop')
        ],
        'sqs': [
            (r"'", String, '#pop')
        ],
        'tdqs': [
            (r'"""', String, '#pop')
        ],
        'tsqs': [
            (r"'''", String, '#pop')
        ],
    }
