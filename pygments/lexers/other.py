# -*- coding: utf-8 -*-
"""
    pygments.lexers.other
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for other languages.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
from pygments.lexer import ExtendedRegexLexer, RegexLexer, include, bygroups, \
    default, using, this, combined, words
from pygments.token import Generic, Comment, String, Text, Number, Keyword, Name, \
    Error, Operator, Punctuation, Literal, Whitespace

# backwards compatibility
from pygments.lexers.sql import SqlLexer, MySqlLexer, SqliteConsoleLexer
from pygments.lexers.shell import BashLexer, BashSessionLexer, BatchLexer, \
    TcshLexer
from pygments.lexers.robotframework import RobotFrameworkLexer
from pygments.lexers.testing import GherkinLexer
from pygments.lexers.esoteric import BrainfuckLexer, BefungeLexer, RedcodeLexer
from pygments.lexers.prolog import LogtalkLexer
from pygments.lexers.misc.snobol import SnobolLexer
from pygments.lexers.misc.rebol import RebolLexer
from pygments.lexers.configs import KconfigLexer
from pygments.lexers.modeling import ModelicaLexer
from pygments.lexers.scripting import AppleScriptLexer, MOOCodeLexer
from pygments.lexers.graphics import PostScriptLexer, GnuplotLexer, \
    AsymptoteLexer, PovrayLexer
from pygments.lexers.business import ABAPLexer, OpenEdgeLexer, \
    GoodDataCLLexer, MaqlLexer
from pygments.lexers.automation import AutoItLexer, AutohotkeyLexer
from pygments.lexers.dsls import ProtoBufLexer, BroLexer, PuppetLexer, \
    MscgenLexer, VGLLexer
from pygments.lexers.misc.basic import CbmBasicV2Lexer
from pygments.lexers.misc.pawn import SourcePawnLexer, PawnLexer
from pygments.lexers.installers import NSISLexer, RPMSpecLexer
from pygments.lexers.misc.smalltalk import SmalltalkLexer, NewspeakLexer

__all__ = ['HybrisLexer', 'AwkLexer', 'Cfengine3Lexer', 'ECLLexer',
           'UrbiscriptLexer', 'AmbientTalkLexer', 'PanLexer']


class ECLLexer(RegexLexer):
    """
    Lexer for the declarative big-data `ECL
    <http://hpccsystems.com/community/docs/ecl-language-reference/html>`_
    language.

    .. versionadded:: 1.5
    """

    name = 'ECL'
    aliases = ['ecl']
    filenames = ['*.ecl']
    mimetypes = ['application/x-ecl']

    flags = re.IGNORECASE | re.MULTILINE

    tokens = {
        'root': [
            include('whitespace'),
            include('statements'),
        ],
        'whitespace': [
            (r'\s+', Text),
            (r'\/\/.*', Comment.Single),
            (r'/(\\\n)?\*(.|\n)*?\*(\\\n)?/', Comment.Multiline),
        ],
        'statements': [
            include('types'),
            include('keywords'),
            include('functions'),
            include('hash'),
            (r'"', String, 'string'),
            (r'\'', String, 'string'),
            (r'(\d+\.\d*|\.\d+|\d+)e[+-]?\d+[lu]*', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-f]+[lu]*', Number.Hex),
            (r'0[0-7]+[lu]*', Number.Oct),
            (r'\d+[LlUu]*', Number.Integer),
            (r'\*/', Error),
            (r'[~!%^&*+=|?:<>/-]+', Operator),
            (r'[{}()\[\],.;]', Punctuation),
            (r'[a-z_]\w*', Name),
        ],
        'hash': [
            (r'^#.*$', Comment.Preproc),
        ],
        'types': [
            (r'(RECORD|END)\D', Keyword.Declaration),
            (r'((?:ASCII|BIG_ENDIAN|BOOLEAN|DATA|DECIMAL|EBCDIC|INTEGER|PATTERN|'
             r'QSTRING|REAL|RECORD|RULE|SET OF|STRING|TOKEN|UDECIMAL|UNICODE|'
             r'UNSIGNED|VARSTRING|VARUNICODE)\d*)(\s+)',
             bygroups(Keyword.Type, Text)),
        ],
        'keywords': [
            (words((
                'APPLY', 'ASSERT', 'BUILD', 'BUILDINDEX', 'EVALUATE', 'FAIL',
                'KEYDIFF', 'KEYPATCH', 'LOADXML', 'NOTHOR', 'NOTIFY', 'OUTPUT',
                'PARALLEL', 'SEQUENTIAL', 'SOAPCALL', 'CHECKPOINT', 'DEPRECATED',
                'FAILCODE', 'FAILMESSAGE', 'FAILURE', 'GLOBAL', 'INDEPENDENT',
                'ONWARNING', 'PERSIST', 'PRIORITY', 'RECOVERY', 'STORED', 'SUCCESS',
                'WAIT', 'WHEN'), suffix=r'\b'),
             Keyword.Reserved),
            # These are classed differently, check later
            (words((
                'ALL', 'AND', 'ANY', 'AS', 'ATMOST', 'BEFORE', 'BEGINC++', 'BEST', 'BETWEEN', 'CASE', 'CONST',
                'COUNTER', 'CSV', 'DESCEND', 'ENCRYPT', 'ENDC++', 'ENDMACRO', 'EXCEPT', 'EXCLUSIVE',
                'EXPIRE', 'EXPORT', 'EXTEND', 'FALSE', 'FEW', 'FIRST', 'FLAT', 'FULL', 'FUNCTION', 'GROUP',
                'HEADER', 'HEADING', 'HOLE', 'IFBLOCK', 'IMPORT', 'IN', 'JOINED', 'KEEP', 'KEYED', 'LAST',
                'LEFT', 'LIMIT', 'LOAD', 'LOCAL', 'LOCALE', 'LOOKUP', 'MACRO', 'MANY', 'MAXCOUNT',
                'MAXLENGTH', 'MIN SKEW', 'MODULE', 'INTERFACE', 'NAMED', 'NOCASE', 'NOROOT', 'NOSCAN',
                'NOSORT', 'NOT', 'OF', 'ONLY', 'OPT', 'OR', 'OUTER', 'OVERWRITE', 'PACKED', 'PARTITION',
                'PENALTY', 'PHYSICALLENGTH', 'PIPE', 'QUOTE', 'RELATIONSHIP', 'REPEAT', 'RETURN',
                'RIGHT', 'SCAN', 'SELF', 'SEPARATOR', 'SERVICE', 'SHARED', 'SKEW', 'SKIP', 'SQL', 'STORE',
                'TERMINATOR', 'THOR', 'THRESHOLD', 'TOKEN', 'TRANSFORM', 'TRIM', 'TRUE', 'TYPE',
                'UNICODEORDER', 'UNSORTED', 'VALIDATE', 'VIRTUAL', 'WHOLE', 'WILD', 'WITHIN', 'XML',
                'XPATH', '__COMPRESSED__'), suffix=r'\b'),
             Keyword.Reserved),
        ],
        'functions': [
            (words((
                'ABS', 'ACOS', 'ALLNODES', 'ASCII', 'ASIN', 'ASSTRING', 'ATAN', 'ATAN2', 'AVE', 'CASE',
                'CHOOSE', 'CHOOSEN', 'CHOOSESETS', 'CLUSTERSIZE', 'COMBINE', 'CORRELATION', 'COS',
                'COSH', 'COUNT', 'COVARIANCE', 'CRON', 'DATASET', 'DEDUP', 'DEFINE', 'DENORMALIZE',
                'DISTRIBUTE', 'DISTRIBUTED', 'DISTRIBUTION', 'EBCDIC', 'ENTH', 'ERROR', 'EVALUATE',
                'EVENT', 'EVENTEXTRA', 'EVENTNAME', 'EXISTS', 'EXP', 'FAILCODE', 'FAILMESSAGE',
                'FETCH', 'FROMUNICODE', 'GETISVALID', 'GLOBAL', 'GRAPH', 'GROUP', 'HASH', 'HASH32',
                'HASH64', 'HASHCRC', 'HASHMD5', 'HAVING', 'IF', 'INDEX', 'INTFORMAT', 'ISVALID',
                'ITERATE', 'JOIN', 'KEYUNICODE', 'LENGTH', 'LIBRARY', 'LIMIT', 'LN', 'LOCAL', 'LOG', 'LOOP',
                'MAP', 'MATCHED', 'MATCHLENGTH', 'MATCHPOSITION', 'MATCHTEXT', 'MATCHUNICODE',
                'MAX', 'MERGE', 'MERGEJOIN', 'MIN', 'NOLOCAL', 'NONEMPTY', 'NORMALIZE', 'PARSE', 'PIPE',
                'POWER', 'PRELOAD', 'PROCESS', 'PROJECT', 'PULL', 'RANDOM', 'RANGE', 'RANK', 'RANKED',
                'REALFORMAT', 'RECORDOF', 'REGEXFIND', 'REGEXREPLACE', 'REGROUP', 'REJECTED',
                'ROLLUP', 'ROUND', 'ROUNDUP', 'ROW', 'ROWDIFF', 'SAMPLE', 'SET', 'SIN', 'SINH', 'SIZEOF',
                'SOAPCALL', 'SORT', 'SORTED', 'SQRT', 'STEPPED', 'STORED', 'SUM', 'TABLE', 'TAN', 'TANH',
                'THISNODE', 'TOPN', 'TOUNICODE', 'TRANSFER', 'TRIM', 'TRUNCATE', 'TYPEOF', 'UNGROUP',
                'UNICODEORDER', 'VARIANCE', 'WHICH', 'WORKUNIT', 'XMLDECODE', 'XMLENCODE',
                'XMLTEXT', 'XMLUNICODE'), suffix=r'\b'),
             Name.Function),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\'', String, '#pop'),
            (r'[^"\']+', String),
        ],
    }


class HybrisLexer(RegexLexer):
    """
    For `Hybris <http://www.hybris-lang.org>`_ source code.

    .. versionadded:: 1.4
    """

    name = 'Hybris'
    aliases = ['hybris', 'hy']
    filenames = ['*.hy', '*.hyb']
    mimetypes = ['text/x-hybris', 'application/x-hybris']

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'root': [
            # method names
            (r'^(\s*(?:function|method|operator\s+)+?)'
             r'([a-zA-Z_]\w*)'
             r'(\s*)(\()', bygroups(Keyword, Name.Function, Text, Operator)),
            (r'[^\S\n]+', Text),
            (r'//.*?\n', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline),
            (r'@[a-zA-Z_][\w\.]*', Name.Decorator),
            (r'(break|case|catch|next|default|do|else|finally|for|foreach|of|'
             r'unless|if|new|return|switch|me|throw|try|while)\b', Keyword),
            (r'(extends|private|protected|public|static|throws|function|method|'
             r'operator)\b', Keyword.Declaration),
            (r'(true|false|null|__FILE__|__LINE__|__VERSION__|__LIB_PATH__|'
             r'__INC_PATH__)\b', Keyword.Constant),
            (r'(class|struct)(\s+)',
             bygroups(Keyword.Declaration, Text), 'class'),
            (r'(import|include)(\s+)',
             bygroups(Keyword.Namespace, Text), 'import'),
            (words((
                'gc_collect', 'gc_mm_items', 'gc_mm_usage', 'gc_collect_threshold',
                'urlencode', 'urldecode', 'base64encode', 'base64decode', 'sha1', 'crc32', 'sha2',
                'md5', 'md5_file', 'acos', 'asin', 'atan', 'atan2', 'ceil', 'cos', 'cosh', 'exp', 'fabs', 'floor',
                'fmod', 'log', 'log10', 'pow', 'sin', 'sinh', 'sqrt', 'tan', 'tanh', 'isint', 'isfloat', 'ischar',
                'isstring', 'isarray', 'ismap', 'isalias', 'typeof', 'sizeof', 'toint', 'tostring',
                'fromxml', 'toxml', 'binary', 'pack', 'load', 'eval', 'var_names', 'var_values',
                'user_functions', 'dyn_functions', 'methods', 'call', 'call_method', 'mknod',
                'mkfifo', 'mount', 'umount2', 'umount', 'ticks', 'usleep', 'sleep', 'time', 'strtime',
                'strdate', 'dllopen', 'dlllink', 'dllcall', 'dllcall_argv', 'dllclose', 'env', 'exec',
                'fork', 'getpid', 'wait', 'popen', 'pclose', 'exit', 'kill', 'pthread_create',
                'pthread_create_argv', 'pthread_exit', 'pthread_join', 'pthread_kill',
                'smtp_send', 'http_get', 'http_post', 'http_download', 'socket', 'bind', 'listen',
                'accept', 'getsockname', 'getpeername', 'settimeout', 'connect', 'server', 'recv',
                'send', 'close', 'print', 'println', 'printf', 'input', 'readline', 'serial_open',
                'serial_fcntl', 'serial_get_attr', 'serial_get_ispeed', 'serial_get_ospeed',
                'serial_set_attr', 'serial_set_ispeed', 'serial_set_ospeed', 'serial_write',
                'serial_read', 'serial_close', 'xml_load', 'xml_parse', 'fopen', 'fseek', 'ftell',
                'fsize', 'fread', 'fwrite', 'fgets', 'fclose', 'file', 'readdir', 'pcre_replace', 'size',
                'pop', 'unmap', 'has', 'keys', 'values', 'length', 'find', 'substr', 'replace', 'split', 'trim',
                'remove', 'contains', 'join'), suffix=r'\b'),
             Name.Builtin),
            (words((
                'MethodReference', 'Runner', 'Dll', 'Thread', 'Pipe', 'Process',
                'Runnable', 'CGI', 'ClientSocket', 'Socket', 'ServerSocket',
                'File', 'Console', 'Directory', 'Exception'), suffix=r'\b'),
             Keyword.Type),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r"'\\.'|'[^\\]'|'\\u[0-9a-f]{4}'", String.Char),
            (r'(\.)([a-zA-Z_]\w*)',
             bygroups(Operator, Name.Attribute)),
            (r'[a-zA-Z_]\w*:', Name.Label),
            (r'[a-zA-Z_\$]\w*', Name),
            (r'[~\^\*!%&\[\]\(\)\{\}<>\|+=:;,./?\-@]+', Operator),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'0x[0-9a-f]+', Number.Hex),
            (r'[0-9]+L?', Number.Integer),
            (r'\n', Text),
        ],
        'class': [
            (r'[a-zA-Z_]\w*', Name.Class, '#pop')
        ],
        'import': [
            (r'[\w.]+\*?', Name.Namespace, '#pop')
        ],
    }


class AwkLexer(RegexLexer):
    """
    For Awk scripts.

    .. versionadded:: 1.5
    """

    name = 'Awk'
    aliases = ['awk', 'gawk', 'mawk', 'nawk']
    filenames = ['*.awk']
    mimetypes = ['application/x-awk']

    tokens = {
        'commentsandwhitespace': [
            (r'\s+', Text),
            (r'#.*$', Comment.Single)
        ],
        'slashstartsregex': [
            include('commentsandwhitespace'),
            (r'/(\\.|[^[/\\\n]|\[(\\.|[^\]\\\n])*])+/'
             r'\B', String.Regex, '#pop'),
            (r'(?=/)', Text, ('#pop', 'badregex')),
            default('#pop')
        ],
        'badregex': [
            (r'\n', Text, '#pop')
        ],
        'root': [
            (r'^(?=\s|/)', Text, 'slashstartsregex'),
            include('commentsandwhitespace'),
            (r'\+\+|--|\|\||&&|in\b|\$|!?~|'
             r'(\*\*|[-<>+*%\^/!=])=?', Operator, 'slashstartsregex'),
            (r'[{(\[;,]', Punctuation, 'slashstartsregex'),
            (r'[})\].]', Punctuation),
            (r'(break|continue|do|while|exit|for|if|else|'
             r'return)\b', Keyword, 'slashstartsregex'),
            (r'function\b', Keyword.Declaration, 'slashstartsregex'),
            (r'(atan2|cos|exp|int|log|rand|sin|sqrt|srand|gensub|gsub|index|'
             r'length|match|split|sprintf|sub|substr|tolower|toupper|close|'
             r'fflush|getline|next|nextfile|print|printf|strftime|systime|'
             r'delete|system)\b', Keyword.Reserved),
            (r'(ARGC|ARGIND|ARGV|CONVFMT|ENVIRON|ERRNO|FIELDWIDTHS|FILENAME|FNR|FS|'
             r'IGNORECASE|NF|NR|OFMT|OFS|ORFS|RLENGTH|RS|RSTART|RT|'
             r'SUBSEP)\b', Name.Builtin),
            (r'[$a-zA-Z_]\w*', Name.Other),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+', Number.Integer),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r"'(\\\\|\\'|[^'])*'", String.Single),
        ]
    }


class Cfengine3Lexer(RegexLexer):
    """
    Lexer for `CFEngine3 <http://cfengine.org>`_ policy files.

    .. versionadded:: 1.5
    """

    name = 'CFEngine3'
    aliases = ['cfengine3', 'cf3']
    filenames = ['*.cf']
    mimetypes = []

    tokens = {
        'root': [
            (r'#.*?\n', Comment),
            (r'(body)(\s+)(\S+)(\s+)(control)',
             bygroups(Keyword, Text, Keyword, Text, Keyword)),
            (r'(body|bundle)(\s+)(\S+)(\s+)(\w+)(\()',
             bygroups(Keyword, Text, Keyword, Text, Name.Function, Punctuation),
             'arglist'),
            (r'(body|bundle)(\s+)(\S+)(\s+)(\w+)',
             bygroups(Keyword, Text, Keyword, Text, Name.Function)),
            (r'(")([^"]+)(")(\s+)(string|slist|int|real)(\s*)(=>)(\s*)',
             bygroups(Punctuation,Name.Variable,Punctuation,
                      Text,Keyword.Type,Text,Operator,Text)),
            (r'(\S+)(\s*)(=>)(\s*)',
             bygroups(Keyword.Reserved,Text,Operator,Text)),
            (r'"', String, 'string'),
            (r'(\w+)(\()', bygroups(Name.Function, Punctuation)),
            (r'([\w.!&|\(\)]+)(::)', bygroups(Name.Class, Punctuation)),
            (r'(\w+)(:)', bygroups(Keyword.Declaration,Punctuation)),
            (r'@[\{\(][^\)\}]+[\}\)]', Name.Variable),
            (r'[(){},;]', Punctuation),
            (r'=>', Operator),
            (r'->', Operator),
            (r'\d+\.\d+', Number.Float),
            (r'\d+', Number.Integer),
            (r'\w+', Name.Function),
            (r'\s+', Text),
        ],
        'string': [
            (r'\$[\{\(]', String.Interpol, 'interpol'),
            (r'\\.', String.Escape),
            (r'"', String, '#pop'),
            (r'\n', String),
            (r'.', String),
        ],
        'interpol': [
            (r'\$[\{\(]', String.Interpol, '#push'),
            (r'[\}\)]', String.Interpol, '#pop'),
            (r'[^\$\{\(\)\}]+', String.Interpol),
        ],
        'arglist': [
            (r'\)', Punctuation, '#pop'),
            (r',', Punctuation),
            (r'\w+', Name.Variable),
            (r'\s+', Text),
        ],
    }


class UrbiscriptLexer(ExtendedRegexLexer):
    """
    For UrbiScript source code.

    .. versionadded:: 1.5
    """

    name = 'UrbiScript'
    aliases = ['urbiscript']
    filenames = ['*.u']
    mimetypes = ['application/x-urbiscript']

    flags = re.DOTALL

    ## TODO
    # - handle Experimental and deprecated tags with specific tokens
    # - handle Angles and Durations with specific tokens

    def blob_callback(lexer, match, ctx):
        text_before_blob = match.group(1)
        blob_start = match.group(2)
        blob_size_str = match.group(3)
        blob_size = int(blob_size_str)
        yield match.start(), String, text_before_blob
        ctx.pos += len(text_before_blob)

        # if blob size doesn't match blob format (example : "\B(2)(aaa)")
        # yield blob as a string
        if ctx.text[match.end() + blob_size] != ")":
            result = "\\B(" + blob_size_str + ")("
            yield match.start(), String, result
            ctx.pos += len(result)
            return

        # if blob is well formated, yield as Escape
        blob_text = blob_start + ctx.text[match.end():match.end()+blob_size] + ")"
        yield match.start(), String.Escape, blob_text
        ctx.pos = match.end() + blob_size + 1 # +1 is the ending ")"

    tokens = {
        'root': [
            (r'\s+', Text),
            # comments
            (r'//.*?\n', Comment),
            (r'/\*', Comment.Multiline, 'comment'),
            (r'(?:every|for|loop|while)(?:;|&|\||,)',Keyword),
            (r'(?:assert|at|break|case|catch|closure|compl|continue|'
             r'default|else|enum|every|external|finally|for|freezeif|if|new|'
             r'onleave|return|stopif|switch|this|throw|timeout|try|'
             r'waituntil|whenever|while)\b', Keyword),
            (r'(?:asm|auto|bool|char|const_cast|delete|double|dynamic_cast|'
             r'explicit|export|extern|float|friend|goto|inline|int|'
             r'long|mutable|namespace|register|reinterpret_cast|short|'
             r'signed|sizeof|static_cast|struct|template|typedef|typeid|'
             r'typename|union|unsigned|using|virtual|volatile|'
             r'wchar_t)\b', Keyword.Reserved),
            # deprecated keywords, use a meaningfull token when available
            (r'(?:emit|foreach|internal|loopn|static)\b', Keyword),
            # ignored keywords, use a meaningfull token when available
            (r'(?:private|protected|public)\b', Keyword),
            (r'(?:var|do|const|function|class)\b', Keyword.Declaration),
            (r'(?:true|false|nil|void)\b', Keyword.Constant),
            (r'(?:Barrier|Binary|Boolean|CallMessage|Channel|Code|'
             r'Comparable|Container|Control|Date|Dictionary|Directory|'
             r'Duration|Enumeration|Event|Exception|Executable|File|Finalizable|'
             r'Float|FormatInfo|Formatter|Global|Group|Hash|InputStream|'
             r'IoService|Job|Kernel|Lazy|List|Loadable|Lobby|Location|Logger|Math|'
             r'Mutex|nil|Object|Orderable|OutputStream|Pair|Path|Pattern|Position|'
             r'Primitive|Process|Profile|PseudoLazy|PubSub|RangeIterable|Regexp|'
             r'Semaphore|Server|Singleton|Socket|StackFrame|Stream|String|System|'
             r'Tag|Timeout|Traceable|TrajectoryGenerator|Triplet|Tuple'
             r'|UObject|UValue|UVar)\b', Name.Builtin),
            (r'(?:this)\b', Name.Builtin.Pseudo),
            # don't match single | and &
            (r'(?:[-=+*%/<>~^:]+|\.&?|\|\||&&)', Operator),
            (r'(?:and_eq|and|bitand|bitor|in|not|not_eq|or_eq|or|xor_eq|xor)\b',
             Operator.Word),
            (r'[{}\[\]()]+', Punctuation),
            (r'(?:;|\||,|&|\?|!)+', Punctuation),
            (r'[$a-zA-Z_]\w*', Name.Other),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            # Float, Integer, Angle and Duration
            (r'(?:[0-9]+(?:(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)?'
             r'((?:rad|deg|grad)|(?:ms|s|min|h|d))?)\b', Number.Float),
            # handle binary blob in strings
            (r'"', String.Double, "string.double"),
            (r"'", String.Single, "string.single"),
        ],
        'string.double': [
            (r'((?:\\\\|\\"|[^"])*?)(\\B\((\d+)\)\()', blob_callback),
            (r'(\\\\|\\"|[^"])*?"', String.Double, '#pop'),
        ],
        'string.single': [
            (r"((?:\\\\|\\'|[^'])*?)(\\B\((\d+)\)\()", blob_callback),
            (r"(\\\\|\\'|[^'])*?'", String.Single, '#pop'),
        ],
        # from http://pygments.org/docs/lexerdevelopment/#changing-states
        'comment': [
            (r'[^*/]', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ]
    }


class AmbientTalkLexer(RegexLexer):
    """
    Lexer for `AmbientTalk <https://code.google.com/p/ambienttalk>`_ source code.

    .. versionadded:: 2.0
    """
    name = 'AmbientTalk'
    filenames = ['*.at']
    aliases = ['at', 'ambienttalk', 'ambienttalk/2']
    mimetypes = ['text/x-ambienttalk']

    flags = re.MULTILINE | re.DOTALL

    builtin = words(('if:', 'then:', 'else:', 'when:', 'whenever:', 'discovered:',
                     'disconnected:', 'reconnected:', 'takenOffline:', 'becomes:',
                     'export:', 'as:', 'object:', 'actor:', 'mirror:', 'taggedAs:',
                     'mirroredBy:', 'is:'))
    tokens = {
        'root' : [
            (r'\s+', Text),
            (r'//.*?\n', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline),
            (r'(def|deftype|import|alias|exclude)\b', Keyword),
            (builtin, Name.Builtin),
            (r'(true|false|nil)\b', Keyword.Constant),
            (r'(~|lobby|jlobby|/)\.', Keyword.Constant, 'namespace'),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r'\|', Punctuation, 'arglist'),
            (r'<:|[\^\*!%&<>+=,./?-]|:=', Operator),
            (r"`[a-zA-Z_]\w*", String.Symbol),
            (r"[a-zA-Z_]\w*:", Name.Function),
            (r"[\{\}()\[\];`]", Punctuation),
            (r'(self|super)\b', Name.Variable.Instance),
            (r"[a-zA-Z_]\w*", Name.Variable),
            (r"@[a-zA-Z_]\w*", Name.Class),
            (r"@\[", Name.Class, 'annotations'),
            include('numbers'),
        ],
        'numbers' : [
            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
            (r'\d+', Number.Integer)
        ],
        'namespace': [
            (r'[a-zA-Z_]\w*\.', Name.Namespace),
            (r'[a-zA-Z_]\w*:', Name.Function , '#pop'),
            (r'[a-zA-Z_]\w*(?!\.)', Name.Function , '#pop')
        ],
        'annotations' : [
            (r"(.*?)\]", Name.Class, '#pop')
        ],
        'arglist' : [
            (r'\|', Punctuation, '#pop'),
            (r'\s*(,)\s*', Punctuation),
            (r'[a-zA-Z_]\w*', Name.Variable),
        ],
    }


class PanLexer(RegexLexer):
    """
    Lexer for `pan <http://github.com/quattor/pan/>`_ source files.

    Based on tcsh lexer.

    .. versionadded:: 2.0
    """

    name = 'Pan'
    aliases = ['pan']
    filenames = ['*.pan']

    tokens = {
        'root': [
            include('basic'),
            (r'\(', Keyword, 'paren'),
            (r'{', Keyword, 'curly'),
            include('data'),
        ],
        'basic': [
            (r'\b(if|for|with|else|type|bind|while|valid|final|prefix|unique|'
             r'object|foreach|include|template|function|variable|structure|'
             r'extensible|declaration)\s*\b',
             Keyword),
            (r'\b(file_contents|format|index|length|match|matches|replace|'
             r'splice|split|substr|to_lowercase|to_uppercase|debug|error|'
             r'traceback|deprecated|base64_decode|base64_encode|digest|escape|'
             r'unescape|append|create|first|nlist|key|length|list|merge|next|'
             r'prepend|splice|is_boolean|is_defined|is_double|is_list|is_long|'
             r'is_nlist|is_null|is_number|is_property|is_resource|is_string|'
             r'to_boolean|to_double|to_long|to_string|clone|delete|exists|'
             r'path_exists|if_exists|return|value)\s*\b',
             Name.Builtin),
            (r'#.*', Comment),
            (r'\\[\w\W]', String.Escape),
            (r'(\b\w+)(\s*)(=)', bygroups(Name.Variable, Text, Operator)),
            (r'[\[\]{}()=]+', Operator),
            (r'<<\s*(\'?)\\?(\w+)[\w\W]+?\2', String),
            (r';', Punctuation),
        ],
        'data': [
            (r'(?s)"(\\\\|\\[0-7]+|\\.|[^"\\])*"', String.Double),
            (r"(?s)'(\\\\|\\[0-7]+|\\.|[^'\\])*'", String.Single),
            (r'\s+', Text),
            (r'[^=\s\[\]{}()$"\'`\\;#]+', Text),
            (r'\d+(?= |\Z)', Number),
        ],
        'curly': [
            (r'}', Keyword, '#pop'),
            (r':-', Keyword),
            (r'\w+', Name.Variable),
            (r'[^}:"\'`$]+', Punctuation),
            (r':', Punctuation),
            include('root'),
        ],
        'paren': [
            (r'\)', Keyword, '#pop'),
            include('root'),
        ],
    }
