"""
    pygments.lexers.yacc
    ~~~~~~~~~~~~~~~~~~~~

    Lexers for Yacc grammars.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, DelegatingLexer, include, bygroups, \
    words, default, inherit
from pygments.token import Keyword, Punctuation, Comment, Other, Name, \
    Whitespace, Operator, Text
from pygments.lexers.c_cpp import CLexer, CppLexer, CFamilyLexer
from pygments.lexers.d import DLexer
from pygments.lexers.jvm import JavaLexer

__all__ = ['YaccCLexer', 'YaccCppLexer', 'YaccDLexer', 'YaccJavaLexer']

class YaccBase(RegexLexer):
    """
    Base for a Yacc lexer; needs language-specific tokens['ctypes'] specified.

    This is like an abstract base, and should not be used directly.  A language
    `foo' gets a YaccForFoo derived from YaccBase which highlights Yacc syntax
    and delegates the embedded language to Other (while not stopping on a '}'
    that appears in foo's strings or comments), a FooDelegate derived from
    FooLexer that also highlights Yacc-relevant keywords (if necessary), and a
    YaccFooLexer derived from DelegatingLexer which links the two and is exposed.
    """

    yaccName = r'[A-Za-z_][-\w.]*'
    sComment = r'//.*'
    mComment = r'(?s)/\*.*?\*/'

    tokens = {
        # 'string' is a dependency of 'literals' and shouldn't be used directly
        'string': CFamilyLexer.tokens['string'],
        'literals': CFamilyLexer.tokens['literals'],

        'yaccVarname': [
            (r'\$|-?\d+|[A-Za-z_]\w*|\[' + yaccName + r'\]', Name.Variable, '#pop'),
            default('#pop') # don't let a bad variable spoil the whole scan
        ],
        'yaccType': [
            (r'[*()]+', Punctuation), # parentheses are for function pointers
            (r'>', Punctuation, '#pop'),
            (r'\s+', Whitespace),
            include('ctypes'),
            (r'[A-Za-z]\w*', Name.Class)
        ],
        'yaccVars': [
            (r'(\$)(<)', bygroups(Name.Variable, Punctuation),
             ('yaccVarname', 'yaccType')),
            (r'[@$]', Name.Variable, 'yaccVarname')
        ],

        # Mostly lifted from CFamilyLexer, but with Other instead of String,
        # because a string may actually be a Comment.PreprocFile
        'cstring': [
            (r'"', Other, '#pop'),
            (r'\\(.|x[a-fA-F0-9]{2,4}|u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8}|'
             + r'[0-7]{1,3})', Other),
            (r'[^\\"\n]+', Other),  # all other characters
            (r'\\\n', Other),  # line continuation
            (r'\\', Other),  # stray backslash
        ],
        'cBase': [
            (mComment, Other),
            (sComment, Other),
            include('yaccVars'),
            # Also pilfered from CFamilyLexer
            (r'([LuU]|u8)?"', Other, 'cstring'),
            (r"([LuU]|u8)?(')(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])(')",
             Other)
        ],
        'embeddedC': [
            (r'\{', Other, '#push'),
            (r'\}', Other, '#pop'),
            include('cBase'),
            (r'[^{}$@/\'"]+|/', Other)
        ],
        'POSIXembeddedC': [
            (r'%\}', Punctuation, '#pop'),
            include('cBase'),
            (r'[^}$@%/\'"]+|[%}/]', Other)
        ],

        'common': [
            (r'\{', Other, 'embeddedC'),
            (r'<', Punctuation, 'yaccType'),
            (r'\s+', Whitespace),
            (mComment, Comment.Multiline),
            (sComment, Comment.Single),
            include('yaccVars'),
            include('literals')

        ],
        'gettext': [
            # GNU Bison specially allows gettext calls around string tokens
            # (which are also a Bison extension)
            (r'\)', Punctuation, '#pop'),
            include('common')
        ],
        'define': [
            (r'(api\.value\.type)(\s+)(union(?:-directive)?|variant)\b',
             bygroups(Name, Whitespace, Keyword), '#pop'),
            (r'(api\.value\.union\.name)(\s+)([A-Za-z_]\w*)\b',
             bygroups(Name, Whitespace, Name.Class), '#pop'),
            (r'(api\.(?:header|location)\.include)(\s+)'
             + r'(\{)(\s*)("[^"]+"|<[^>]+>)(\s*)(\})',
             bygroups(Name, Whitespace, Punctuation, Whitespace,
                      Comment.PreprocFile, Whitespace, Punctuation),
             '#pop'),
            default('#pop')
        ],
        'declarations': [
            # According to POSIX, just a `%%' token is enough; it doesn't
            # necessarily have to be on its own line
            (r'%%', Keyword, '#pop'),
            (r'%\{', Punctuation, 'POSIXembeddedC'),
            (r'^(%)(define)(\s+)', bygroups(Punctuation, Keyword, Whitespace), 'define'),
            (r'^(%)([\w-]+)', bygroups(Punctuation, Keyword)),
            (r';', Punctuation),
            (r'(_)(\()', bygroups(Name.Function.Magic, Punctuation), 'gettext'),
            (yaccName, Name),
            include('common')
        ],
        'predicate': [
            # Hackily support GNU Bison GLR predicates.  This just keeps
            # the braces balanced
            (r'\{', Other, 'embeddedC'),
            include('common')
        ],
        'rules': [
            (r'%%', Keyword, '#pop'),
            (r'%\?', Keyword, 'predicate'),
            (r'[:;|]', Operator),
            (words(('empty', 'prec'), prefix='%', suffix=r'\b'), Keyword),
            (r'\berror\b', Keyword),
            (r'(' + yaccName + r')(?:(\[)(' + yaccName + r')(\]))?',
             bygroups(Name, Punctuation, Name, Punctuation)),
            (yaccName + r'(?:(\[)' + yaccName + r'(\]))?',
             bygroups(Name, Punctuation, Name, Punctuation)),
            include('common')
        ],
        'root': [ # aka Yacc `epilogue'
            # Just defer everything to the C lexer
            (r'(?s).+', Other)
        ]
    }

    # Set a default stack
    def get_tokens_unprocessed(self, text, stack=('root', 'rules', 'declarations')):
        yield from RegexLexer.get_tokens_unprocessed(self, text, stack)

class CDelegate(CLexer):
    tokens = {
        'keywords': [
            (r'yy(l(loc|val)|erro[rk]|clear(in|err))\b', Name.Builtin),
            (words(('parse', 'lex', 'debug', 'nerrs', 'tname', 'char'),
                   prefix='yy', suffix=r'\b'), Name.Builtin),
            (words(('ABORT', 'ACCEPT', 'NOMEM', 'EOF', 'error', 'ERROR',
                    'RECOVERING', 'DEBUG'),
                   prefix='YY', suffix=r'\b'), Name.Builtin),
            (r'YY[SL]TYPE\b', Name.Builtin),
            inherit
        ]
    }

class YaccForC(YaccBase):
    tokens = {
        # [TODO] highlighting of optional types?
        'ctypes': [
            (words(('int8', 'int16', 'int32', 'int64', 'wchar_t'), prefix=r'__',
                    suffix=r'\b'), Keyword.Reserved),
            (words(('bool', 'int', 'long', 'float', 'short', 'double', 'char',
                    'unsigned', 'signed', 'void', '_BitInt', '__int128',
                    '_Bool', '_Complex', '_Atomic'),
                   suffix=r'\b'), Keyword.Type),
            (words(('asm', 'auto', 'const', 'enum', 'extern', 'register',
                    'restricted', 'sizeof', 'struct', 'static',
                    'volatile', 'union', 'thread_local', '_Pragma',
                    '_Noreturn', 'noreturn', '_Thread_local',
                    '_Imaginary', 'imaginary', 'complex'),
                   suffix=r'\b'), Keyword),
            (words(('inline', '_inline', '__inline', 'naked', 'restrict',
                    'thread'), suffix=r'\b'), Keyword.Reserved)
        ]
    }

class YaccCLexer(DelegatingLexer):
    """
    Lexer for Yacc
    """
    name = 'Yacc'
    aliases = ['yacc', 'bison']
    filenames = ['*.y']
    url = 'https://pubs.opengroup.org/onlinepubs/9799919799/utilities/yacc.html'
    version_added = '2.20'
    def __init__(self, **options):
        super().__init__(CDelegate, YaccForC, **options)

# Bison is the only implementation that explicitly supports C++, or supports
# any of the other languages

class CppDelegate(CppLexer):
    tokens = {
        'keywords': [
            (r'yy(errok)?\b', Name.Builtin),
            inherit
        ]
    }

class YaccForCpp(YaccBase):
    tokens = {
       'yaccType': [
           (r'::', Operator),
           (r'<', Punctuation, '#push'),
           inherit
       ],
       'ctypes': [
            (words(('int8', 'int16', 'int32', 'int64', 'wchar_t'), prefix=r'__',
                    suffix=r'\b'), Keyword.Reserved),
            (words(('bool', 'int', 'long', 'float', 'short', 'double', 'char',
                    'unsigned', 'signed', 'void', '_BitInt', '__int128'),
                   suffix=r'\b'), Keyword.Type),
            (words(('asm', 'auto', 'const', 'enum', 'extern', 'register',
                    'restricted', 'sizeof', 'struct', 'static',
                    'volatile', 'union', 'thread_local', '_Pragma',
                    'class', 'concept', 'typename', 'nullptr_t', 'template',
                    'decltype', 'noexcept', 'override', 'virtual', 'mutable',
                    'constexpr', 'consteval', 'constinit',
                    'co_await', 'co_return', 'co_yield'),
                   suffix=r'\b'), Keyword),
            (words(('inline', '_inline', '__inline', 'naked', 'restrict',
                    'thread'), suffix=r'\b'), Keyword.Reserved)
        ]
    }

class YaccCppLexer(DelegatingLexer):
    """
    Lexer for Yacc with embedded C++
    """
    name = 'Yacc with C++'
    aliases = ['yacc++', 'bison++', 'yacc-c++', 'bison-c++']
    filenames = ['*.yy']
    url = 'https://www.gnu.org/s/bison'
    version_added = '2.20'
    def __init__(self, **options):
        super().__init__(CppDelegate, YaccForCpp, **options)

class DDelegate(DLexer):
    tokens = {
        'root': [
            # yylex, yyerror etc. are ommitted as they are members of these
            # classes, not global definitions
            (words(('YYParser', 'Lexer', 'Position', 'Location', 'yyerrok'), suffix=r'\b'),
             Name.Builtin),
            inherit
        ]
    }

class YaccForD(YaccBase):
    tokens = {
        'ctypes': DLexer.tokens['keywords'],
        # Pinched wholesale from DLexer, with classes changed to Other.
        # Omitted is token_string, which should be handled fine anyway.
        'cBase': [
            (r'/\+', Other, 'nested_comment'),
            (r'`[^`]*`', Other),
            (r'q"\[', Other, 'delimited_bracket'),
            (r'q"\(', Other, 'delimited_parenthesis'),
            (r'q"<', Other, 'delimited_angle'),
            (r'q"\{', Other, 'delimited_curly'),
            (r'q"([a-zA-Z_]\w*)\n.*?\n\1"', Other),
            (r'q"(.).*?\1"', Other),
            inherit
        ],
        'nested_comment': [
            (r'[^+/]+', Other),
            (r'/\+', Other, '#push'),
            (r'\+/', Other, '#pop'),
            (r'[+/]', Other),
        ],
        'delimited_bracket': [
            (r'[^\[\]]+', Other),
            (r'\[', Other, 'delimited_inside_bracket'),
            (r'\]"', Other, '#pop'),
        ],
        'delimited_inside_bracket': [
            (r'[^\[\]]+', Other),
            (r'\[', Other, '#push'),
            (r'\]', Other, '#pop'),
        ],
        'delimited_parenthesis': [
            (r'[^()]+', Other),
            (r'\(', Other, 'delimited_inside_parenthesis'),
            (r'\)"', Other, '#pop'),
        ],
        'delimited_inside_parenthesis': [
            (r'[^()]+', Other),
            (r'\(', Other, '#push'),
            (r'\)', Other, '#pop'),
        ],
        'delimited_angle': [
            (r'[^<>]+', Other),
            (r'<', Other, 'delimited_inside_angle'),
            (r'>"', Other, '#pop'),
        ],
        'delimited_inside_angle': [
            (r'[^<>]+', Other),
            (r'<', Other, '#push'),
            (r'>', Other, '#pop'),
        ],
        'delimited_curly': [
            (r'[^{}]+', Other),
            (r'\{', Other, 'delimited_inside_curly'),
            (r'\}"', Other, '#pop'),
        ],
        'delimited_inside_curly': [
            (r'[^{}]+', Other),
            (r'\{', Other, '#push'),
            (r'\}', Other, '#pop'),
        ],
    }

class YaccDLexer(DelegatingLexer):
    """
    Lexer for Yacc with embedded D
    """
    name = 'Yacc for D'
    aliases = ['yacc-d', 'bison-d']
    filenames = [] # Only works with *.y, but that's for regular Yacc
    url = 'https://www.gnu.org/s/bison'
    version_added = '2.20'
    def __init__(self, **options):
        super().__init__(DDelegate, YaccForD, **options)

class YaccForJava(YaccBase):
    tokens = {
        'cBase': [
            (r'"""', Other, 'multiline_string'),
            inherit
        ],
        'cstring': [
            (r'"', Other, '#pop'),
            (r'\$\{', Other, 'embeddedC'),
            (r'[^\\"$]+|\$|\\.', Other)
        ],
        'multiline_string': [
            (r'"""', Other, '#pop'),
            (r'"', Other),
            include('cstring')
        ],
        # Mostly filched from JavaLexer
        'ctypes': [
            (r'(boolean|byte|char|double|float|int|long|short|void)\b',
             Keyword.Type),
            (r'(abstract|const|enum|extends|final|implements|native|private|'
             r'protected|public|sealed|static|strictfp|super|synchronized|throws|'
             r'transient|volatile|yield)\b', Keyword.Declaration),
            (r'(class|interface)\b', Keyword.Declaration, 'javaClass'),
            (r'(var)(\s+)', bygroups(Keyword.Declaration, Whitespace), 'javaVar')
        ],
        'javaClass': [
            (r'\s+', Text),
            (r'([^\W\d]|\$)[\w$]*', Name.Class, '#pop')
        ],
        'javaVar': [
            (r'([^\W\d]|\$)[\w$]*', Name, '#pop')
        ]
    }

class YaccJavaLexer(DelegatingLexer):
    """
    Lexer for Yacc with embedded Java
    """
    name = 'Yacc for Java'
    aliases = ['yacc-java', 'bison-java']
    filenames = [] # Only works with *.y, but that's for regular Yacc
    url = 'https://www.gnu.org/s/bison'
    version_added = '2.20'
    def __init__(self, **options):
        super().__init__(JavaLexer, YaccForJava, **options)
