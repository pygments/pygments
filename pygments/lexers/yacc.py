"""
    pygments.lexers.yacc
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for Yacc grammars.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, DelegatingLexer, include, bygroups, \
    words, default, inherit
from pygments.token import Keyword, Punctuation, Comment, Other, Name, \
    Whitespace, Operator
from pygments.lexers.c_cpp import CLexer, CFamilyLexer

__all__ = ['YaccLexer']

class YaccCLexer(CLexer):
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

class YaccBase(RegexLexer):
    yaccName = r'[A-Za-z_][-\w.]*'
    sComment = r'//.*'
    mComment = r'(?s)/\*.*?\*/'

    tokens = {
        # 'string' is a dependency of 'literals' and shouldn't be used directly
        'string': CFamilyLexer.tokens['string'],
        'literals': CFamilyLexer.tokens['literals'],
        'ctypes': CFamilyLexer.tokens['types'],

        'yaccVarname': [
            (r'\$|-?\d+|[A-Za-z_]\w*|\[' + yaccName + r'\]', Name.Variable, '#pop'),
            default('#pop') # don't let a bad variable spoil the whole scan
        ],
        'yaccType': [
            (r'[*()]+', Punctuation), # parentheses are for function pointers
            (r'>', Punctuation, '#pop'),
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

class YaccLexer(DelegatingLexer):
    name = 'Yacc'
    aliases = ['yacc', 'bison']
    filenames = ['*.y']
    url = 'https://pubs.opengroup.org/onlinepubs/9799919799/utilities/yacc.html'
    version_added = '2.20'
    def __init__(self, **options):
        super().__init__(YaccCLexer, YaccBase, **options)
