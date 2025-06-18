"""
    pygments.lexers.yacc
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for Yacc grammars.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, DelegatingLexer, include, bygroups, words
from pygments.token import Keyword, Punctuation, Comment, Other, Name, \
    Whitespace, String, Operator
from pygments.lexers.c_cpp import CLexer

__all__ = ['YaccLexer']

class YaccBase(RegexLexer):
    yaccType = r'<\s*[A-Za-z][*\w\s()]*>' # parentheses are for function pointers
    yaccName = r'[A-Za-z_][-\w.]*'
    yaccVarname = r'\$|-?\d+|[A-Za-z_]\w*|\[' + yaccName + r'\]'

    tokens = {
        # Pilfered from c_cpp.py
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|'
             r'u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String),  # all other characters
            (r'\\\n', String),  # line continuation
            (r'\\', String),  # stray backslash
        ],
        'yaccVars': [
            (r'(\$)(' + yaccType + r')?(' + yaccVarname + ')',
             bygroups(Name.Variable, Keyword.Type, Name.Variable)),
            (r'@(' + yaccVarname + ')', Name.Variable)
        ],
        'embeddedC': [
            (r'\{', Other, '#push'),
            (r'\}', Other, '#pop'),
            (r'[^{}$@]+', Other),
            include('yaccVars')
        ],
        'POSIXembeddedC': [
            (r'%\}', Punctuation, '#pop'),
            include('yaccVars'),
            (r'[^}$@%]+|[%}]', Other)
        ],
        'common': [
            # According to POSIX, just a `%%' token is enough; it doesn't
            # necessarily have to be on its own line
            (r'%%', Keyword, '#pop'),
            (r'\{', Other, 'embeddedC'),
            (yaccType, Keyword.Type),
            (r'(?s)/\*.*?\*/', Comment),
            (r'//.*', Comment),
            (r'\s+', Whitespace),
            # Also purloined from c_cpp.py
            (r'([LuU]|u8)?(")', bygroups(String.Affix, String), 'string'),
            (r"([LuU]|u8)?(')(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])(')",
             bygroups(String.Affix, String.Char, String.Char, String.Char)),
            include('yaccVars')
        ],
        'gettext': [
            # GNU Bison specially allows gettext calls around string tokens
            # (which are also a Bison extension)
            (r'\)', Punctuation, '#pop'),
            include('common')
        ],
        'declarations': [
            (r'%\{', Punctuation, 'POSIXembeddedC'),
            (r'^(%)([-\w]+)', bygroups(Punctuation, Keyword)),
            (r';', Punctuation),
            (r'(_)(\()', bygroups(Name.Function.Magic, Punctuation), 'gettext'),
            (yaccName, Name),
            include('common')
        ],
        'rules': [
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
        super().__init__(CLexer, YaccBase, **options)
