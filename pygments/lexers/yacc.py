"""
    pygments.lexers.yacc
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for Yacc grammars.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, DelegatingLexer, include, bygroups, \
    words, default
from pygments.token import Keyword, Punctuation, Comment, Other, Name, \
    Whitespace, Operator
from pygments.lexers.c_cpp import CLexer, CFamilyLexer

__all__ = ['YaccLexer']

class YaccBase(RegexLexer):
    yaccType = r'<(?:\s*[A-Za-z][*\w\s()]*|\*)?>' # parentheses are for function pointers
    yaccName = r'[A-Za-z_][-\w.]*'
    yaccVarname = r'\$|-?\d+|[A-Za-z_]\w*|\[' + yaccName + r'\]'

    tokens = {
        'string': CFamilyLexer.tokens['string'],
        'literals': CFamilyLexer.tokens['literals'],
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
            (r'\{', Other, 'embeddedC'),
            (yaccType, Keyword.Type),
            (r'(?s)/\*.*?\*/', Comment),
            (r'//.*', Comment),
            (r'\s+', Whitespace),
            include('literals'),
            include('yaccVars')
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
        super().__init__(CLexer, YaccBase, **options)
