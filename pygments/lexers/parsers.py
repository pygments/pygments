# -*- coding: utf-8 -*-
"""
    pygments.lexers.parsers
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for parser generators.

    :copyright: 2008 by Ana Nelson <ana@ananelson.com>.
    :license: BSD, see LICENSE for more details.
"""

import re

from pygments.lexer import RegexLexer, DelegatingLexer, \
    include, bygroups, using, this
from pygments.token import Error, Punctuation, Generic, Other, \
    Text, Comment, Operator, Keyword, Name, String, Number, Whitespace
from pygments.lexers.compiled import JavaLexer, CLexer, CppLexer, \
    ObjectiveCLexer, DLexer
from pygments.lexers.dotnet import CSharpLexer
from pygments.lexers.agile import RubyLexer, PythonLexer, PerlLexer
from pygments.lexers.web import ActionScriptLexer
# Use TextLexer during development to just focus on one part of a delegating lexer.
from pygments.lexers.special import TextLexer

__all__ = ['RagelLexer', 'RagelEmbeddedLexer', 'RagelCLexer', 'RagelDLexer', \
    'RagelCppLexer', 'RagelObjectiveCLexer', 'RagelRubyLexer', 'RagelJavaLexer', \
    'AntlrLexer', 'AntlrPythonLexer', 'AntlrPerlLexer', 'AntlrRubyLexer', \
    'AntlrCppLexer', 'AntlrCLexer', 'AntlrCSharpLexer', 'AntlrObjectiveCLexer', \
    'AntlrJavaLexer', "AntlrActionScriptLexer"]

class RagelLexer(RegexLexer):
    """
    A pure Ragel lexer.
    Only call this for ragel fragments.
    An .rl file needs the RagelEmbeddedLexer.
    """

    name = 'Ragel'
    aliases = ['ragel']
    filenames = []

    tokens = {
        'whitespace': [
            (r'\s+', Whitespace)
        ],
        'comments': [
            (r'\#.*$', Comment),
        ],
        'keywords': [
            (r'(access|action|alphtype)\b', Keyword),
            (r'(getkey|write|machine|include)\b', Keyword),
            (r'(any|ascii|extend|alpha|digit|alnum|lower|upper)\b', Keyword),
            (r'(xdigit|cntrl|graph|print|punct|space|zlen|empty)\b', Keyword)
        ],
        'numbers': [
            (r'0x[0-9A-Fa-f]+', Number.Hex),
            (r'[+-]?[0-9]+', Number.Integer),
        ],
        'literals': [
            (r'"(\\\\|\\"|[^"])*"', String), # double quote string
            (r"'(\\\\|\\'|[^'])*'", String), # single quote string
            (r'\[(\\\\|\\\]|[^\]])*\]', String), # square bracket literals
            (r'/(?!\*)(\\\\|\\/|[^/])*/', String.Regex), # regular expressions
        ],
        'identifiers': [
            (r'[a-zA-Z_][a-zA-Z_0-9]*', Name.Variable),
        ],
        'operators': [
            (r',', Operator), # Join
            (r'\||&|-|--', Operator), # Union, Intersection and Subtraction
            (r'\.|<:|:>|:>>', Operator), # Concatention
            (r':', Operator), # Label
            (r'->', Operator), # Epsilon Transition
            (r'(>|\$|%|<|@|<>)(/|eof\b)', Operator), # EOF Actions
            (r'(>|\$|%|<|@|<>)(!|err\b)', Operator), # Global Error Actions
            (r'(>|\$|%|<|@|<>)(\^|lerr\b)', Operator), # Local Error Actions
            (r'(>|\$|%|<|@|<>)(~|to\b)', Operator), # To-State Actions
            (r'(>|\$|%|<|@|<>)(\*|from\b)', Operator), # From-State Actions
            (r'>|@|\$|%', Operator), # Transition Actions and Priorities
            (r'\*|\?|\+|{[0-9]*,[0-9]*}', Operator), # Repetition
            (r'!|\^', Operator), # Negation
            (r'\(|\)', Operator), # Grouping
        ],
        'root': [
            include('literals'),
            include('whitespace'),
            include('comments'),
            include('keywords'),
            include('numbers'),
            include('identifiers'),
            include('operators'),
            (r'{', Punctuation, 'host'),
            (r'=', Operator),
            (r';', Punctuation),
        ],
        'host': [
            (r'(' + r'|'.join(( # keep host code in largest possible chunks
                r'[^{}\'"/#]+', # exclude unsafe characters
                r'[^\\][\\][{}]', # allow escaped { or }

                # strings and comments may safely contain unsafe characters
                r'"(\\\\|\\"|[^"])*"', # double quote string
                r"'(\\\\|\\'|[^'])*'", # single quote string
                r'//.*$\n?', # single line comment
                r'/\*(.|\n)*?\*/', # multi-line javadoc-style comment
                r'\#.*$\n?', # ruby comment

                # regular expression: There's no reason for it to start
                # with a * and this stops confusion with comments.
                r'/(?!\*)(\\\\|\\/|[^/])*/',

                # / is safe now that we've handled regex and javadoc comments
                r'/',
            )) + r')+', Other),

            (r'{', Punctuation, '#push'),
            (r'}', Punctuation, '#pop'),
        ],
    }

class RagelEmbeddedLexer(RegexLexer):
    """
    A lexer for Ragel embedded in a host language file.
    This will only highlight Ragel statements, if you want host
    language highlighting then call the language-specific ragel lexer.
    """

    name = 'Embedded Ragel'
    aliases = ['ragel-em']
    filenames = ['*.rl']

    tokens = {
        'root': [
            (r'(' + r'|'.join(( # keep host code in largest possible chunks
                r'[^%\'"/#]+', # exclude unsafe characters
                r'%(?=[^%]|$)', # a single % sign is okay, just not 2 of them

                # strings and comments may safely contain unsafe characters
                r'"(\\\\|\\"|[^"])*"', # double quote string
                r"'(\\\\|\\'|[^'])*'", # single quote string
                r'/\*(.|\n)*?\*/', # multi-line javadoc-style comment
                r'//.*$\n?', # single line comment
                r'\#.*$\n?', # ruby/ragel comment
                r'/(?!\*)(\\\\|\\/|[^/])*/', # regular expression

                # / is safe now that we've handled regex and javadoc comments
                r'/',
            )) + r')+', Other),

            # Single Line FSM.
            # Please don't put a quoted newline in a single line FSM.
            # That's just mean. It will break this.
            (r'(%%)(?![{%])(.*)($|;)\n?', bygroups(Punctuation, using(RagelLexer))),

            # Multi Line FSM.
            (r'(%%%%|%%){', Punctuation, 'multi-line-fsm'),
        ],
        'multi-line-fsm': [
            (r'(' + r'|'.join(( # keep ragel code in largest possible chunks.
                r'(' + r'|'.join((
                    r'[^}\'"\[/#]', # exclude unsafe characters
                    r'}(?=[^%]|$)', # } is okay as long as it's not followed by %
                    r'}%(?=[^%]|$)', # ...well, one %'s okay, just not two...
                    r'[^\\][\\][{}]', # ...and } is okay if it's escaped

                    # allow / if it's preceded with one of these symbols
                    # (ragel EOF actions)
                    r'(>|\$|%|<|@|<>)/',

                    # specifically allow regex followed immediately by *
                    # so it doesn't get mistaken for a comment
                    r'/(?!\*)(\\\\|\\/|[^/])*/\*',

                    # allow / as long as it's not followed by another / or by a *
                    r'/(?=[^/\*]|$)',

                    # We want to match as many of these as we can in one block.
                    # Not sure if we need the + sign here,
                    # does it help performance?
                    )) + r')+',

                # strings and comments may safely contain unsafe characters
                r'"(\\\\|\\"|[^"])*"', # double quote string
                r"'(\\\\|\\'|[^'])*'", # single quote string
                r"\[(\\\\|\\\]|[^\]])*\]", # square bracket literal
                r'/\*(.|\n)*?\*/', # multi-line javadoc-style comment
                r'//.*$\n?', # single line comment
                r'\#.*$\n?', # ruby/ragel comment
            )) + r')+', using(RagelLexer)),

            (r'}%%', Punctuation, '#pop'),
        ]
    }

    def analyse_text(text):
        return '@LANG: indep' in text

class RagelRubyLexer(DelegatingLexer):
     """
     A lexer for Ragel in a Ruby host file
     """

     name = 'Ragel in Ruby Host'
     aliases = ['ragel-ruby', 'ragel-rb']
     filenames = ['*.rl']

     def __init__(self, **options):
         super(RagelRubyLexer, self).__init__(RubyLexer, RagelEmbeddedLexer,
                                              **options)

     def analyse_text(text):
         return '@LANG: ruby' in text

class RagelCLexer(DelegatingLexer):
    """
    A lexer for Ragel in a C host file
    """

    name = 'Ragel in C Host'
    aliases = ['ragel-c']
    filenames = ['*.rl']

    def __init__(self, **options):
        super(RagelCLexer, self).__init__(CLexer, RagelEmbeddedLexer,
                                          **options)

    def analyse_text(text):
        return '@LANG: c' in text

class RagelDLexer(DelegatingLexer):
    """
    A lexer for Ragel in a D host file
    """

    name = 'Ragel in D Host'
    aliases = ['ragel-d']
    filenames = ['*.rl']

    def __init__(self, **options):
        super(RagelDLexer, self).__init__(DLexer, RagelEmbeddedLexer, **options)

    def analyse_text(text):
        return '@LANG: d' in text

class RagelCppLexer(DelegatingLexer):
    """
    A lexer for Ragel in a CPP host file
    """

    name = 'Ragel in CPP Host'
    aliases = ['ragel-cpp']
    filenames = ['*.rl']

    def __init__(self, **options):
        super(RagelCppLexer, self).__init__(CppLexer, RagelEmbeddedLexer, **options)

    def analyse_text(text):
        return '@LANG: c++' in text

class RagelObjectiveCLexer(DelegatingLexer):
    """
    A lexer for Ragel in an Objective C host file
    """

    name = 'Ragel in Objective C Host'
    aliases = ['ragel-objc']
    filenames = ['*.rl']

    def __init__(self, **options):
        super(RagelObjectiveCLexer, self).__init__(ObjectiveCLexer, \
        RagelEmbeddedLexer, **options)

    def analyse_text(text):
        return '@LANG: objc' in text

class RagelJavaLexer(DelegatingLexer):
    """
    A lexer for Ragel in a Java host file
    """

    name = 'Ragel in Java Host'
    aliases = ['ragel-java']
    filenames = ['*.rl']

    def __init__(self, **options):
        super(RagelJavaLexer, self).__init__(JavaLexer, RagelEmbeddedLexer, **options)

    def analyse_text(text):
        return '@LANG: java' in text

class AntlrLexer(RegexLexer):
    """
    Generic ANTLR Lexer.
    Should not be called directly, instead
    use DelegatingLexer for your target language.
    """

    name = 'ANTLR'
    aliases = ['antlr']
    filenames = ['*.G', '*.g']

    _id =          r'[A-Za-z][A-Za-z_0-9]*'
    _TOKEN_REF =   r'[A-Z][A-Za-z_0-9]*'
    _RULE_REF =    r'[a-z][A-Za-z_0-9]*'
    _STRING_LITERAL = r'\'(?:\\\\|\\\'|[^\']*)\''
    _INT = r'[0-9]+'

    tokens = {
        'whitespace': [
            (r'\s+', Whitespace),
        ],
        'comments': [
            (r'//.*$', Comment),
            (r'/\*(.|\n)*?\*/', Comment),
        ],
        'root': [
            include('whitespace'),
            include('comments'),

            (r'(lexer|parser|tree)?(\s*)(grammar\b)(\s*)(' + _id + ')(;)', \
                bygroups(Keyword, Whitespace, Keyword, Whitespace, Name.Class, \
                Punctuation)),
            # optionsSpec
            (r'options\b', Keyword, 'options'),
            # tokensSpec
            (r'tokens\b', Keyword, 'tokens'),
            # attrScope
            (r'(scope)(\s*)(' + _id + ')(\s*)({)', \
                bygroups(Keyword, Whitespace, Name.Variable, Whitespace, \
                Punctuation), 'action'),
            # exception
            (r'(catch|finally)\b', Keyword, 'exception'),
            # action
            (r'(@' + _id + ')(\s*)(::)?(\s*)(' + _id + ')(\s*)({)', \
                bygroups(Name.Label, Whitespace, Punctuation, Whitespace, \
                Name.Label, Whitespace, Punctuation), 'action'),
            # rule
            (r'((?:protected|private|public|fragment)\b)?(\s*)(' + _id + ')(!)?', \
                bygroups(Keyword, Whitespace, Name.Label, Punctuation),
                ('rule-alts', 'rule-prelims')),
        ],
        'exception': [
            (r'\n', Whitespace, '#pop'),
            (r'\s', Whitespace),
            include('comments'),

            (r'\[', Punctuation, 'nested-arg-action'),
            (r'\{', Punctuation, 'action'),
        ],
        'rule-prelims': [
            include('whitespace'),
            include('comments'),

            (r'returns\b', Keyword),
            (r'\[', Punctuation, 'nested-arg-action'),
            (r'\{', Punctuation, 'action'),
            # throwsSpec
            (r'(throws)(\s+)(' + _id + ')', bygroups(Keyword, Whitespace, \
            Name.Label)),
            (r'(?:(,)(\s*)(' + _id + '))+', bygroups(Punctuation, Whitespace, \
            Name.Label)), # Additional throws
            # optionsSpec
            (r'options\b', Keyword, 'options'),
            # ruleScopeSpec - scope followed by target language code or name of action
            # TODO finish implementing other possibilities for scope
            # L173 ANTLRv3.g from ANTLR book
            (r'(scope)(\s+)({)', bygroups(Keyword, Whitespace, Punctuation), \
            'action'),
            (r'(scope)(\s+)(' + _id + ')(\s*)(;)', bygroups(Keyword, Whitespace, \
            Name.Label, Whitespace, Punctuation)),
            # ruleAction
            (r'(@' + _id + ')(\s*)({)', \
                bygroups(Name.Label, Whitespace, Punctuation), 'action'),
            # finished prelims, go to rule alts!
            (r':', Punctuation, '#pop')
        ],
        'rule-alts': [
            include('whitespace'),
            include('comments'),

            # These might need to go in a separate 'block' state triggered by (
            (r'options\b', Keyword, 'options'),
            (r':', Punctuation),

            # literals
            (r"'(\\\\|\\'|[^'])*'", String),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r'<<([^>]|>[^>])>>', String),
            # identifiers
            # Tokens start with capital letter.
            (r'\$?[A-Z_][A-Za-z_0-9]*', Name.Constant),
             # Rules start with small letter.
            (r'\$?[a-z_][A-Za-z_0-9]*', Name.Variable),
            # operators
            (r'(\+|\||->|=>|=|\(|\)|\.\.|\.|\?|\*|\^|!|\#|~)', Operator),
            (r',', Punctuation),
            (r'\[', Punctuation, 'nested-arg-action'),
            (r'\{', Punctuation, 'action'),
            (r';', Punctuation, '#pop')
        ],
        'tokens': [
            include('whitespace'),
            include('comments'),
            (r'{', Punctuation),
            (r'(' + _TOKEN_REF + r')(\s*)(=)?(\s*)(' + _STRING_LITERAL + ')?(\s*)(;)', \
                bygroups(Name.Label, Whitespace, Punctuation, Whitespace, \
                String, Whitespace, Punctuation)),
            (r'}', Punctuation, '#pop'),
        ],
        'options': [
            include('whitespace'),
            include('comments'),
            (r'{', Punctuation),
            (r'(' + _id + r')(\s*)(=)(\s*)(' + \
            '|'.join((_id, _STRING_LITERAL, _INT, '\*'))+ ')(\s*)(;)',\
            bygroups(Name.Variable, Whitespace, Punctuation, \
            Whitespace, Text, Whitespace, Punctuation)),
            (r'}', Punctuation, '#pop'),
        ],
        'action': [
            (r'(' + r'|'.join(( # keep host code in largest possible chunks
                r'[^\${}\'"/\\]+', # exclude unsafe characters

                # strings and comments may safely contain unsafe characters
                r'"(\\\\|\\"|[^"])*"', # double quote string
                r"'(\\\\|\\'|[^'])*'", # single quote string
                r'//.*$\n?', # single line comment
                r'/\*(.|\n)*?\*/', # multi-line javadoc-style comment

                # regular expression: There's no reason for it to start
                # with a * and this stops confusion with comments.
                r'/(?!\*)(\\\\|\\/|[^/])*/',

                # backslashes are okay, as long as we are not backslashing a %
                r'\\(?!%)',

                # Now that we've handled regex and javadoc comments
                # it's safe to let / through.
                r'/',
            )) + r')+', Other),
            (r'(\\)(%)', bygroups(Punctuation, Other)),
            (r'(\$[a-zA-Z]+)(\.?)(text|value)?', \
            bygroups(Name.Variable, Punctuation, Name.Property)),
            (r'{', Punctuation, '#push'),
            (r'}', Punctuation, '#pop'),
        ],
        'nested-arg-action': [
            (r'(' + r'|'.join(( # keep host code in largest possible chunks.
                r'[^\$\[\]\'"/]+', # exclude unsafe characters

                # strings and comments may safely contain unsafe characters
                r'"(\\\\|\\"|[^"])*"', # double quote string
                r"'(\\\\|\\'|[^'])*'", # single quote string
                r'//.*$\n?', # single line comment
                r'/\*(.|\n)*?\*/', # multi-line javadoc-style comment

                # regular expression: There's no reason for it to start
                # with a * and this stops confusion with comments.
                r'/(?!\*)(\\\\|\\/|[^/])*/',

                # Now that we've handled regex and javadoc comments
                # it's safe to let / through.
                r'/',
            )) + r')+', Other),


            (r'\[', Punctuation, '#push'),
            (r'\]', Punctuation, '#pop'),
            (r'(\$[a-zA-Z]+)(\.?)(text|value)?', \
            bygroups(Name.Variable, Punctuation, Name.Property)),
            (r'(\\\\|\\\]|\\\[|[^\[\]])+', Other),
        ]
    }

# http://www.antlr.org/wiki/display/ANTLR3/Code+Generation+Targets

class AntlrCLexer(DelegatingLexer):
    """
    ANTLR with C Target
    """

    name = 'ANTLR With C Target'
    aliases = ['antlr-c']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrCLexer, self).__init__(CLexer, AntlrLexer, **options)

class AntlrCppLexer(DelegatingLexer):
    """
    ANTLR with CPP Target
    """

    name = 'ANTLR With CPP Target'
    aliases = ['antlr-cpp']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrCppLexer, self).__init__(CppLexer, AntlrLexer, **options)

class AntlrObjectiveCLexer(DelegatingLexer):
    """
    ANTLR with ObjectiveC Target
    """

    name = 'ANTLR With ObjectiveC Target'
    aliases = ['antlr-objc']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrObjectiveCLexer, self).__init__(ObjectiveCLexer,\
        AntlrLexer, **options)

class AntlrCSharpLexer(DelegatingLexer):
    """
    ANTLR with C# Target
    """

    name = 'ANTLR With C# Target'
    aliases = ['antlr-csharp', 'antlr-c#']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrCSharpLexer, self).__init__(CSharpLexer, AntlrLexer, **options)

class AntlrPythonLexer(DelegatingLexer):
    """
    ANTLR with Python Target
    """

    name = 'ANTLR With Python Target'
    aliases = ['antlr-python']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrPythonLexer, self).__init__(PythonLexer, AntlrLexer, **options)


class AntlrJavaLexer(DelegatingLexer):
    """
    ANTLR with Java Target
    """

    name = 'ANTLR With Java Target'
    aliases = ['antlr-java']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrJavaLexer, self).__init__(JavaLexer, AntlrLexer, **options)


class AntlrRubyLexer(DelegatingLexer):
    """
    ANTLR with Ruby Target
    """

    name = 'ANTLR With Ruby Target'
    aliases = ['antlr-ruby', 'antlr-rb']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrRubyLexer, self).__init__(RubyLexer, AntlrLexer, **options)

class AntlrPerlLexer(DelegatingLexer):
    """
    ANTLR with Perl Target
    """

    name = 'ANTLR With Perl Target'
    aliases = ['antlr-perl']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrPerlLexer, self).__init__(PerlLexer, AntlrLexer, **options)

class AntlrActionScriptLexer(DelegatingLexer):
    """
    ANTLR with ActionScript Target
    """

    name = 'ANTLR With ActionScript Target'
    aliases = ['antlr-as', 'antlr-actionscript']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrActionScriptLexer, self).__init__(ActionScriptLexer,\
        AntlrLexer, **options)
