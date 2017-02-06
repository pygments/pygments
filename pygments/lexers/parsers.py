# -*- coding: utf-8 -*-
"""
    pygments.lexers.parsers
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for parser generators.

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, DelegatingLexer, \
    include, bygroups, using, default
from pygments.token import Punctuation, Other, Text, Comment, Operator, \
    Keyword, Name, String, Number, Whitespace
from pygments.lexers.jvm import JavaLexer
from pygments.lexers.c_cpp import CLexer, CppLexer
from pygments.lexers.objective import ObjectiveCLexer
from pygments.lexers.d import DLexer
from pygments.lexers.dotnet import CSharpLexer
from pygments.lexers.ruby import RubyLexer
from pygments.lexers.python import PythonLexer
from pygments.lexers.perl import PerlLexer

__all__ = ['RagelLexer', 'RagelEmbeddedLexer', 'RagelCLexer', 'RagelDLexer',
           'RagelCppLexer', 'RagelObjectiveCLexer', 'RagelRubyLexer',
           'RagelJavaLexer', 'AntlrLexer', 'Antlr4Lexer', 'AntlrPythonLexer',
           'AntlrPerlLexer', 'AntlrRubyLexer', 'AntlrCppLexer',
           # 'AntlrCLexer',
           'AntlrCSharpLexer', 'AntlrObjectiveCLexer',
           'AntlrJavaLexer', 'Antlr4JavaLexer', 'AntlrActionScriptLexer',
           'TreetopLexer', 'EbnfLexer']


class RagelLexer(RegexLexer):
    """
    A pure `Ragel <http://www.complang.org/ragel/>`_ lexer.  Use this for
    fragments of Ragel.  For ``.rl`` files, use RagelEmbeddedLexer instead
    (or one of the language-specific subclasses).

    .. versionadded:: 1.1
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
            (r'"(\\\\|\\"|[^"])*"', String),              # double quote string
            (r"'(\\\\|\\'|[^'])*'", String),              # single quote string
            (r'\[(\\\\|\\\]|[^\]])*\]', String),          # square bracket literals
            (r'/(?!\*)(\\\\|\\/|[^/])*/', String.Regex),  # regular expressions
        ],
        'identifiers': [
            (r'[a-zA-Z_]\w*', Name.Variable),
        ],
        'operators': [
            (r',', Operator),                           # Join
            (r'\||&|--?', Operator),                    # Union, Intersection and Subtraction
            (r'\.|<:|:>>?', Operator),                  # Concatention
            (r':', Operator),                           # Label
            (r'->', Operator),                          # Epsilon Transition
            (r'(>|\$|%|<|@|<>)(/|eof\b)', Operator),    # EOF Actions
            (r'(>|\$|%|<|@|<>)(!|err\b)', Operator),    # Global Error Actions
            (r'(>|\$|%|<|@|<>)(\^|lerr\b)', Operator),  # Local Error Actions
            (r'(>|\$|%|<|@|<>)(~|to\b)', Operator),     # To-State Actions
            (r'(>|\$|%|<|@|<>)(\*|from\b)', Operator),  # From-State Actions
            (r'>|@|\$|%', Operator),                    # Transition Actions and Priorities
            (r'\*|\?|\+|\{[0-9]*,[0-9]*\}', Operator),  # Repetition
            (r'!|\^', Operator),                        # Negation
            (r'\(|\)', Operator),                       # Grouping
        ],
        'root': [
            include('literals'),
            include('whitespace'),
            include('comments'),
            include('keywords'),
            include('numbers'),
            include('identifiers'),
            include('operators'),
            (r'\{', Punctuation, 'host'),
            (r'=', Operator),
            (r';', Punctuation),
        ],
        'host': [
            (r'(' + r'|'.join((  # keep host code in largest possible chunks
                r'[^{}\'"/#]+',  # exclude unsafe characters
                r'[^\\]\\[{}]',  # allow escaped { or }

                # strings and comments may safely contain unsafe characters
                r'"(\\\\|\\"|[^"])*"',  # double quote string
                r"'(\\\\|\\'|[^'])*'",  # single quote string
                r'//.*$\n?',            # single line comment
                r'/\*(.|\n)*?\*/',      # multi-line javadoc-style comment
                r'\#.*$\n?',            # ruby comment

                # regular expression: There's no reason for it to start
                # with a * and this stops confusion with comments.
                r'/(?!\*)(\\\\|\\/|[^/])*/',

                # / is safe now that we've handled regex and javadoc comments
                r'/',
            )) + r')+', Other),

            (r'\{', Punctuation, '#push'),
            (r'\}', Punctuation, '#pop'),
        ],
    }


class RagelEmbeddedLexer(RegexLexer):
    """
    A lexer for `Ragel`_ embedded in a host language file.

    This will only highlight Ragel statements. If you want host language
    highlighting then call the language-specific Ragel lexer.

    .. versionadded:: 1.1
    """

    name = 'Embedded Ragel'
    aliases = ['ragel-em']
    filenames = ['*.rl']

    tokens = {
        'root': [
            (r'(' + r'|'.join((   # keep host code in largest possible chunks
                r'[^%\'"/#]+',    # exclude unsafe characters
                r'%(?=[^%]|$)',   # a single % sign is okay, just not 2 of them

                # strings and comments may safely contain unsafe characters
                r'"(\\\\|\\"|[^"])*"',  # double quote string
                r"'(\\\\|\\'|[^'])*'",  # single quote string
                r'/\*(.|\n)*?\*/',      # multi-line javadoc-style comment
                r'//.*$\n?',  # single line comment
                r'\#.*$\n?',  # ruby/ragel comment
                r'/(?!\*)(\\\\|\\/|[^/])*/',  # regular expression

                # / is safe now that we've handled regex and javadoc comments
                r'/',
            )) + r')+', Other),

            # Single Line FSM.
            # Please don't put a quoted newline in a single line FSM.
            # That's just mean. It will break this.
            (r'(%%)(?![{%])(.*)($|;)(\n?)', bygroups(Punctuation,
                                                     using(RagelLexer),
                                                     Punctuation, Text)),

            # Multi Line FSM.
            (r'(%%%%|%%)\{', Punctuation, 'multi-line-fsm'),
        ],
        'multi-line-fsm': [
            (r'(' + r'|'.join((  # keep ragel code in largest possible chunks.
                r'(' + r'|'.join((
                    r'[^}\'"\[/#]',   # exclude unsafe characters
                    r'\}(?=[^%]|$)',   # } is okay as long as it's not followed by %
                    r'\}%(?=[^%]|$)',  # ...well, one %'s okay, just not two...
                    r'[^\\]\\[{}]',   # ...and } is okay if it's escaped

                    # allow / if it's preceded with one of these symbols
                    # (ragel EOF actions)
                    r'(>|\$|%|<|@|<>)/',

                    # specifically allow regex followed immediately by *
                    # so it doesn't get mistaken for a comment
                    r'/(?!\*)(\\\\|\\/|[^/])*/\*',

                    # allow / as long as it's not followed by another / or by a *
                    r'/(?=[^/*]|$)',

                    # We want to match as many of these as we can in one block.
                    # Not sure if we need the + sign here,
                    # does it help performance?
                )) + r')+',

                # strings and comments may safely contain unsafe characters
                r'"(\\\\|\\"|[^"])*"',      # double quote string
                r"'(\\\\|\\'|[^'])*'",      # single quote string
                r"\[(\\\\|\\\]|[^\]])*\]",  # square bracket literal
                r'/\*(.|\n)*?\*/',          # multi-line javadoc-style comment
                r'//.*$\n?',                # single line comment
                r'\#.*$\n?',                # ruby/ragel comment
            )) + r')+', using(RagelLexer)),

            (r'\}%%', Punctuation, '#pop'),
        ]
    }

    def analyse_text(text):
        return '@LANG: indep' in text


class RagelRubyLexer(DelegatingLexer):
    """
    A lexer for `Ragel`_ in a Ruby host file.

    .. versionadded:: 1.1
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
    A lexer for `Ragel`_ in a C host file.

    .. versionadded:: 1.1
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
    A lexer for `Ragel`_ in a D host file.

    .. versionadded:: 1.1
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
    A lexer for `Ragel`_ in a CPP host file.

    .. versionadded:: 1.1
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
    A lexer for `Ragel`_ in an Objective C host file.

    .. versionadded:: 1.1
    """

    name = 'Ragel in Objective C Host'
    aliases = ['ragel-objc']
    filenames = ['*.rl']

    def __init__(self, **options):
        super(RagelObjectiveCLexer, self).__init__(ObjectiveCLexer,
                                                   RagelEmbeddedLexer,
                                                   **options)

    def analyse_text(text):
        return '@LANG: objc' in text


class RagelJavaLexer(DelegatingLexer):
    """
    A lexer for `Ragel`_ in a Java host file.

    .. versionadded:: 1.1
    """

    name = 'Ragel in Java Host'
    aliases = ['ragel-java']
    filenames = ['*.rl']

    def __init__(self, **options):
        super(RagelJavaLexer, self).__init__(JavaLexer, RagelEmbeddedLexer,
                                             **options)

    def analyse_text(text):
        return '@LANG: java' in text


class AntlrLexer(RegexLexer):
    """
    Generic `ANTLR`_ Lexer.
    Should not be called directly, instead
    use DelegatingLexer for your target language.

    .. versionadded:: 1.1

    .. _ANTLR: http://www.antlr3.org/
    """

    name = 'ANTLR'
    aliases = ['antlr']
    filenames = []

    _id = r'[A-Za-z]\w*'
    _TOKEN_REF = r'[A-Z]\w*'
    _RULE_REF = r'[a-z]\w*'
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

            (r'(lexer|parser|tree)?(\s*)(grammar\b)(\s*)(' + _id + ')(;)',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Name.Class,
                      Punctuation)),
            # optionsSpec
            (r'options\b', Keyword, 'options'),
            # tokensSpec
            (r'tokens\b', Keyword, 'tokens'),
            # attrScope
            (r'(scope)(\s*)(' + _id + r')(\s*)(\{)',
             bygroups(Keyword, Whitespace, Name.Variable, Whitespace,
                      Punctuation), 'action'),
            # exception
            (r'(catch|finally)\b', Keyword, 'exception'),
            # action
            (r'(@' + _id + r')(\s*)(::)?(\s*)(' + _id + r')(\s*)(\{)',
             bygroups(Name.Label, Whitespace, Punctuation, Whitespace,
                      Name.Label, Whitespace, Punctuation), 'action'),
            # rule
            (r'((?:protected|private|public|fragment)\b)?(\s*)(' + _id + ')(!)?',
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
            (r'(throws)(\s+)(' + _id + ')',
             bygroups(Keyword, Whitespace, Name.Label)),
            (r'(,)(\s*)(' + _id + ')',
             bygroups(Punctuation, Whitespace, Name.Label)),  # Additional throws
            # optionsSpec
            (r'options\b', Keyword, 'options'),
            # ruleScopeSpec - scope followed by target language code or name of action
            # TODO finish implementing other possibilities for scope
            # L173 ANTLRv3.g from ANTLR book
            (r'(scope)(\s+)(\{)', bygroups(Keyword, Whitespace, Punctuation),
             'action'),
            (r'(scope)(\s+)(' + _id + r')(\s*)(;)',
             bygroups(Keyword, Whitespace, Name.Label, Whitespace, Punctuation)),
            # ruleAction
            (r'(@' + _id + r')(\s*)(\{)',
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
            (r'\$?[A-Z_]\w*', Name.Constant),
            # Rules start with small letter.
            (r'\$?[a-z_]\w*', Name.Variable),
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
            (r'\{', Punctuation),
            (r'(' + _TOKEN_REF + r')(\s*)(=)?(\s*)(' + _STRING_LITERAL
             + r')?(\s*)(;)',
             bygroups(Name.Label, Whitespace, Punctuation, Whitespace,
                      String, Whitespace, Punctuation)),
            (r'\}', Punctuation, '#pop'),
        ],
        'options': [
            include('whitespace'),
            include('comments'),
            (r'\{', Punctuation),
            (r'(' + _id + r')(\s*)(=)(\s*)(' +
             '|'.join((_id, _STRING_LITERAL, _INT, r'\*')) + r')(\s*)(;)',
             bygroups(Name.Variable, Whitespace, Punctuation, Whitespace,
                      Text, Whitespace, Punctuation)),
            (r'\}', Punctuation, '#pop'),
        ],
        'action': [
            (r'(' + r'|'.join((    # keep host code in largest possible chunks
                r'[^${}\'"/\\]+',  # exclude unsafe characters

                # strings and comments may safely contain unsafe characters
                r'"(\\\\|\\"|[^"])*"',  # double quote string
                r"'(\\\\|\\'|[^'])*'",  # single quote string
                r'//.*$\n?',            # single line comment
                r'/\*(.|\n)*?\*/',      # multi-line javadoc-style comment

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
            (r'(\$[a-zA-Z]+)(\.?)(text|value)?',
             bygroups(Name.Variable, Punctuation, Name.Property)),
            (r'\{', Punctuation, '#push'),
            (r'\}', Punctuation, '#pop'),
        ],
        'nested-arg-action': [
            (r'(' + r'|'.join((    # keep host code in largest possible chunks.
                r'[^$\[\]\'"/]+',  # exclude unsafe characters

                # strings and comments may safely contain unsafe characters
                r'"(\\\\|\\"|[^"])*"',  # double quote string
                r"'(\\\\|\\'|[^'])*'",  # single quote string
                r'//.*$\n?',            # single line comment
                r'/\*(.|\n)*?\*/',      # multi-line javadoc-style comment

                # regular expression: There's no reason for it to start
                # with a * and this stops confusion with comments.
                r'/(?!\*)(\\\\|\\/|[^/])*/',

                # Now that we've handled regex and javadoc comments
                # it's safe to let / through.
                r'/',
            )) + r')+', Other),


            (r'\[', Punctuation, '#push'),
            (r'\]', Punctuation, '#pop'),
            (r'(\$[a-zA-Z]+)(\.?)(text|value)?',
             bygroups(Name.Variable, Punctuation, Name.Property)),
            (r'(\\\\|\\\]|\\\[|[^\[\]])+', Other),
        ]
    }

    def analyse_text(text):
        return re.search(r'^\s*grammar\s+[a-zA-Z0-9]+\s*;', text, re.M)


class Antlr4Lexer(RegexLexer):
    """
    Generic `ANTLR 4`_ Lexer.
    Should not be called directly, instead
    use DelegatingLexer for your target language.

    .. versionadded:: 2.2.1

    .. _ANTLR 4: http://www.antlr.org/
    """

    name = 'ANTLR 4'
    aliases = ['antlr4']
    filenames = []

    # The structure of this lexer is designed heavily upon ANTLRv4Parser.g4

    _comma = '\s*,\s*'

    _id = r'[A-Za-z]\w*'
    # matches a list of _id, e.g. '_id, _id'
    _id_list = _id + r'(?:' + _comma + _id + ')*'
    # same as above, but allow a comma after the last _id
    _id_list_trailing_comma = _id_list + '(?:' + _comma + ')?'

    _token_mods = 'fragment'
    _rule_mods = 'public|protected|private|' + _token_mods

    _TOKEN_REF = r'[A-Z]\w*'
    _RULE_REF = r'[a-z]\w*'

    _s_string_literal = r"'(?:\\\\|\\'|[^'])*'"
    _d_string_literal = r'"(?:\\\\|\\"|[^"])*"'

    _escapes = r'\\(?:[nrbtf\]\-\\]|u\d{4})'

    _INT = r'[0-9]+'

    _sep_slot = object()

    def _sep_ids(bygroup_tuple, sep=u'.', id_class=Name.Class):
        """
        A helper callback to handle `_id`s separated by something.
        The :bygroup_tuple: should be like the tuple for bygroups, but with
        _sep_slot wherever the group for the joined `_id`s is.
        :param sep: the separator for the `_id`s
        """
        split_pattern = re.compile(r'(\s*)(' + re.escape(sep) + r')(\s*)')
        sep_class = Whitespace if sep.isspace() else Punctuation

        def callback(lexer, match):
            global _sep_slot
            for i in range(len(bygroup_tuple)):
                group_index = i + 1
                g = bygroup_tuple[i]
                text = match.group(group_index)
                index = match.start(group_index)
                if g == Antlr4Lexer._sep_slot:
                    text_index = 0
                    m = split_pattern.search(text, text_index)
                    while m is not None:
                        identifier = text[text_index:m.start()]
                        yield (index + text_index, id_class, identifier)

                        ws = m.group(1)
                        if ws:
                            yield (index + m.start(1), Whitespace, ws)

                        separator = m.group(2)
                        if separator:
                            yield (index + m.start(2), sep_class, separator)

                        ws = m.group(3)
                        if ws:
                            yield (index + m.start(3), Whitespace, ws)

                        text_index = m.end()
                        m = split_pattern.search(text, text_index)
                    # yield last identifier, if any
                    if text_index != len(text):
                        yield (index + text_index, id_class, text[text_index:])
                elif text and index != -1:
                    yield (index, g, text)

        return callback

    # really weird scoping rules by python require this default argument setup
    def _host_code(bounding_chars, dsl=_d_string_literal,
                   ssl=_s_string_literal):
        return (r'(' + r'|'.join((  # keep host code in largest possible chunks
            # exclude unsafe characters
            r'[^$' + re.escape(bounding_chars) + r'\'"/]+',

            # strings and comments may safely contain unsafe characters
            dsl,  # double quote string
            ssl,  # single quote string
            r'//.*$\n?',  # single line comment
            r'/\*(.|\n)*?\*/',  # multi-line javadoc-style comment

            # regular expression: There's no reason for it to start
            # with a * and this stops confusion with comments.
            r'/(?!\*)(\\\\|\\/|[^/])*/',

            # Now that we've handled regex and javadoc comments
            # it's safe to let / through.
            r'/',
        )) + r')+', Other)

    tokens = {
        'whitespace': [
            (r'\s+', Whitespace),
        ],
        'comments': [
            (r'//.*?(?:\r|\r\n|\n)', Comment),
            (r'/\*(?:.|\n)*?\*/', Comment.Multiline),
        ],
        'options-block': [
            (r'(options)(\s*)(\{)', bygroups(Keyword, Whitespace, Punctuation),
             'options')
        ],
        'root': [
            include('whitespace'),
            include('comments'),

            (r'(lexer|parser)?(\s*)(grammar)(\s+)(' + _id + ')(;)',
             bygroups(Keyword, Whitespace, Keyword, Whitespace, Name.Class,
                      Punctuation)),
            include('options-block'),
            (r'(import)(\s+)(' + _id_list + r')(\s*)(;)',
             _sep_ids((Keyword, Whitespace, _sep_slot, Whitespace, Punctuation),
                      sep=u',')),
            (r'(tokens)(\s*)(\{)(\s*)(' + _id_list_trailing_comma +
             r')(\s*)(\})',
             _sep_ids(
                 (Keyword, Whitespace, Punctuation, Whitespace, _sep_slot,
                  Whitespace, Punctuation), sep=u','
             )),
            (r'(channels)(\s*)(\{)(\s*)(' + _id_list_trailing_comma +
             r')(\s*)(\})',
             _sep_ids(
                 (Keyword, Whitespace, Punctuation, Whitespace, _sep_slot,
                  Whitespace, Punctuation), sep=u','
             )),
            (r'\@', Punctuation, 'action'),

            (r'(mode)(\s+)(' + _id + ')(\s*)(;)',
             bygroups(Keyword, Whitespace, Name.Class, Whitespace,
                      Punctuation)),
            (r'((?:(?:' + _token_mods + r')\s+)*)(' + _TOKEN_REF + ')',
             _sep_ids((_sep_slot, Name.Class), sep=u' ', id_class=Keyword),
             'token-prequel'),
            (r'((?:(?:' + _rule_mods + r')\s+)*)(' + _RULE_REF + ')',
             _sep_ids((_sep_slot, Name.Class), sep=u' ', id_class=Keyword),
             'rule-prequel'),
        ],
        'options': [
            include('whitespace'),
            include('comments'),

            ('(' + _id + ')' + r'(\s*)(=)(\s*)',
             bygroups(Name.Variable, Whitespace, Operator, Whitespace),
             'option-value'),
            (r'\}', Punctuation, '#pop')
        ],
        'option-value': [
            include('whitespace'),

            # matches text like 'Identifer.Identifier;'
            ('(' + _id +
             r'(?:\s*\.\s*' + _id + ')*' +
             r')(\s*)(;)',
             _sep_ids((_sep_slot, Whitespace, Punctuation)),
             '#pop'),
            # String literal
            ('(' + _s_string_literal + ')(\s*)(;)',
             bygroups(String, Whitespace, Punctuation), '#pop'),
            # Actions
            (r'\{', Punctuation, 'action-block'),
            # Int literal
            ('(' + _INT + ')(\s*)(;)',
             bygroups(Number.Integer, Whitespace, Punctuation), '#pop'),
            # end of option value
            (';', Punctuation, '#pop')
        ],
        'action': [
            include('whitespace'),

            (r'(?:(' + _id + r')(\s*)(::)(\s*))?(\s*)(' + _id + r')(\s*)(\{)',
             bygroups(Name.Class, Whitespace, Punctuation, Whitespace,
                      Whitespace, Name.Variable, Whitespace, Punctuation),
             ('#pop', 'action-block'))
        ],
        'action-block': [
            _host_code('{}'),

            (r'(\$[a-zA-Z]+)(\.?)(text|value)?',
             bygroups(Name.Variable, Punctuation, Name.Property)),
            (r'\{', Punctuation, '#push'),
            (r'\}', Punctuation, '#pop'),
        ],
        'arg-block': [
            _host_code('[]'),

            (r'\[', Punctuation, '#push'),
            (r'\]', Punctuation, '#pop'),
        ],
        'rule-prequel': [
            include('whitespace'),
            include('comments'),

            (r'\[', Punctuation, 'arg-block'),
            (r'(returns)(\s*)(\[)', bygroups(Keyword, Whitespace, Punctuation),
             'arg-block'),
            (r'(throws)(\s+)(' + _id_list + ')',
             _sep_ids((Keyword, Whitespace, _sep_slot), sep=u',')),
            (r'(locals)(\s*)(\[)', bygroups(Keyword, Whitespace, Punctuation),
             'arg-block'),
            (r'(locals)(\s*)(\[)', bygroups(Keyword, Whitespace, Punctuation),
             'arg-block'),
            (r'(\@)(' + _id + r')(\s*)(\{)',
             bygroups(Punctuation, Name.Label, Whitespace, Punctuation),
             'action-block'),
            include('options-block'),
            (r':', Punctuation, ('#pop', 'rule'))
        ],
        'rule-token-shared': [
            include('whitespace'),
            include('comments'),

            (_TOKEN_REF, Name.Variable, 'post-ref'),
            (r'(' + _s_string_literal + r')(\.\.)(' + _s_string_literal + r')',
             bygroups(String, Operator, String), 'post-ref'),
            (_s_string_literal, String, 'post-ref'),
            (r'\[', Punctuation, ('post-ref', 'charset')),
            (r'\.', Punctuation, 'post-ref'),
            (r'~', Operator),
            (r'\(', Punctuation),
            (r'\)', Punctuation, 'post-paren'),
            (r'\|', Operator),
            (r'\{', Punctuation, ('predicate-close', 'action-block'))
        ],
        'charset': [
            (r'(?:(' + _escapes + r')|(.))(-)' +
             r'(?:(' + _escapes + r')|(.))',
             bygroups(String.Escape, String.Regex, Operator, String.Escape,
                      String.Regex)),

            (r'(?:(' + _escapes + r')|([^\]]))',
             bygroups(String.Escape, String.Symbol)),

            (r'\]', Punctuation, '#pop'),
        ],
        'predicate-close': [
            (r'(\?)(\<)',
             bygroups(Operator, Punctuation), ('#pop', 'rule-element-options')),
            (r'\?', Operator, '#pop'),

            default('#pop')
        ],
        'rule': [
            include('rule-token-shared'),

            (_RULE_REF, Name.Variable.Class, 'post-ref'),
            # swap rule for rule-catch
            (r';', Punctuation, ('#pop', 'rule-catch'))
        ],
        'repeaters': [
            include('whitespace'),
            include('comments'),
            (r'\?', Operator),
            (r'[*+]\??', Operator)
        ],
        'post-paren': [
            include('repeaters'),

            default('#pop')
        ],
        'post-ref': [
            include('repeaters'),

            (r'\<', Punctuation, 'rule-element-options'),
            default('#pop')
        ],
        'rule-element-options': [
            include('whitespace'),

            ('(' + _id + ')' + r'(\s*)(=)(\s*)',
             bygroups(Name.Variable, Whitespace, Operator, Whitespace),
             'rule-element-option-value'),
            (_id, Name.Variable),
            (r',', Punctuation),
            (r'\>', Punctuation, '#pop')
        ],
        'rule-element-option-value': [
            (_id, Name.Class, '#pop'),
            (_s_string_literal, String, '#pop'),
            (r'\{', Punctuation, ('#pop', 'action-block'))
        ],
        'rule-catch': [
            include('whitespace'),
            include('comments'),

            (r'(catch)(\s*)(\[)', bygroups(Keyword, Whitespace, Punctuation),
             # switch to arg-block, then follow it with (\{) + action-block
             ('rule-catch-action-block', 'arg-block')),
            default('#pop')
        ],
        'rule-catch-action-block': [
            include('whitespace'),
            include('comments'),

            # swap for action-block
            (r'\{', Punctuation, ('#pop', 'action-block'))
        ],
        'token-prequel': [
            include('whitespace'),
            include('comments'),

            (r':', Punctuation, ('#pop', 'token'))
        ],
        'token': [
            include('rule-token-shared'),

            (r'-\>', Operator, 'token-command'),
            (r';', Punctuation, '#pop')
        ],
        'token-command': [
            include('whitespace'),
            include('comments'),

            (r'skip|more|popMode', Name.Function),
            (r'(mode|pushMode|type|channel)(\s*)' +
             r'(\()' + r'(\s*)(?:(' + _id + ')|(' + _INT + r'))(\s*)' + r'(\))',
             bygroups(Name.Function, Whitespace, Punctuation, Whitespace,
                      Name.Constant, Number.Integer, Whitespace, Punctuation)),
            (r',', Punctuation),
            default('#pop')
        ]
    }

    def analyse_text(text):
        return re.search(r'^\s*grammar\s+[a-zA-Z0-9]+\s*;', text, re.M)

# http://www.antlr.org/wiki/display/ANTLR3/Code+Generation+Targets

# TH: I'm not aware of any language features of C++ that will cause
# incorrect lexing of C files.  Antlr doesn't appear to make a distinction,
# so just assume they're C++.  No idea how to make Objective C work in the
# future.

# class AntlrCLexer(DelegatingLexer):
#    """
#    ANTLR with C Target
#
#    .. versionadded:: 1.1
#    """
#
#    name = 'ANTLR With C Target'
#    aliases = ['antlr-c']
#    filenames = ['*.G', '*.g']
#
#    def __init__(self, **options):
#        super(AntlrCLexer, self).__init__(CLexer, AntlrLexer, **options)
#
#    def analyse_text(text):
#        return re.match(r'^\s*language\s*=\s*C\s*;', text)


class AntlrCppLexer(DelegatingLexer):
    """
    `ANTLR`_ with CPP Target

    .. versionadded:: 1.1
    """

    name = 'ANTLR With CPP Target'
    aliases = ['antlr-cpp']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrCppLexer, self).__init__(CppLexer, AntlrLexer, **options)

    def analyse_text(text):
        return AntlrLexer.analyse_text(text) and \
            re.search(r'^\s*language\s*=\s*C\s*;', text, re.M)


class AntlrObjectiveCLexer(DelegatingLexer):
    """
    `ANTLR`_ with Objective-C Target

    .. versionadded:: 1.1
    """

    name = 'ANTLR With ObjectiveC Target'
    aliases = ['antlr-objc']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrObjectiveCLexer, self).__init__(ObjectiveCLexer,
                                                   AntlrLexer, **options)

    def analyse_text(text):
        return AntlrLexer.analyse_text(text) and \
            re.search(r'^\s*language\s*=\s*ObjC\s*;', text)


class AntlrCSharpLexer(DelegatingLexer):
    """
    `ANTLR`_ with C# Target

    .. versionadded:: 1.1
    """

    name = 'ANTLR With C# Target'
    aliases = ['antlr-csharp', 'antlr-c#']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrCSharpLexer, self).__init__(CSharpLexer, AntlrLexer,
                                               **options)

    def analyse_text(text):
        return AntlrLexer.analyse_text(text) and \
            re.search(r'^\s*language\s*=\s*CSharp2\s*;', text, re.M)


class AntlrPythonLexer(DelegatingLexer):
    """
    `ANTLR`_ with Python Target

    .. versionadded:: 1.1
    """

    name = 'ANTLR With Python Target'
    aliases = ['antlr-python']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrPythonLexer, self).__init__(PythonLexer, AntlrLexer,
                                               **options)

    def analyse_text(text):
        return AntlrLexer.analyse_text(text) and \
            re.search(r'^\s*language\s*=\s*Python\s*;', text, re.M)


class AntlrJavaLexer(DelegatingLexer):
    """
    `ANTLR`_ with Java Target

    .. versionadded:: 1.
    """

    name = 'ANTLR With Java Target'
    aliases = ['antlr-java']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrJavaLexer, self).__init__(JavaLexer, AntlrLexer,
                                             **options)

    def analyse_text(text):
        # Antlr language is Java by default
        return AntlrLexer.analyse_text(text) and 0.9


class Antlr4JavaLexer(DelegatingLexer):
    """
    `ANTLR 4`_ with Java Target

    .. versionadded:: 2.2.1
    """

    name = 'ANTLR 4 With Java Target'
    aliases = ['antlr4-java']
    filenames = ['*.G4', '*.g4']

    def __init__(self, **options):
        super(Antlr4JavaLexer, self).__init__(JavaLexer, Antlr4Lexer,
                                             **options)

    def analyse_text(text):
        # Antlr language is Java by default
        return Antlr4Lexer.analyse_text(text) and 0.9


class AntlrRubyLexer(DelegatingLexer):
    """
    `ANTLR`_ with Ruby Target

    .. versionadded:: 1.1
    """

    name = 'ANTLR With Ruby Target'
    aliases = ['antlr-ruby', 'antlr-rb']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrRubyLexer, self).__init__(RubyLexer, AntlrLexer,
                                             **options)

    def analyse_text(text):
        return AntlrLexer.analyse_text(text) and \
            re.search(r'^\s*language\s*=\s*Ruby\s*;', text, re.M)


class AntlrPerlLexer(DelegatingLexer):
    """
    `ANTLR`_ with Perl Target

    .. versionadded:: 1.1
    """

    name = 'ANTLR With Perl Target'
    aliases = ['antlr-perl']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        super(AntlrPerlLexer, self).__init__(PerlLexer, AntlrLexer,
                                             **options)

    def analyse_text(text):
        return AntlrLexer.analyse_text(text) and \
            re.search(r'^\s*language\s*=\s*Perl5\s*;', text, re.M)


class AntlrActionScriptLexer(DelegatingLexer):
    """
    `ANTLR`_ with ActionScript Target

    .. versionadded:: 1.1
    """

    name = 'ANTLR With ActionScript Target'
    aliases = ['antlr-as', 'antlr-actionscript']
    filenames = ['*.G', '*.g']

    def __init__(self, **options):
        from pygments.lexers.actionscript import ActionScriptLexer
        super(AntlrActionScriptLexer, self).__init__(ActionScriptLexer,
                                                     AntlrLexer, **options)

    def analyse_text(text):
        return AntlrLexer.analyse_text(text) and \
            re.search(r'^\s*language\s*=\s*ActionScript\s*;', text, re.M)


class TreetopBaseLexer(RegexLexer):
    """
    A base lexer for `Treetop <http://treetop.rubyforge.org/>`_ grammars.
    Not for direct use; use TreetopLexer instead.

    .. versionadded:: 1.6
    """

    tokens = {
        'root': [
            include('space'),
            (r'require[ \t]+[^\n\r]+[\n\r]', Other),
            (r'module\b', Keyword.Namespace, 'module'),
            (r'grammar\b', Keyword, 'grammar'),
        ],
        'module': [
            include('space'),
            include('end'),
            (r'module\b', Keyword, '#push'),
            (r'grammar\b', Keyword, 'grammar'),
            (r'[A-Z]\w*(?:::[A-Z]\w*)*', Name.Namespace),
        ],
        'grammar': [
            include('space'),
            include('end'),
            (r'rule\b', Keyword, 'rule'),
            (r'include\b', Keyword, 'include'),
            (r'[A-Z]\w*', Name),
        ],
        'include': [
            include('space'),
            (r'[A-Z]\w*(?:::[A-Z]\w*)*', Name.Class, '#pop'),
        ],
        'rule': [
            include('space'),
            include('end'),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r"'(\\\\|\\'|[^'])*'", String.Single),
            (r'([A-Za-z_]\w*)(:)', bygroups(Name.Label, Punctuation)),
            (r'[A-Za-z_]\w*', Name),
            (r'[()]', Punctuation),
            (r'[?+*/&!~]', Operator),
            (r'\[(?:\\.|\[:\^?[a-z]+:\]|[^\\\]])+\]', String.Regex),
            (r'([0-9]*)(\.\.)([0-9]*)',
             bygroups(Number.Integer, Operator, Number.Integer)),
            (r'(<)([^>]+)(>)', bygroups(Punctuation, Name.Class, Punctuation)),
            (r'\{', Punctuation, 'inline_module'),
            (r'\.', String.Regex),
        ],
        'inline_module': [
            (r'\{', Other, 'ruby'),
            (r'\}', Punctuation, '#pop'),
            (r'[^{}]+', Other),
        ],
        'ruby': [
            (r'\{', Other, '#push'),
            (r'\}', Other, '#pop'),
            (r'[^{}]+', Other),
        ],
        'space': [
            (r'[ \t\n\r]+', Whitespace),
            (r'#[^\n]*', Comment.Single),
        ],
        'end': [
            (r'end\b', Keyword, '#pop'),
        ],
    }


class TreetopLexer(DelegatingLexer):
    """
    A lexer for `Treetop <http://treetop.rubyforge.org/>`_ grammars.

    .. versionadded:: 1.6
    """

    name = 'Treetop'
    aliases = ['treetop']
    filenames = ['*.treetop', '*.tt']

    def __init__(self, **options):
        super(TreetopLexer, self).__init__(RubyLexer, TreetopBaseLexer, **options)


class EbnfLexer(RegexLexer):
    """
    Lexer for `ISO/IEC 14977 EBNF
    <http://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form>`_
    grammars.

    .. versionadded:: 2.0
    """

    name = 'EBNF'
    aliases = ['ebnf']
    filenames = ['*.ebnf']
    mimetypes = ['text/x-ebnf']

    tokens = {
        'root': [
            include('whitespace'),
            include('comment_start'),
            include('identifier'),
            (r'=', Operator, 'production'),
        ],
        'production': [
            include('whitespace'),
            include('comment_start'),
            include('identifier'),
            (r'"[^"]*"', String.Double),
            (r"'[^']*'", String.Single),
            (r'(\?[^?]*\?)', Name.Entity),
            (r'[\[\]{}(),|]', Punctuation),
            (r'-', Operator),
            (r';', Punctuation, '#pop'),
            (r'\.', Punctuation, '#pop'),
        ],
        'whitespace': [
            (r'\s+', Text),
        ],
        'comment_start': [
            (r'\(\*', Comment.Multiline, 'comment'),
        ],
        'comment': [
            (r'[^*)]', Comment.Multiline),
            include('comment_start'),
            (r'\*\)', Comment.Multiline, '#pop'),
            (r'[*)]', Comment.Multiline),
        ],
        'identifier': [
            (r'([a-zA-Z][\w \-]*)', Keyword),
        ],
    }
