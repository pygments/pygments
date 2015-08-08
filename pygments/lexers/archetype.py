# -*- coding: utf-8 -*-
"""
    pygments.lexers.archetype
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for Archetype-related syntaxes, including:
        ODIN syntax <https://github.com/openEHR/odin>.
        ADL syntax <http://www.openehr.org/releases/trunk/architecture/am/adl2.pdf>.
        cADL sub-syntax of ADL

    For uses of this syntax, see the openEHR archetypes <http://www.openEHR.org/ckm>

    Contributed by Thomas Beale <https://github.com/wolandscat>,
    <https://bitbucket.org/thomas_beale>.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, bygroups, using
from pygments.token import Text, Comment, Name, Literal, Number, String, \
    Punctuation, Keyword, Operator, Generic

__all__ = ['OdinLexer', 'CadlLexer', 'AdlLexer']


class AtomsLexer(RegexLexer):
    """
    Lexer for Values used in ADL and ODIN.

    .. versionadded:: 2.1
    """

    tokens = {
        # ----- pseudo-states for inclusion -----
        'whitespace': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'[ \t]*--.*$', Comment),
        ],
        'archetype_id': [
            (r'[ \t]*([a-zA-Z]\w+(\.[a-zA-Z]\w+)*::)?[a-zA-Z]\w+(-[a-zA-Z]\w+){2}'
             r'\.\w+[\w-]*\.v\d+(\.\d+){,2}((-[a-z]+)(\.\d+)?)?', Name.Decorator),   # archetype id
        ],
        'date_constraints': [
            (r'[Xx?YyMmDdHhSs\d]{2,4}([:-][Xx?YyMmDdHhSs\d]{2}){2}', Literal.Date),  # ISO 8601-based date/time constraints
            (r'(P[YyMmWwDd]+(T[HhMmSs]+)?|PT[HhMmSs]+)/?', Literal.Date),            # ISO 8601-based duration constraints + optional trailing slash
        ],
        'ordered_values': [
            (r'\d{4}-\d{2}-\d{2}T?', Literal.Date),                                  # ISO 8601 date with optional 'T' ligature
            (r'\d{2}:\d{2}:\d{2}(\.\d+)?([+-]\d{4}|Z)?', Literal.Date),              # ISO 8601 time
            (r'P((\d*(\.\d+)?[YyMmWwDd]){1,3}(T(\d*(\.\d+)?[HhMmSs]){,3})?|'
             r'T(\d*(\.\d+)?[HhMmSs]){,3})', Literal.Date),                          # ISO 8601 duration
            (r'[+-]?(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+', Number.Float),
            (r'[+-]?(\d+)*\.\d+%?', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'[+-]?\d+%?', Number.Integer),
        ],
        'values': [
            include('ordered_values'),
            (r'([Tt]rue|[Ff]alse)', Literal),
            (r'"', String, 'string'),
            (r"'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", String.Char),
            (r'[a-z][a-z0-9+.-]*:', Literal, 'uri'),
            (r'(\[)(\w[\w-]*(?:\([^)\n]+\))?)(::)(\w[\w-]*)(\])',
             bygroups(Punctuation, Name.Decorator, Punctuation, Name.Decorator, Punctuation)),  # term code
            (r'\|', Punctuation, 'interval'),
            (r'\.\.\.', Punctuation),                                                # list continuation
        ],
        'constraint_values': [
            (r'(\[)(\w[\w-]*(?:\([^)\n]+\))?)(::)',
             bygroups(Punctuation, Name.Decorator, Punctuation), 'adl14_code_constraint'),
            (r'(\d*)(\|)(\[\w[\w-]*::\w[\w-]*\])((?:[,;])?)',
             bygroups(Number, Punctuation, Name.Decorator, Punctuation)),  # ADL 1.4 ordinal constraint
            include('date_constraints'),
            include('values'),
        ],

        # ----- real states -----
        'string': [
            ('"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|'
             r'u[a-fA-F0-9]{4}|U[a-fA-F0-9]{8}|[0-7]{1,3})', String.Escape),
            (r'[^\\"]+', String),                                   # all other characters
            (r'\\', String),                                        # stray backslash
        ],
        'uri': [
            (r'[,>\s]', Punctuation, '#pop'),                       # effective URI terminators
            (r'[^>\s,]*', Literal),
        ],
        'interval': [
            (r'\|', Punctuation, '#pop'),
            include('ordered_values'),
            (r'\.\.', Punctuation),
            (r'[<>=] *', Punctuation),
            (r'\+/-', Punctuation),                                 # handle +/-
            (r'\s*', Text),
        ],
        'any_code': [
            include('archetype_id'),
            (r'[a-z_]\w*[0-9.]+(@[^\]]+)?', Name.Decorator),        # if it is a code
            (r'[a-z_]\w*', Name.Class),                             # if it is tuple with attribute names
            (r'[0-9]+', Text),                                      # if it is an integer, i.e. Xpath child index
            (r'\|', Punctuation, 'code_rubric'),
            (r'\]', Punctuation, '#pop'),
            (r'\s*,\s*', Punctuation),                              # handle use_archetype statement
        ],
        'code_rubric': [
            (r'\|', Punctuation, '#pop'),
            (r'[^|]*', String),
        ],
        'adl14_code_constraint': [
            (r'\]', Punctuation, '#pop'),
            (r'\|', Punctuation, 'code_rubric'),
            (r'(\w[\w-]*)([;,]?)', bygroups(Name.Decorator, Punctuation)),
            include('whitespace'),
        ],
    }


class OdinLexer(AtomsLexer):
    """
    Lexer for ODIN syntax.

    .. versionadded:: 2.1
    """
    name = 'ODIN'
    aliases = ['odin']
    filenames = ['*.odin']
    mimetypes = ['text/odin']

    tokens = {
        'path': [
            (r'>', Punctuation, '#pop'),
            (r'[a-z_]\w*', Name.Class),                         # attribute name
            (r'/', Punctuation),
            (r'\[', Punctuation, 'key'),
            (r'\s*,\s*', Punctuation, '#pop'),
            (r'\s+', Text, '#pop'),
        ],
        'key': [
            include('values'),
            (r'\]', Punctuation, '#pop'),
        ],
        'type_cast': [
            (r'\)', Punctuation, '#pop'),
            (r'[^)]*',  Name.Class),
        ],
        'root': [
            include('whitespace'),
            (r'([Tt]rue|[Ff]alse)', Literal),
            include('values'),
            (r'/', Punctuation, 'path'),                        # x-ref path
            (r'\[', Punctuation, 'key'),                        # x-ref path starting with key
            (r'[a-z_]\w*', Name.Class),                         # attribute name
            (r'=', Operator),
            (r'\(', Punctuation, 'type_cast'),
            (r',', Punctuation),
            (r'<', Punctuation),
            (r'>', Punctuation),
            (r';', Punctuation),
        ],
    }


class CadlLexer(AtomsLexer):
    """
    Lexer for cADL syntax.

    .. versionadded:: 2.1
    """
    name = 'cADL'
    aliases = ['cadl']
    filenames = ['*.cadl']

    tokens = {
        'path': [
            (r'[a-z_]\w*', Name.Class),                                 # attribute name
            (r'/', Punctuation),
            (r'\[', Punctuation, 'any_code'),
            (r'\s*', Punctuation, '#pop'),
        ],
        'root': [
            include('whitespace'),
            (r'(cardinality|existence|occurrences|group|include|exclude|'
             r'allow_archetype|use_archetype|use_node)\W', Keyword.Type),
            (r'(and|or|not|there_exists|xor|implies|for_all)\W', Keyword.Type),
            (r'(after|before|closed)\W', Keyword.Type),
            (r'(not)\W', Operator),
            (r'(matches|is_in)\W', Operator),
            (u'(\u2208|\u2209)', Operator),                             # is_in / not is_in char
            (u'(\u2203|\u2204|\u2200|\u2227|\u2228|\u22BB|\223C)',
             Operator),                                                 # there_exists / not there_exists / for_all / and / or
            (r'(\{)(\s*/[^}]+/\s*)(\})',
             bygroups(Punctuation, String.Regex, Punctuation)),         # regex in slot or as string constraint
            (r'(\{)(\s*\^[^}]+\^\s*)(\})',
             bygroups(Punctuation, String.Regex, Punctuation)),         # regex in slot or as string constraint
            (r'/', Punctuation, 'path'),
            (r'(\{)((?:\d+\.\.)?(?:\d+|\*))((?:\s*;\s*(?:ordered|unordered|unique)){,2})(\})',
             bygroups(Punctuation, Number, Number, Punctuation)),       # for cardinality etc
            (r'\[\{', Punctuation),                                     # [{ is start of a tuple value
            (r'\}\]', Punctuation),
            (r'\{', Punctuation),
            (r'\}', Punctuation),
            include('constraint_values'),
            (r'[A-Z]\w+(<[A-Z]\w+([A-Za-z_<>]*)?>)?',  Name.Class),     # type name
            (r'[a-z_]\w*', Name.Class),                                 # attribute name
            (r'\[', Punctuation, 'any_code'),
            (r'(~|//|\\\\|\+|-|/|\*|\^|!=|=|<=|>=|<|>]?)', Operator),
            (r'\(', Punctuation),
            (r'\)', Punctuation),
            (r',', Punctuation),                                        # for lists of values
            (r'"', String, 'string'),
            (r';', Punctuation),                                        # for assumed value
        ],
    }


class AdlLexer(AtomsLexer):
    """
    Lexer for ADL syntax.

    .. versionadded:: 2.1
    """

    name = 'ADL'
    aliases = ['adl']
    filenames = ['*.adl', '*.adls', '*.adlf', '*.adlx']

    tokens = {
        'whitespace': [
            (r'\s*\n', Text),                                           # blank line ends
            (r'^[ \t]*--.*$', Comment),                                 # comment-only line
        ],
        'odin_section': [
            # repeating the following two rules from the root state enable multi-line strings that start in the
            # first column to be dealt with
            (r'^(language|description|ontology|terminology|annotations|'
             r'component_terminologies|revision_history)[ \t]*\n', Generic.Heading),
            (r'^(definition)[ \t]*\n', Generic.Heading, 'cadl_section'),
            (r'^([ \t]*|[ \t]+.*)\n', using(OdinLexer)),
            (r'^([^"]*")(>[ \t]*\n)', bygroups(String, Punctuation)),
            (r'^.*\n', String),
            ('', Text, '#pop'),
        ],
        'cadl_section': [
            (r'^([ \t]*|[ \t]+.*)\n', using(CadlLexer)),
            ('', Text, '#pop'),
        ],
        'rules_section': [
            (r'^[ \t]+.*\n', using(CadlLexer)),
            ('', Text, '#pop'),
        ],
        'metadata': [
            (r'\)', Punctuation, '#pop'),
            (r';', Punctuation),
            (r'([Tt]rue|[Ff]alse)', Literal),
            (r'\d+(\.\d+)*', Literal),                                  # numbers and version ids
            (r'(\d|[a-fA-F])+(-(\d|[a-fA-F])+){3,}', Literal),          # Guids
            (r'\w+', Name.Class),
            (r'"', String, 'string'),
            (r'=', Operator),
            (r'[ \t]*', Text),
        ],
        'root': [
            (r'^(archetype|template|template_overlay|operational_template|'
             r'speciali[sz]e)', Generic.Heading),
            (r'^(language|description|ontology|terminology|annotations|'
             r'component_terminologies|revision_history)[ \t]*\n',
             Generic.Heading, 'odin_section'),
            (r'^(definition)[ \t]*\n', Generic.Heading, 'cadl_section'),
            (r'^(rules)[ \t]*\n', Generic.Heading, 'rules_section'),
            include('archetype_id'),
            (r'[ \t]*\(', Punctuation, 'metadata'),
            include('whitespace'),
        ],
    }
