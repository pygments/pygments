"""
    pygments.lexers.prov
    ~~~~~~~~~~~~~~~~~~~~

    Lexer for PROV-N, the W3C provenance notation.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, default, include, words
from pygments.token import Comment, Keyword, Literal, Name, Number, \
    Operator, Punctuation, String, Whitespace

__all__ = ['ProvNLexer']

# The XML NameStartChar ranges, without ":". PN_CHARS adds the NameChar ranges,
# and PN_CHARS_BASE drops "_".
_NCNAME_START_CHARS = (
    '\x41-\x5a'  # [A-Z]
    '\x5f'  # "_"
    '\x61-\x7a'  # [a-z]
    '\xc0-\xd6'
    '\xd8-\xf6'
    '\xf8-˿'
    'Ͱ-ͽ'
    'Ϳ-῿'
    '‌-‍'
    '⁰-↏'
    'Ⰰ-⿯'
    '、-퟿'
    '豈-﷏'
    'ﷰ-�'
    '\U00010000-\U000effff'
)
_PN_CHARS = _NCNAME_START_CHARS + (
    '\\-'  # "-", escaped so it is not a range
    '\x30-\x39'  # [0-9]
    '\xb7'
    '̀-ͯ'
    '‿-⁀'
)
_PN_CHARS_ESC = "='(),:;[]-."
_PN_CHARS_OTHER = '/@~&+*?#$!'
_PN_CHARS_BASE = _NCNAME_START_CHARS.replace('_', '')
_PN_PREFIX = rf'[{_PN_CHARS_BASE}](?:[{_PN_CHARS}.]*[{_PN_CHARS}])?'

_LANGTAG = r'[A-Za-z]+(?:-[A-Za-z0-9]+)*'
_DATETIME = (
    r'-?[0-9]{4,}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}'
    r'(?:\.[0-9]+)?(?:Z|[+-][0-9]{2}:[0-9]{2})?'
)

# The 17 expression names and the bare mentionOf, keywords before "(".
_EXPRESSIONS = (
    'entity',
    'activity',
    'agent',
    'wasGeneratedBy',
    'used',
    'wasInvalidatedBy',
    'wasStartedBy',
    'wasEndedBy',
    'wasInformedBy',
    'wasAttributedTo',
    'wasAssociatedWith',
    'actedOnBehalfOf',
    'wasDerivedFrom',
    'wasInfluencedBy',
    'alternateOf',
    'specializationOf',
    'hadMember',
    'mentionOf',
)
_STRUCTURAL = ('document', 'endDocument', 'endBundle')

_PN_OTHERS = re.escape(_PN_CHARS_OTHER)
_PERCENT = r'%[0-9A-Fa-f]{2}'
_ESC = rf'\\[{re.escape(_PN_CHARS_ESC)}]'
_LOCAL_START = rf'[{_NCNAME_START_CHARS}0-9{_PN_OTHERS}]|{_PERCENT}|{_ESC}'
_LOCAL_CONT = rf'[{_PN_CHARS}.{_PN_OTHERS}]|{_PERCENT}|{_ESC}'
_LOCAL_LAST = rf'[{_PN_CHARS}{_PN_OTHERS}]|{_PERCENT}|{_ESC}'
_PN_LOCAL = rf'(?:{_LOCAL_START})(?:(?:{_LOCAL_CONT})*(?:{_LOCAL_LAST}))?'

_LINE_COMMENT = r'//[^\n\r]*'
_BLOCK_COMMENT = r'/\*(?:[^*]|\*(?!/))*\*/'

# Lookaheads. A name ends where no further local-part character can follow,
# and a bare word is not a prefix when a colon follows. Trivia is whitespace
# and comments; each alternative has one way to match, so a failed lookahead
# backtracks in linear time.
_NAME_END = rf'(?!{_LOCAL_LAST}|\.+(?:{_LOCAL_LAST}))'
_WORD_END = _NAME_END + '(?!:)'
_TRIVIA = rf'(?:[ \t\r\n]|{_LINE_COMMENT}(?![^\n\r])|{_BLOCK_COMMENT})*'
_BEFORE_PAREN = rf'(?={_TRIVIA}\()'
_BEFORE_EQUALS = rf'(?={_TRIVIA}=)'

# A qualified name as prefix, colon and optional local part, or a bare local
# name; the groups take Name.Namespace, Punctuation and the construct's class
# twice.
_QNAME = rf'(?:({_PN_PREFIX})(:)({_PN_LOCAL})?|({_PN_LOCAL}))'
_IRI = r'<(?:[^<>"{}|^`\\\x00-\x20]|\\[^\x00-\x20])*>?'


class ProvNLexer(RegexLexer):
    """
    Lexer for PROV-N, the notation of the W3C PROV data model.
    """

    name = 'PROV-N'
    aliases = ['provn', 'prov-n']
    filenames = ['*.provn']
    mimetypes = ['text/provenance-notation']
    url = 'https://www.w3.org/TR/prov-n/'
    version_added = '2.22'

    tokens = {
        # Outside brackets: bundle, document, endDocument and endBundle, and
        # the prefix and default declarations.
        'root': [
            (rf'bundle{_WORD_END}', Keyword, 'bundle_name'),
            (words(_STRUCTURAL, suffix=_WORD_END), Keyword),
            (rf'prefix{_WORD_END}', Keyword.Declaration, 'prefix_name'),
            (rf'default{_WORD_END}', Keyword.Declaration),
            include('common'),
        ],
        'trivia': [
            (r'[ \t\r\n]+', Whitespace),
            (_LINE_COMMENT, Comment.Single),
            (_BLOCK_COMMENT, Comment.Multiline),
            # An unterminated comment runs to the end of the text.
            (r'/\*[\s\S]*', Comment.Multiline),
        ],
        # Root includes it. Every opener pushes it and every closer pops it,
        # so it holds inside ( ), { } and [ ].
        'common': [
            include('trivia'),
            (_DATETIME, Literal.Date),
            (r'"""(?:\\[\s\S]|"(?!"")|[^"\\])*"""', String.Double,
             'string_tail'),
            # An unterminated string or quoted name runs to the end of its
            # line, a long string to the end of the text, and an IRI to the
            # first character in \x00-\x20 (a space included), so a bad file
            # lexes in linear time.
            (r'"""[\s\S]*', String.Double),
            (r'"(?:\\.|[^"\\\n\r])*"', String.Double, 'string_tail'),
            (r'"(?:\\.|[^"\\\n\r])*', String.Double),
            (r"'(?:\\.|[^'\\\n\r])*'?", String.Single),
            (_IRI, String.Other),
            (r'%%', Operator, 'datatype'),
            (words(_EXPRESSIONS, suffix=_NAME_END + _BEFORE_PAREN), Keyword),
            (rf'(prov)(:)(mentionOf){_NAME_END}{_BEFORE_PAREN}',
             bygroups(Name.Namespace, Punctuation, Keyword)),
            (_QNAME + _NAME_END + _BEFORE_PAREN,
             bygroups(Name.Namespace, Punctuation, Name.Function,
                      Name.Function)),
            (_QNAME + _NAME_END + _BEFORE_EQUALS,
             bygroups(Name.Namespace, Punctuation, Name.Attribute,
                      Name.Attribute)),
            (rf'-[0-9]+|[0-9]+{_NAME_END}', Number.Integer),
            (_QNAME, bygroups(Name.Namespace, Punctuation, Name, Name)),
            (r'-', Keyword.Constant),
            (r'[(\[{]', Punctuation, 'common'),
            (r'[)\]}]', Punctuation, '#pop'),
            (r'[,;=]', Punctuation),
        ],
        # The name after "bundle" is a name, whatever it spells.
        'bundle_name': [
            include('trivia'),
            (_QNAME, bygroups(Name.Namespace, Punctuation, Name, Name),
             '#pop'),
            default('#pop'),
        ],
        'prefix_name': [
            include('trivia'),
            (_PN_PREFIX, Name.Namespace, '#pop'),
            default('#pop'),
        ],
        # A language tag follows a string, with trivia allowed between them.
        'string_tail': [
            include('trivia'),
            (rf'@{_LANGTAG}', Name.Decorator, '#pop'),
            default('#pop'),
        ],
        'datatype': [
            include('trivia'),
            (_QNAME,
             bygroups(Name.Namespace, Punctuation, Keyword.Type, Keyword.Type),
             '#pop'),
            default('#pop'),
        ],
    }
