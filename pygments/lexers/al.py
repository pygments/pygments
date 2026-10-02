"""
    pygments.lexers.al
    ~~~~~~~~~~~~~~~~~~

    Lexer for the AL language of Microsoft Dynamics 365 Business Central.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, default, include, words
from pygments.token import Comment, Keyword, Literal, Name, Number, Operator, \
    Punctuation, String, Whitespace

__all__ = ['ALLexer']

# Kinds of top level object introduced as ``<kind> <id> <name>``.
OBJECT_KINDS_WITH_ID = (
    'codeunit', 'enum', 'enumextension', 'page', 'pageextension',
    'permissionset', 'permissionsetextension', 'query', 'report',
    'reportextension', 'table', 'tableextension', 'xmlport',
)

# Kinds of top level object that carry no id.  ``dotnet`` is the odd one out:
# a DotNet alias package is a bare ``dotnet`` with neither an id nor a name.
OBJECT_KINDS_WITHOUT_ID = (
    'controladdin', 'dotnet', 'entitlement', 'interface', 'pagecustomization',
    'profile', 'profileextension',
)

# Object kinds that also name a type, as in ``Customer: Record Customer``.
OBJECT_TYPES = (
    'Codeunit', 'DotNet', 'Enum', 'Interface', 'Page', 'Query', 'Record',
    'Report', 'TestPage', 'XmlPort',
)

# Sections of an object body.  A few of these double as member names, as in
# ``Dict.Keys()``, so the rule that matches them refuses to match after a dot.
SECTION_KEYWORDS = (
    'actions', 'dataset', 'elements', 'fields', 'fieldgroups', 'keys',
    'labels', 'layout', 'rendering', 'requestpage', 'schema', 'views',
)

# Elements of an object body, the operations a page or table extension can
# apply to them, and the parts of a filter or CalcFormula expression.  Unlike
# the sections above these are ordinary words, so they are only recognised
# directly before an opening parenthesis and never after a dot, which keeps
# ``RecRef.Field(1)`` and ``Dict.Values()`` out of it.  Words that remain
# ambiguous even then are left out: ``Modify``, ``Max`` and ``Min`` are far
# more often a method or a helper procedure than a keyword.
ELEMENT_KEYWORDS = (
    'action', 'actionref', 'addafter', 'addbefore', 'addfirst', 'addlast',
    'area', 'assembly', 'average', 'chartpart', 'column', 'const', 'count',
    'cuegroup', 'dataitem', 'exist', 'field', 'fieldattribute', 'fieldelement',
    'fieldgroup', 'filter', 'fixed', 'grid', 'group', 'key', 'label', 'lookup',
    'moveafter', 'movebefore', 'movefirst', 'movelast', 'order', 'part',
    'repeater', 'separator', 'sorting', 'sum', 'systempart', 'tableelement',
    'textattribute', 'textelement', 'type', 'upperlimit', 'usercontrol',
    'value', 'view', 'where',
)

BUILTIN_TYPES = (
    'Action', 'Automation', 'BigInteger', 'BigText', 'Blob', 'Boolean', 'Byte',
    'Char', 'Code', 'Codeunit', 'Database', 'Date', 'DateFormula', 'DateTime',
    'Decimal', 'Dialog', 'Dictionary', 'DotNet', 'Duration', 'Enum', 'ErrorInfo',
    'FieldRef', 'File', 'FilterPageBuilder', 'Guid', 'HttpClient',
    'HttpContent', 'HttpHeaders', 'HttpRequestMessage', 'HttpResponseMessage',
    'InStream', 'Integer', 'Interface', 'JsonArray', 'JsonObject', 'JsonToken',
    'JsonValue', 'KeyRef', 'Label', 'List', 'Media', 'MediaSet', 'ModuleInfo',
    'Notification', 'Option', 'OutStream', 'Page', 'Query', 'Record',
    'RecordId', 'RecordRef', 'Report', 'SecretText', 'SessionSettings',
    'TestPage', 'Text', 'TextBuilder', 'TextConst', 'Time', 'Variant',
    'Version', 'WebServiceActionContext', 'XmlAttribute', 'XmlCData',
    'XmlComment', 'XmlDeclaration', 'XmlDocument', 'XmlElement',
    'XmlNamespaceManager', 'XmlNode', 'XmlNodeList', 'XmlPort',
    'XmlReadOptions', 'XmlText', 'XmlWriteOptions',
)


class ALLexer(RegexLexer):
    """
    For AL source code, the language of Microsoft Dynamics 365 Business
    Central.
    """

    name = 'AL'
    url = 'https://learn.microsoft.com/en-us/dynamics365/business-central/dev-itpro/developer/devenv-programming-in-al'
    aliases = ['al']
    filenames = ['*.al', '*.dal']
    mimetypes = ['text/x-al']
    version_added = '2.21'

    # AL is case insensitive.  Current tooling emits lower case keywords, but
    # code converted from the older C/AL is upper case, and much of the
    # Business Central base application is still written that way.
    flags = re.MULTILINE | re.IGNORECASE

    tokens = {
        'root': [
            # Attributes are always the first thing on a line, which is what
            # tells them apart from array indexing.
            (r'(^[^\S\n]*)(\[)', bygroups(Whitespace, Name.Decorator),
             'attribute'),
            (r'(^[^\S\n]*)(#[^\S\n]*(?:if|elif|else|endif|define|undef'
             r'|pragma|region|endregion)\b.*)',
             bygroups(Whitespace, Comment.Preproc)),
            include('whitespace'),
            include('comments'),
            (words(('namespace', 'using'), suffix=r'\b'), Keyword.Namespace,
             'namespace'),
            # Several object kinds are also type names, as in
            # ``Handler: Codeunit "Sales Post"``.  A declaration is told apart
            # by its id, or, for the kinds that have none, by starting a line.
            (words(OBJECT_KINDS_WITH_ID, suffix=r'\b(?=[^\S\n]+\d)'),
             Keyword.Declaration, 'object'),
            (words(OBJECT_KINDS_WITHOUT_ID, prefix=r'^', suffix=r'\b'),
             Keyword.Declaration, 'object'),
            (words(OBJECT_TYPES, suffix=r'([^\S\n]+)("[^"\n]*"|[a-z_]\w*)'),
             bygroups(Keyword.Type, Whitespace, Name.Class)),
            (r'(procedure|trigger|event)([^\S\n]+)("[^"\n]*"|[a-z_]\w*)',
             bygroups(Keyword.Declaration, Whitespace, Name.Function)),
            include('literals'),
            include('keywords'),
            (r'[a-z_]\w*', Name),
            include('operators'),
        ],
        'whitespace': [
            # Split so that a rule anchored to the start of a line is still
            # tried once the preceding newline has been consumed.
            (r'\n', Whitespace),
            (r'[^\S\n]+', Whitespace),
        ],
        'comments': [
            (r'///.*', Comment.Special),
            (r'//.*', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment'),
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
        'namespace': [
            (r'[^\S\n]+', Whitespace),
            (r'[a-z_]\w*', Name.Namespace),
            (r'\.', Punctuation),
            (r';', Punctuation, '#pop'),
            default('#pop'),
        ],
        'object': [
            include('whitespace'),
            include('comments'),
            (r'\d+', Number.Integer),
            (words(('extends', 'implements', 'customizes'), suffix=r'\b'),
             Keyword),
            (r'"[^"\n]*"|[a-z_]\w*', Name.Class),
            (r',', Punctuation),
            default('#pop'),
        ],
        'attribute': [
            include('whitespace'),
            include('comments'),
            (r'\]', Name.Decorator, '#pop'),
            (words(('false', 'true'), suffix=r'\b'), Keyword.Constant),
            (r'[a-z_]\w*', Name.Decorator),
            include('literals'),
            # ``[EventSubscriber(ObjectType::Table, ...)]`` and its like.
            (r'::', Operator),
            (r'[(),.]', Punctuation),
            default('#pop'),
        ],
        'literals': [
            (r"@'", String.Single, 'verbatim-string'),
            (r"'", String.Single, 'string'),
            (r'"[^"\n]*"', Name.Variable),
            (r'\d+(?:DT|D|T)\b', Literal.Date),
            (r'\d+\.\d+', Number.Float),
            (r'\d+(?:L\b)?', Number.Integer),
        ],
        'string': [
            # There are no backslash escapes; an embedded quote is doubled.
            (r"''", String.Escape),
            (r"'", String.Single, '#pop'),
            (r"[^'\n]+", String.Single),
            (r'\n', Whitespace, '#pop'),
        ],
        'verbatim-string': [
            # Unlike an ordinary string, this one may span lines.
            (r"''", String.Escape),
            (r"'", String.Single, '#pop'),
            (r"[^']+", String.Single),
        ],
        'keywords': [
            (words(('false', 'true'), suffix=r'\b'), Keyword.Constant),
            (words(('and', 'as', 'div', 'in', 'is', 'mod', 'not', 'or', 'xor'),
                   suffix=r'\b'), Operator.Word),
            (words(('array', 'asserterror', 'begin', 'break', 'case', 'do',
                    'downto', 'else', 'end', 'exit', 'for', 'foreach', 'if',
                    'of', 'repeat', 'then', 'to', 'until', 'while', 'with'),
                   suffix=r'\b'), Keyword),
            (words(('event', 'internal', 'local', 'protected', 'runonclient',
                    'temporary', 'var'), suffix=r'\b'), Keyword.Declaration),
            # A keyword never follows a dot; a member of the same name can.
            (words(SECTION_KEYWORDS, prefix=r'(?<![.\w])', suffix=r'\b'),
             Keyword),
            (words(ELEMENT_KEYWORDS, prefix=r'(?<![.\w])',
                   suffix=r'(?=[^\S\n]*\()'), Keyword),
            (words(BUILTIN_TYPES, prefix=r'(?<![.\w])', suffix=r'\b'),
             Keyword.Type),
        ],
        'operators': [
            # ``|``, ``&``, ``@`` and a lone ``*`` are the filter expression
            # operators of a ``where`` or ``filter`` clause; ``?`` is the
            # conditional operator.
            (r'[-+*/]=|:=|<>|<=|>=|::|\.\.|[-+*/<>=?|&@]', Operator),
            (r'[(){}\[\];,.:]', Punctuation),
        ],
    }
