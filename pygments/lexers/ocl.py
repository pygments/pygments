# -*- coding: utf-8 -*-
"""
    pygments.lexers.ocl
    ~~~~~~~~~~~~~~~~~~~

    Lexer for the Object Constraint language.

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include, combined, default, words
from pygments.token import Generic, Text, Keyword, Number, Name, String, Comment, Punctuation, Operator
from pygments.filters import Filter

__all__ = ['OCLLexer']

class OCLLexer(RegexLexer):
    """
    For the `Object Constraint language <https://www.omg.org/spec/OCL/2.4>`_.
    """

    name = 'OCL'
    aliases = ['ocl']
    filenames = ['*.ocl']
    mimetypes = ['text/ocl']

    _keywords = [
        'context',
        'pre',
        'post',
        'inv',
        'init', 
        'body',
        'def',
        'derive',
        'if',
        'then',
        'else',
        'endif',
        'import',
        'package',
        'endpackage',
        'let',
        'in'
    ]

    _keywords_type = [
        'Boolean',
        'Integer', 
        'UnlimitedNatural',
        'Real',
        'String', 
        'OrderedSet', 
        'Tuple',
        'Bag',
        'Set',
        'Sequence',
        'OclInvalid', 
        'OclVoid',
        'TupleType', 
        'OclState',
        'Collection',
        'OclMessage'
    ]

    _functions = [
        # OclAny Operations
        'oclAsSet',
        'oclIsNew',
        'oclIsUndefined',
        'oclIsInvalid',
        'oclAsType',
        'oclIsTypeOf',
        'oclIsKindOf',
        'oclInState',
        'oclType',
        'oclLocale',
        # OclMessage Operations
        'hasReturned',
        'result',
        'isSignalSent',
        'isOperationCall'
        # Real Operations
        'abs',
        'floor',
        'round',
        'max',
        'min',
        'toString',
        # Integer Operations
        'div',
        'mod',
        # String Operations
        'size',
        'substring',
        'concat',
        'toInteger',
        'toReal',
        'toUpperCase',
        'toLowerCase',
        'indexOf',
        'equalsIgnoreCase',
        'at',
        'characters',
        'toBoolean',
        # Collection Operations
        'includes',
        'excludes',
        'count',
        'includesAll',
        'excludesAll',
        'isEmpty',
        'notEmpty',
        'sum',
        'product',
        'selectByKind',
        'selectByType',
        'asBag',
        'asSequence',
        'asOrderedSet',
        'asSet',        
        'flatten',
        # Set Operations
        'union',
        'intersection',
        'including',
        'excluding',
        'symmetricDifference'
        'count',
        # OrderedSet Operations
        'append',
        'prepend',
        'insertAt',
        'subOrderedSet',
        'first',
        'last',
        'reverse',
        # Bag Operations
        # Sequence Operations
        'subSequence',
        # Iterator Expressions
        'any',
        'closure',
        'collect',
        'collectNested',
        'exists',
        'forAll',
        'isUnique',
        'iterate',
        'one',
        'reject',
        'select',
        'sortedBy',
        # Predefined operations
        'allInstances',
        # Controversial
        'average',
        'conformsTo'
    ]

    _operator = [
        'or', 
        'xor',
        'and',
        'not',
        'implies'
    ]

    tokens = {
        'root': [
            (r'\n', Text),
            (r'[^\S\n]+', Text),
            (r'--.*?\n', Comment.Single),
            (r'[]{}:(),;[]', Punctuation),
            (r'\\\n', Text),
            (r'\\', Text),
            (words(_operator, suffix=r'\b'), Operator.Word),
            (r'<>|==|->|<=|>=|\?|[-~+/*%=<>&^|.!]', Operator),
            include('keywords'),
            include('builtins'),
            include('name'),
            (r'("(\\\\|\\"|[^"])*")|(\'(\\\\|\\\'|[^\'])*\')', String),
            include('numbers'),
        ],
        'keywords': [
            (words(_keywords, suffix=r'\b'), Keyword),
            (words(_keywords_type, suffix=r'\b'), Keyword.Type),
            (words(_functions, suffix=r'\b'), Keyword.Pseudo),
        ],
        'builtins': [
            (r'(true|false|null|invalid|self|result|@pre)\b', Name.Builtin.Pseudo),
        ],
        'numbers': [
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'0x[0-9a-f]+', Number.Hex),
            (r'[0-9]+L?', Number.Integer),
        ],
        'name': [
            (r'@[a-zA-Z0-9_.]+', Name.Decorator),
            ('([a-zA-Z$_]|[\u00C0-\u00D6]|[\u00D8-\u00F6]|[\u00F8-\u02FF]|[\u0370-\u037D]|[\u037F-\u1FFF]|[\u200C-\u200D]|[\u2070-\u218F]|[\u2C00-\u2FEF]|[\u3001-\uD7FF]|[\uF900-\uFDCF]|[\uFDF0-\uFFFD])' # Start character
             '([a-zA-Z$_]|[\u00C0-\u00D6]|[\u00D8-\u00F6]|[\u00F8-\u02FF]|[\u0370-\u037D]|[\u037F-\u1FFF]|[\u200C-\u200D]|[\u2070-\u218F]|[\u2C00-\u2FEF]|[\u3001-\uD7FF]|[\uF900-\uFDCF]|[\uFDF0-\uFFFD][0-9])*' # Continuation character
             , Name
             ),
        ],
        }


    def __init__(self, **options):
        RegexLexer.__init__(self, **options)
        self.add_filter(OCLSymbolFilter())
        
class OCLSymbolFilter(Filter):
    symbols = {
        '->'            : '\u27f6',
        '>='            : '≥',
        '<='            : '≤'
    }

    def __init__(self, **options):
        Filter.__init__(self, **options)

    def filter(self, lexer, stream):
        for ttype, value in stream:
            if value in self.symbols:
                yield ttype, self.symbols[value]
            else:
                yield ttype, value