"""
    pygments.lexers.soar
    ~~~~~~~~~~~~~~~~~~~~

    Pygments lexers for Soar.

    :copyright: Copyright 2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words, bygroups
from pygments.token import Comment, Operator, Keyword, Name, \
    Number, Punctuation, Whitespace, Literal

__all__ = ['SoarLexer']


class SoarLexer(RegexLexer):

    """
    Lexer for Soar
    """

    name = 'Soar'
    aliases = ['soar', 'Soar']
    filenames = ['*.soar']
    url = "https://soar.eecs.umich.edu/soar_manual/03_SyntaxOfSoarPrograms/#grammar-of-soar-productions"
    version_added = '2.18'

    keyword_types = (words((
        # This will never be triggered due to the first match via bygroups.
        'sp',
        'state'
    )), Keyword.Reserved)

    builtin_macros = (words((
        'abs',
        'atan2',
        'capitalize-symbol',
        'compute-heading',
        'compute-range',
        'concat',
        'cos',
        'dc',
        'deep-copy',
        'div',
        'dont-learn',
        'exec',
        'float',
        'force-learn',
        'ifeq',
        'int',
        'link-stm-to-ltm',
        'make-constant-symbol',
        'max',
        'min',
        'mod',
        'rand-float',
        'rand-int',
        'round-off',
        'round-off-heading',
        'sin',
        'size',
        'sqrt',
        'strlen',
        'timestamp',
        'trim',
    )), Name.Function)

    builtins_functions = (words((
        'crlf',
        'halt',
        'interrupt',
        'log',
        'wait',
        'write',
        'cmd',
        'load',
        'file',
        'source'
    )), Name.Builtin)

    tokens = {
        'root': [
            (r'\s+', Whitespace),  # whitespace
            (r'(^sp|gp)(\s)(\{)([\S]+)',
             bygroups(Keyword.Reserved, Whitespace, Punctuation, Name.Function)),
            keyword_types,
            builtins_functions,
            builtin_macros,
            (r'(#|").*', Comment.Single),  # single-line comments or docstring
            (r':[a-z-]+', Name.Attribute),
            (r'-->', Punctuation),  # the arrow
            (r'(\()([A-Za-z]\d+)',
                bygroups(Punctuation, Name)),  # identifier without <>
            (r'\{|\}|\(|\)', Punctuation),
            (r'<[^>]+>', Name),  # variable names in <>
            (r'\^[^\s]+', Name.Variable),  # any path like ^test.<test>.any
            # operators and other symbols
            (r'(\-|\+|=|<>|[{}])', Operator),
            (r'(<=>|<>|<=|>=|<|>)', Operator),  # mathematical predicates
            (r'(@|!@|@+|@-)', Operator),  # special predicates for LTI links
            (r'(\d+\.\d*|\.\d+|\d+)', Number),  # numbers
            (r'(\b[a-zA-Z][a-zA-Z0-9_\-\*]*)', Literal),  # identifiers
            (r'\|[^|]*\|', Literal.String),  # pipe-delimited strings
        ],
    }
