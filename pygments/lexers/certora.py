"""
    pygments.lexers.cvl
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Certora Verification Language.

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words
from pygments.token import Comment, Keyword, Operator, Punctuation, Text, Name, Number, String, Whitespace

from pygments.lexers.solidity import SolidityLexer

__all__ = ['CVLLexer']

x = (words(("mathint", "calldataarg", "storage", "env", "method"), suffix=r'\b'),
      Keyword.Type)

class CVLLexer(RegexLexer):
    """
    For CVL source code.  Just do comments and highlight keywords
    """

    name      = 'Certora'
    aliases   = ['certora', 'cvl', 'spec']
    filenames = ['*.spec']
    mimetypes = []

    tokens = {
        'root': [
            (r'/\*', Comment.Multiline, 'comment'),
            (r'//.*?$', Comment.Singleline),
            (r'\s+', Whitespace),
            (SolidityLexer.datatype,Keyword.Type),
#            (words(("mathint", "calldataarg", "storage", "env", "method"), suffix=r'\b'),
#                Keyword.Type)
            (words(("sort", "mapping", "ghost", "definition", "axiom",
                    "hook", "Sload", "Sstore", "Create", "STORAGE", 
                    "ALWAYS", "CONSTANT", "PER_CALLEE_CONSTANT", "NONDET", "HAVOC_ECF", "HAVOC_ALL", "AUTO", "DISPATCHER",
                    "UNRESOLVED", "ALL",
                    "norevert", "withrevert", "whole", "dontsummarize",
                    "forall", "exists", "true", "false", "pragma", "specify",
                    "rule", "function", "indexed", "returns", "return",
                    "envfree", "havoc", "assuming", "require", "static_require",
                    "requireInvariant", "assert",
                    "call", "invariant", "preserved", "static_assert", "methods",
                    "events", "description", "good_description", "filtered",
                    "reset_storage", "invoke", "sinvoke", "invoke_fallback",
                    "invoke_whole", "if", "else", "as", "using", "import", "use",
                    "builtin", "override")),
                Keyword),
            (words((">", "<", "<=", ">=", "==", "!=",
                    "+", "-", "*", "/", "%", "^",
                    "!", "||", "&&", "=>", "<=>", "&", "|", "xor", "~",
                    "<<", ">>", ">>>")),
                Operator),
            (r'[=,:;{}\[\]()?.]', Punctuation),
            (r'[A-Za-z_][A-Za-z_0-9]*', Name),
            (r'[0-9]+', Number.Integer),
            (r'0x[0-9A-Fa-f]+', Number.Hex),
            (r'"[^"]*"', String.Double),
            (words(("->", "in", "at", "with", "@", "old", "new")), Keyword),
            (r'.', Text)
        ],

        'comment': [
            (r'[^*/]', Comment.Multiline),
            (r'/\*',  Comment.Multiline, '#push'),
            (r'\*/',  Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
    }

