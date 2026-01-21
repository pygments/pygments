from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *

__all__ = ['FIRRTLLexer']

'''
    pygments.lexers.firrtl
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for FIRRTL, output from SiFive's Chisel.
    
    :copyright: Copyright 2006-2019, the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
'''
class FIRRTLLexer(RegexLexer):

    name = 'FIRRTLLexer'
    filenames = ['*.fir']

    declarations = words(('circuit', 'module', 'input', 'output', 'wire', 'reg', 'node'), prefix = r'\b', suffix = r'\b') # keywords for declaration
    other_keywords = words(('flip', 'skip', 'is', 'invalid', 'when', 'else'), prefix = r'\b', suffix = r'\b') # other various keywords
    types = words(('UInt', 'SInt', 'Clock'), prefix = r'\b', suffix = r'\b') # allowed types in FIRRTL.  Might need to add Reset if becomes new type

    arithmetic = words(('add', 'sub', 'mul', 'div', 'rem'), prefix = r'\b', suffix = r'\b') # arthimetic operators
    typecast = words(('asUInt', 'asSInt', 'asClock'), prefix = r'\b', suffix = r'\b') # type conversions
    comparisons = words(('lt', 'leq', 'gt', 'geq', 'eq', 'neq'), prefix = r'\b', suffix = r'\b') # comparisons
    logic = words(('shl', 'shr', 'dshl', 'dshr', 'neg', 'not', 'and', 'or', 'xor', 'andr', 'orr', 'xorr'), prefix = r'\b', suffix = r'\b') # bitwise/logic operators
    other = words(('pad', 'cvt', 'cat', 'bits', 'head', 'tail'), prefix = r'\b', suffix = r'\b') # other operators
    
    tokens = {
        'root' : [
            (r';.*', Token.Comment.Single, 'comment'), # single-line comment
            (r'"', Token.Literal.String, 'string'), # start of string
            (r'@\[', Token.Comment.Special, 'info'), # start of automated comment
            (r'([A-Za-z_][A-Za-z0-9_]*)(\.)([A-Za-z0-9]*)', bygroups(Token.Name, Token.Punctuation, Token.Name)), # used to deal to calling an attribute of bundle, even if the name starts with a digit
            (r'([A-Za-z0-9_]+)([ ]+)(<=|=>|<-|->|=)([ ]+)([A-Za-z0-9_]+)', bygroups(Token.Name, Token.Whitespace, Token.Operator, Token.Whitespace, Token.Name)), # connection
            (declarations, Token.Keyword.Declaration),
            (other_keywords, Token.Keyword),
            (types, Token.Keyword.Type),
            (arithmetic, Token.Operator.Word),
            (typecast, Token.Operator.Word),
            (comparisons, Token.Operator.Word),
            (logic, Token.Operator.Word),
            (other, Token.Operator.Word),
            (r'[A-Za-z_][A-Za-z0-9_]*', Token.Name), # check for sequence of valid characters, make sure first character is not a digit
            (r'(-)?[0-9]+', Token.Literal.Number), # any digits not contained in comment, string, or variable name are numerical literals
            (r'<=|=>|<-|->|=', Token.Operator), # connection operators
            (r'[\{]', Token.Punctuation, 'bundle'),
            (r'[:\(\)\<\>\[\],\.\{\}\$]', Token.Punctuation), # other symbols
            (r'[ \n\t]+', Token.Whitespace) # whitespace
        ],
        'string' : [
            (r'[^"\\]+', Token.Literal.String), # non-escape characters
            (r'\\.', Token.Literal.String.Escape), # escape characters
            (r'"', Token.Literal.String, '#pop') # ending quote
        ],
        'comment' : [
            (r'[^\n]+', Token.Comment.Single), # anything but newline continues comment
            (r'\n', Token.Whitespace, '#pop') # comment ends after newline
        ],
        'bundle' : [
            (r';.*', Token.Comment.Single, 'comment'),
            (r'([A-Za-z0-9_]*)([ ]*)(:)', bygroups(Token.Name, Token.Whitespace, Token.Punctuation)),
            (r'"', Token.Literal.String, 'string'),
            (r'[\{]', Token.Punctuation, 'bundle'),
            (r'(flip)([ ])(flip)', bygroups(Token.Keyword, Token.Whitespace, Token.Name)),
            (r'flip', Token.Keyword),
            (r'UInt|SInt|Clock', Token.Keyword.Type),
            (r'(\-)?[0-9]+', Token.Literal.Number),
            (r'[:\(\)\<\>\[\],\.]', Token.Punctuation),
            (r'[ \n\t]+', Token.Whitespace),
            (r'[\}]', Token.Punctuation, '#pop')
        ],
        'info' : [
            (r'[^\]]+', Token.Comment.Special),
            (r'\]', Token.Comment.Special, '#pop'),
        ]
    }
    
    
