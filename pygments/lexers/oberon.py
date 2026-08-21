"""
    pygments.lexers.oberon
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Oberon family languages.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, include, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

__all__ = ['ActiveOberonLexer', 'ComponentPascalLexer']


class ComponentPascalLexer(RegexLexer):
    """
    For Component Pascal source code.
    """
    name = 'Component Pascal'
    aliases = ['componentpascal', 'cp']
    filenames = ['*.cp', '*.cps']
    mimetypes = ['text/x-component-pascal']
    url = 'https://blackboxframework.org'
    version_added = '2.1'

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'root': [
            include('whitespace'),
            include('comments'),
            include('punctuation'),
            include('numliterals'),
            include('strings'),
            include('operators'),
            include('builtins'),
            include('identifiers'),
        ],
        'whitespace': [
            (r'\n+', Text),  # blank lines
            (r'\s+', Text),  # whitespace
        ],
        'comments': [
            (r'\(\*([^$].*?)\*\)', Comment.Multiline),
            # TODO: nested comments (* (* ... *) ... (* ... *) *) not supported!
        ],
        'punctuation': [
            (r'[()\[\]{},.:;|]', Punctuation),
        ],
        'numliterals': [
            (r'[0-9A-F]+X\b', Number.Hex),                 # char code
            (r'[0-9A-F]+[HL]\b', Number.Hex),              # hexadecimal number
            (r'[0-9]+\.[0-9]+E[+-][0-9]+', Number.Float),  # real number
            (r'[0-9]+\.[0-9]+', Number.Float),             # real number
            (r'[0-9]+', Number.Integer),                   # decimal whole number
        ],
        'strings': [
            (r"'[^\n']*'", String),  # single quoted string
            (r'"[^\n"]*"', String),  # double quoted string
        ],
        'operators': [
            # Arithmetic Operators
            (r'[+-]', Operator),
            (r'[*/]', Operator),
            # Relational Operators
            (r'[=#<>]', Operator),
            # Dereferencing Operator
            (r'\^', Operator),
            # Logical AND Operator
            (r'&', Operator),
            # Logical NOT Operator
            (r'~', Operator),
            # Assignment Symbol
            (r':=', Operator),
            # Range Constructor
            (r'\.\.', Operator),
            (r'\$', Operator),
        ],
        'identifiers': [
            (r'([a-zA-Z_$][\w$]*)', Name),
        ],
        'builtins': [
            (words((
                'ANYPTR', 'ANYREC', 'BOOLEAN', 'BYTE', 'CHAR', 'INTEGER', 'LONGINT',
                'REAL', 'SET', 'SHORTCHAR', 'SHORTINT', 'SHORTREAL'
                ), suffix=r'\b'), Keyword.Type),
            (words((
                'ABS', 'ABSTRACT', 'ARRAY', 'ASH', 'ASSERT', 'BEGIN', 'BITS', 'BY',
                'CAP', 'CASE', 'CHR', 'CLOSE', 'CONST', 'DEC', 'DIV', 'DO', 'ELSE',
                'ELSIF', 'EMPTY', 'END', 'ENTIER', 'EXCL', 'EXIT', 'EXTENSIBLE', 'FOR',
                'HALT', 'IF', 'IMPORT', 'IN', 'INC', 'INCL', 'IS', 'LEN', 'LIMITED',
                'LONG', 'LOOP', 'MAX', 'MIN', 'MOD', 'MODULE', 'NEW', 'ODD', 'OF',
                'OR', 'ORD', 'OUT', 'POINTER', 'PROCEDURE', 'RECORD', 'REPEAT', 'RETURN',
                'SHORT', 'SHORTCHAR', 'SHORTINT', 'SIZE', 'THEN', 'TYPE', 'TO', 'UNTIL',
                'VAR', 'WHILE', 'WITH'
                ), suffix=r'\b'), Keyword.Reserved),
            (r'(TRUE|FALSE|NIL|INF)\b', Keyword.Constant),
        ]
    }

    def analyse_text(text):
        """The only other lexer using .cp is the C++ one, so we check if for
        a few common Pascal keywords here. Those are unfortunately quite
        common across various business languages as well."""
        result = 0
        if 'BEGIN' in text:
            result += 0.01
        if 'END' in text:
            result += 0.01
        if 'PROCEDURE' in text:
            result += 0.01
        if 'MODULE' in text:
            result += 0.01

        return result


class ActiveOberonLexer(RegexLexer):
    """
    For Active Oberon source code, the implementation language of the A2
    (Bluebottle) operating system developed at ETH Zürich.
    """
    name = 'Active Oberon'
    aliases = ['activeoberon', 'aos']
    filenames = ['*.Mod', '*.mod']
    mimetypes = ['text/x-active-oberon']
    url = 'https://en.wikipedia.org/wiki/Active_Oberon'
    version_added = '2.21'

    # The Fox compiler reads keywords case insensitively: sources named
    # Module.Mod spell them in upper case, sources named module.mod in
    # lower case. Character classes below are therefore written lower case
    # only.
    flags = re.MULTILINE | re.IGNORECASE

    tokens = {
        'root': [
            include('whitespace'),
            include('preproc'),
            include('comments'),
            include('numliterals'),
            include('strings'),
            include('moduleend'),
            include('asm'),
            include('flags'),
            include('operators'),
            include('punctuation'),
            include('keywords'),
            include('identifiers'),
        ],
        # Everything below "END Module." is ignored by the compiler; sources
        # traditionally keep the commands that build and test the module
        # there.
        'moduleend': [
            (r'(END)(\s+)([a-z_]\w*)(\s*)(\.)',
             bygroups(Keyword.Reserved, Text, Name.Namespace, Text,
                      Punctuation), 'trailer'),
        ],
        'trailer': [
            (r'[^\n]+', Comment.Single),
            (r'\n', Text),
        ],
        # CODE ... END is inline assembler and is not Oberon any more
        'asm': [
            (r'\bCODE\b', Keyword.Reserved, 'code'),
        ],
        'code': [
            include('preproc'),
            # leave the assembler at "END Procedure;" or "END;", but not at
            # an assembler label called end
            (r'\bEND\b(?=;|\s+[a-z_]\w*\s*[;.])', Keyword.Reserved,
             '#pop'),
            (r';[^\n]*', Comment.Single),
            include('whitespace'),
            include('comments'),
            include('numliterals'),
            include('strings'),
            include('flags'),
            (r'[a-z_.][\w.]*', Name),
            (r'[-+*/,:()\[\]@%$#!?=<>&~^\\|]', Operator),
        ],
        'whitespace': [
            (r'\n+', Text),
            (r'\s+', Text),
            # old Oberon texts sometimes carry stray control characters
            (r'[\x00-\x08\x0b\x0c\x0e-\x1f]+', Text),
        ],
        # Conditional compilation, driven by --define= on the command line.
        # It is line oriented and also appears inside inline assembler.
        'preproc': [
            (r'#\s*(if|elsif|else|end)\b[^\n]*', Comment.Preproc),
        ],
        'comments': [
            # (** ... *) is a documentation comment
            (r'\(\*\*(?!\))', Comment.Special, 'doccomment'),
            (r'\(\*', Comment.Multiline, 'comment'),
        ],
        'comment': [
            (r'[^*(]+', Comment.Multiline),
            (r'\(\*', Comment.Multiline, '#push'),
            (r'\*\)', Comment.Multiline, '#pop'),
            (r'[*(]', Comment.Multiline),
        ],
        'doccomment': [
            (r'[^*(]+', Comment.Special),
            (r'\(\*', Comment.Special, '#push'),
            (r'\*\)', Comment.Special, '#pop'),
            (r'[*(]', Comment.Special),
        ],
        'numliterals': [
            (r"[0-9][0-9A-F']*X\b", Number.Hex),        # character code
            (r"[0-9][0-9A-F']*[HL]\b", Number.Hex),     # hexadecimal number
            (r"0x[0-9A-F']+", Number.Hex),
            (r"0b[01']+", Number.Bin),
            # real number, with an optional scale factor; the lookahead
            # keeps the range constructor 1..2 out of the float rule
            (r"[0-9][0-9']*\.(?!\.)[0-9']*(?:[ED][+-]?[0-9]+)?", Number.Float),
            (r"[0-9][0-9']*(?:[ED][+-]?[0-9]+)", Number.Float),
            (r"[0-9][0-9']*", Number.Integer),
        ],
        'strings': [
            # An escaped string \" ... "\ may contain quotes; the variant
            # \X" ... "X\ uses X as an additional escape character.
            (r"""\\(.)(['"])[\s\S]*?\2\1\\""", String.Other),
            (r"""\\(['"])[\s\S]*?\1\\""", String.Other),
            (r"'[^\n']*'", String.Single),
            (r'"[^\n"]*"', String.Double),
        ],
        # Modifiers in braces: PROCEDURE {DELEGATE}, BEGIN {EXCLUSIVE},
        # POINTER {UNSAFE} TO ... The state keeps set constructors such as
        # {0, 2, 4..7} untouched.
        'flags': [
            (r'\{', Punctuation, 'flagset'),
        ],
        'flagset': [
            (r'\}', Punctuation, '#pop'),
            (words((
                # concurrency
                'ACTIVE', 'EXCLUSIVE', 'PRIORITY', 'SAFE', 'REALTIME',
                'UNCOOPERATIVE',
                # object orientation
                'ABSTRACT', 'FINAL', 'OVERRIDE', 'DELEGATE', 'DYNAMIC',
                # memory and safety
                'UNTRACED', 'UNTRACKED', 'UNSAFE', 'UNCHECKED', 'DISPOSABLE',
                'ALIGNED', 'OFFSET', 'MOVABLE', 'REGISTER', 'PLAIN',
                # procedures and linking
                'WINAPI', 'C', 'PlatformCC', 'INTERRUPT', 'NORETURN',
                'ALIGNSTACK', 'PCOFFSET', 'OPENING', 'CLOSING', 'TEST',
                'Fingerprint',
                # active cells
                'DataMemorySize', 'CodeMemorySize', 'InstructionWidth',
                'ChannelWidth', 'ChannelDepth', 'Channels', 'Vector',
                'FloatingPoint', 'NoMul', 'HasNonBlockingIO',
                'FrequencyDivider', 'Engine', 'TRM', 'TRMS', 'Backend',
                'Runtime', 'BaseMem', 'BaseDiv',
                ), suffix=r'\b'), Keyword.Pseudo),
            include('whitespace'),
            include('comments'),
            include('numliterals'),
            include('strings'),
            include('operators'),
            include('punctuation'),
            include('keywords'),
            include('identifiers'),
        ],
        'punctuation': [
            (r'[()\[\],.:;|]', Punctuation),
        ],
        'operators': [
            # communication statements of the Active Cells subset
            (r'<<\?|>>\?|<<|>>|\?\?|!!|[!?]', Operator),
            # element wise relations of math arrays
            (r'\.=|\.#|\.<=|\.>=|\.<|\.>', Operator),
            # element wise and matrix operators, transposition
            (r'\.\*|\./|\*\*|\+\*|\\|`', Operator),
            (r':=|<=|>=|\.\.', Operator),
            (r'[+\-*/=#<>&~^$]', Operator),
        ],
        'keywords': [
            (words((
                'BOOLEAN', 'CHAR', 'INTEGER', 'LONGINTEGER', 'RANGE',
                'INTEGERSET', 'SIGNED8', 'SIGNED16', 'SIGNED32', 'SIGNED64',
                'UNSIGNED8', 'UNSIGNED16', 'UNSIGNED32', 'UNSIGNED64',
                'FLOAT32', 'FLOAT64', 'REAL', 'COMPLEX', 'COMPLEX32',
                'COMPLEX64', 'SET', 'SET8', 'SET16', 'SET32', 'SET64',
                'ADDRESS', 'SIZE', 'ANY', 'OBJECT',
                # historic A2 / Bluebottle type names
                'BYTE', 'SHORTINT', 'LONGINT', 'HUGEINT', 'LONGREAL',
                ), suffix=r'\b'), Keyword.Type),
            (words((
                'ALIAS', 'ARRAY', 'AWAIT', 'BEGIN', 'BY', 'CASE', 'CELL',
                'CELLNET', 'CODE', 'CONST', 'DEFINITION', 'DIV', 'DO',
                'ELSE', 'ELSIF', 'END', 'ENUM', 'EXIT', 'EXTERN', 'FINALLY',
                'FOR', 'IF', 'IGNORE', 'IMPORT', 'IN', 'IS', 'LOOP', 'MOD',
                'MODULE', 'NEW', 'OF', 'OPERATOR', 'OR', 'OUT', 'POINTER',
                'PORT', 'PROCEDURE', 'RECORD', 'REPEAT', 'RETURN', 'THEN',
                'TO', 'TYPE', 'UNTIL', 'VAR', 'WHILE', 'WITH',
                ), suffix=r'\b'), Keyword.Reserved),
            (r'(TRUE|FALSE|NIL|IMAG)\b', Keyword.Constant),
            (r'(SELF|RESULT)\b', Name.Builtin.Pseudo),
            (words((
                'ABS', 'ADDRESSOF', 'ALL', 'ASH', 'ASSERT', 'CAP', 'CAS',
                'CHR', 'CONNECT', 'COPY', 'DEC', 'DECMUL', 'DELEGATE', 'DIM',
                'DISPOSE', 'ENTIER', 'ENTIERH', 'EXCL', 'FIRST', 'FLOOR',
                'GETPROCEDURE', 'HALT', 'IM', 'INC', 'INCL', 'INCMUL',
                'INCR', 'LAST', 'LEN', 'LONG', 'LSH', 'MAX', 'MIN', 'ODD',
                'ORD', 'ORD32', 'RE', 'RECEIVE', 'RESHAPE', 'ROL', 'ROR',
                'ROT', 'SEND', 'SHL', 'SHORT', 'SHR', 'SIZEOF', 'STEP',
                'SUM', 'TRACE', 'WAIT',
                ), suffix=r'\b'), Name.Builtin),
            (r'SYSTEM\b', Name.Builtin),
        ],
        'identifiers': [
            (r'[a-z_]\w*', Name),
        ],
    }

    def analyse_text(text):
        """Modula-2 also claims *.mod, so look for constructs that only
        Active Oberon has."""
        result = 0
        if re.search(r'\bBEGIN\s*\{\s*(ACTIVE|EXCLUSIVE)', text, re.I):
            result += 0.5
        if re.search(r'\bAWAIT\s*\(', text, re.I):
            result += 0.2
        if re.search(r'\b(?:SIGNED|UNSIGNED)(?:8|16|32|64)\b|\bFLOAT(?:32|64)\b',
                     text, re.I):
            result += 0.2
        if re.search(r'=\s*OBJECT\b', text, re.I):
            result += 0.1
        if re.search(r'\bPROCEDURE\s*&', text, re.I):
            result += 0.1
        if re.search(r'^\s*MODULE\s+\w+\s*;', text, re.I | re.M):
            result += 0.05
        return min(result, 1.0)
