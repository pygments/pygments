# -*- coding: utf-8 -*-
"""
    pygments.lexers.qbasic
    ~~~~~~~~~~~~~~~~~~~~~~

    Simple lexer for Microsoft QBasic source code.

    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, bygroups, using, \
     this, combined, ExtendedRegexLexer
from pygments.token import *

__all__ = ['QBasicLexer']

class QBasicLexer(RegexLexer):
    """
    For
    `QBasic <http://en.wikipedia.org/wiki/QBasic>`_
    source code.
    """

    name = 'QBasic'
    aliases = ['qbasic', 'basic']
    filenames = ['*.BAS', '*.bas']
    mimetype = []

    declarations = """\
    DATA LET
    """.split()

    functions = """\
ABS ASC ATN CDBL CINT CLNG COMMAND$ COS CSNG CSRLIN CVD CVDMBF CVI CVL CVS CVSMBF DATE$ ENVIRON$ EOF ERDEV ERDEV$ ERL ERR EXP FILEATTR FIX FRE FREEFILE HEX$ INKEY$ INP INPUT$ INSTR INT IOCTL$ LBOUND LCASE$ LEFT$ LEN LOC LOF LOG LPOS LTRIM$ MID$ MKD$ MKDMBF$ MKI$ MKL$ MKS$ MKSMBF$ OCT$ PEEK PEN PLAY PMAP POINT POS RIGHT$ RND RTRIM$ SADD SCREEN SEEK SETMEM SGN SIN SPACE$ SPC SQR STICK STR$ STRIG STRING$ TAB TAN TIME$ TIMER UBOUND UCASE$ VAL VARPTR VARPTR$ VARSEG
    """.split()

    metacommands = """\
    $DYNAMIC $INCLUDE $STATIC
    """.split()

    operators = """\
    AND EQV IMP NOT OR XOR
    """.split()

    statements = """\
    BEEP BLOAD BSAVE CALL CALL ABSOLUTE CALL INTERRUPT CALLS CHAIN CHDIR CIRCLE CLEAR CLOSE CLS COLOR COM COMMON CONST DATE$ DEF FN DEF SEG DEFDBL DEFINT DEFLNG DEFSNG DEFSTR DIM DO DRAW END ENVIRON ERASE ERROR EXIT FIELD FILES FOR FUNCTION GET GOSUB GOTO IF INPUT IOCTL KEY KILL LINE LOCATE LOCK LPRINT LSET MID$ MKDIR NAME ON OPEN OPTION BASE OUT PAINT PALETTE PCOPY PEN PLAY POKE PRESET PRINT PRINT USING PSET PUT RANDOMIZE READ REDIM RESET RESTORE RESUME RETURN RMDIR RSET RUN SCREEN SEEK SELECT CASE SHARED SHELL SLEEP SOUND STATIC STOP STRIG SUB SWAP SYSTEM TIME$ TIMER TROFF TRON TYPE UEVENT UNLOCK VIEW WAIT WHILE WIDTH WINDOW WRITE
    """.split()

    keywords = """\
    ACCESS ALIAS ANY APPEND AS BASE BINARY BYVAL CASE CDECL DOUBLE ELSE ELSEIF ENDIF INTEGER IS LIST LOCAL LONG LOOP MOD NEXT OFF ON OUTPUT RANDOM SIGNAL SINGLE STEP STRING THEN TO UNTIL USING WEND
    """.split()

    tokens = {
        'root': [
            (r'^(\s*)(\d+)(\s*)',
            	bygroups(Text.Whitespace, Name.Label, Text.Whitespace)),
            (r'\'.*\n', Comment.Single),
            (r'REM .*\n', Comment.Single),
            (r'"[^\n\"]*"', String.Double),
            (r'(DECLARE)(\s+)([A-Z]+)(\s+)([^\s]+)',
            	bygroups(Keyword.Declaration, Text.Whitespace, Name.Variable, Text.Whitespace, Name.Function)),
            (r'(DIM)(\s+)(SHARED)(\s+)([^\n\s\t\(]+)',
            	bygroups(Keyword.Declaration, Text.Whitespace, Name.Variable, Text.Whitespace, Name.Variable.Global)),
            (r'(DIM)(\s+)([^\n\s\t\(]+)',
            	bygroups(Keyword.Declaration, Text.Whitespace, Name.Variable.Global)),
            (r'CHR$\(.+\)', String.Char),
            (r'^(\s*)([a-zA-Z_]+)(\s*)(\=)',
            	bygroups(Text.Whitespace, Name.Variable.Global, Text.Whitespace, Operator)),
            (r'(GOTO)(\s+)(\w+\:?)',
            	bygroups(Keyword.Reserved, Text.Whitespace, Name.Label)),
            (r'[\[\]{}(),;]', Punctuation),
            (r'[a-zA-Z_]\w*[\$|@|#|&|!]', Name.Variable.Global),
            (r'[a-zA-Z_]\w*\:', Name.Label),
            (r'\-?\d*\.\d+\#?', Number.Float),
            (r'\-?\d+\#?', Number.Integer),
            (r'!=|==|:=|\.=|<<|>>|[-~+/\\*%=<>&^|?:!.]', Operator),
            include('declarations'),
            include('functions'),
            include('metacommands'),
            include('operators'),
            include('statements'),
            include('keywords'),
            (r'[\n]+', Text),
            (r'[\s]+', Text.Whitespace),
        ],
        'declarations': [
            (r'(%s)' % '|'.join(declarations), Keyword.Declaration),
        ],
        'functions': [
            (r'(%s)' % '|'.join(functions), Keyword.Reserved),
        ],
        'metacommands': [
            (r'(%s)' % '|'.join(metacommands), Keyword.Constant),
        ],
        'operators': [
            (r'(%s)' % '|'.join(operators), Operator.Word),
        ],
        'statements': [
            (r'(%s)' % '|'.join(statements), Keyword.Reserved),
        ],
        'keywords': [
            (r'(%s)' % '|'.join(keywords), Keyword),
        ],
    }
