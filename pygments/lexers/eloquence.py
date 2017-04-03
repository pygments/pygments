# -*- coding: utf-8 -*-
"""
    pygments.lexers.eloquence
    ~~~~~~~~~~~~~~~~~~~

    Lexers for Eloquence.

    :copyright: Copyright 2006-2017 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include, words, default
from pygments.token import *

__all__ = ['EloquenceLexer']


class EloquenceLexer(RegexLexer):
    """
    For `Eloquence <http://eloquence.marxmeier.com>`_ source code.
    
    Contributed by Robert Andersson <https://github.com/kemichal>,
    <https://bitbucket.org/kemichal>.
    """

    name = 'Eloquence'
    aliases = ['eloquence', 'eloq']
    filenames = ['*.eq']

    keywords = (
        'DESCRIPTION', 'IN DATA SET', 'NO OPERATOR', 'OPTION BASE',
        'TRANSPARENT', 'BACKGROUND', 'DBROLLBACK', 'DUMP STACK', 'EXCLUSIVE',
        'POPUP BOX', 'PREDICATE', 'RANDOMIZE', 'REVISION$', 'DBCOMMIT',
        'DBDELETE', 'DBUNLOCK', 'DBUPDATE', 'LASTLINE', 'MAPPNTR$', 'READONLY',
        'STANDARD', 'WORKFILE', 'CHANGES', 'COMMAND', 'DBBEGIN', 'DBCLOSE',
        'ERRMSG$', 'EXTENDS', 'FCREATE', 'GETENV$', 'LDSPEC$', 'MAPVOL$',
        'PACKFMT', 'PRINTER', 'RELEASE', 'REQUEST', 'RESTORE', 'SUBEXIT',
        'TRAILER', 'TYPEOF$', 'XASSIGN', 'XUNPACK', 'ACCEPT', 'APPEND',
        'ASSIGN', 'BINCMP', 'BINIOR', 'CREATE', 'CURKEY', 'CURSOR', 'DBFIND',
        'DBINFO', 'DBLOCK', 'DBOPEN', 'DEFINE', 'DETAIL', 'DROUND', 'ENABLE',
        'EXPORT', 'HEADER', 'LENGTH', 'LINPUT', 'NUMREC', 'PROUND', 'REMOTE',
        'REPEAT', 'REPORT', 'RETURN', 'RIGHT$', 'ROTATE', 'SELECT', 'SIGNAL',
        'STDERR', 'STDOUT', 'STRUCT', 'SYSID$', 'SYSTEM', 'TASKID', 'THREAD',
        'TOTALS', 'UNPACK', 'UPDATE', 'XPURGE', 'BEGIN', 'BREAK', 'CLOCK',
        'DATE$', 'DBASE', 'DBGET', 'DBPUT', 'ERRM$', 'ERROR', 'FIXED', 'FRACT',
        'GRAND', 'IMAGE', 'LABEL', 'LDISP', 'LINES', 'LISTS', 'MERGE', 'PAUSE',
        'PRINT', 'PURGE', 'REDIM', 'SHIFT', 'SLEEP', 'TIME$', 'TOTAL', 'TRIM$',
        'UNTIL', 'USING', 'USRID', 'WFLEN', 'WHILE', 'WIDTH', 'XPACK', 'BEEP',
        'CALL', 'CASE', 'CHR$', 'DATA', 'DISP', 'DRAW', 'EDIT', 'ELSE', 'ERRL',
        'ERRN', 'EXIT', 'FIND', 'FREE', 'FROM', 'GRAD', 'HOLE', 'LINE', 'LIST',
        'LOAD', 'LOOP', 'LWC$', 'MD5$', 'NEXT', 'OTHR', 'PACK', 'PAGE', 'PNTR',
        'PPID', 'READ', 'RPT$', 'SCAN', 'SIZE', 'SORT', 'STEP', 'STOP', 'SYNC',
        'THEN', 'THIS', 'TYPE', 'UPC$', 'VAL$', 'WAIT', 'WHEN', 'WITH', 'YPOS',
        'ABS', 'ACS', 'ALL', 'ASN', 'ATN', 'COL', 'CON', 'COS', 'DEG', 'DEL',
        'DIV', 'DLG', 'DLL', 'END', 'EXP', 'FOR', 'GET', 'INT', 'KEY', 'LEN',
        'LET', 'LEX', 'LGT', 'LIN', 'LOG', 'MAT', 'MAX', 'MIN', 'MOD', 'NEW',
        'NUM', 'OFF', 'PID', 'POS', 'RAD', 'RND', 'ROW', 'SET', 'SGN', 'SIN',
        'SKP', 'SQR', 'SUB', 'TAN', 'USE', 'VAL', 'ZER', 'AS', 'BY', 'DO',
        'IF', 'IS', 'ON', 'PI', 'TO',
    )

    tokens = {
        'root': [
            include('define_function'),
            include('define_subroutine'),
            include('common'),
        ],
        'define_function': [
            # Class function member
            (r'(DEF FN)([A-Z][a-z0-9_]*)(:)([A-Z][a-z0-9_]*\${0,1})',
             bygroups(Keyword, Keyword.Type, Operator, Name.Function),
             'function'),
            # Function
            (r'(DEF FN)([A-Z][a-z0-9_]*\${0,1})',
             bygroups(Keyword, Name.Function), 'function'),
        ],
        'define_single_line_function': [
            # Single line function definition. Must contain equals sign
            (r'(DEF FN)([A-Z][a-z0-9]*\${0,1})(?=.*=)',
             bygroups(Keyword, Name.Function)),
        ],
        'define_subroutine': [
            # Class subroutine member
            (r'(SUB )([A-Z][a-z0-9_]*)(:)([A-Z][a-z0-9_]*\${0,1})',
             bygroups(Keyword, Keyword.Type, Operator, Name.Function),
             'subroutine'),
            # Use negative look-behind to avoid matches with "DEL SUB" and
            # "LOAD SUB"
            (r'(?<!DEL )(?<!LOAD )(SUB )([A-Z][a-z0-9_]*\${0,1})',
             bygroups(Keyword, Name.Function), 'subroutine'),
        ],
        'function': [
            (r'FNEND', Keyword, '#pop'),
            include('common'),
        ],
        'subroutine': [
            (r'SUBEND', Keyword, '#pop'),
            include('common'),
        ],
        'common': [
            include('whitespace'),
            include('comment'),
            include('define_single_line_function'),
            include('call_function'),
            include('call_subroutine'),
            include('data'),
            include('declaration'),
            include('labels'),
            include('keywords'),
            include('string'),
            include('number'),
            include('operator'),
            include('identifier'),
            # REM Comment, low priority
            (r'REM.*$', Comment),
        ],
        'data': [
            # The DATA keyword can be followed by a list of numbers, quoted
            # strings, unquoted strings or a mix
            (r'(DATA )', Keyword, 'data_list'),
        ],
        'call_function': [
            # Call a class member function
            (r'(FNTHIS)(\.)([A-Z][a-z0-9_]*\${0,1})',
             bygroups(Keyword, Operator, Name.Function)),
            # Call a function
            (r'(FN)([A-Z][a-z0-9_]*\${0,1})',
             bygroups(Keyword, Name.Function)),
        ],
        'call_subroutine': [
            # Call a class member subroutine. The THIS keyword is optional
            (r'(CALL (?:THIS){0,1}\.)([A-Z][a-z0-9_]*)',
             bygroups(Keyword, Name.Function)),
            # Call a subroutine, can be a subroutine defined in a DLL.
            (r'(CALL DLL |LOAD DLL |CALL )([A-Z][a-z0-9_]*)',
             bygroups(Keyword, Name.Function)),
        ],
        'data_list': [
            (r'\n', Whitespace, '#pop'),
            (r',', Operator),
            include('string'),
            include('number'),
            (r'[^,\n]+', String),
        ],
        'comment': [
            (r'!.*$', Comment),
        ],
        'keywords': [
            (words(keywords, prefix=r'\b'), Keyword)
        ],
        'labels': [
            # Label jumps can exists after GOTO, THEN or GOSUB. The label(s)
            # must be the last thing on the line, except for a comment
            (r'(GOTO |THEN |GOSUB )([A-Z][a-z0-9_]*)(?=$|,| *!)',
             bygroups(Keyword, Name.Label), 'label_list'),
            # Label definition, must be located at the start of the line and
            # end with a colon
            (r'^[A-Z][a-z0-9_]*(?=:)', Name.Label),
            (r'(USING REMOTE LISTS |USING )([A-Z][a-z0-9_]*)',
             bygroups(Keyword, Name.Label)),
        ],
        'label_list': [
            (r'(,)([A-Z][a-z0-9_]*)', bygroups(Operator, Name.Label)),
            default('#pop'),
        ],
        'whitespace': [
            (r'\s+', Text.Whitespace),
        ],
        'string': [
            ('"', String, 'string_end'),
        ],
        'string_end': [
            # Consume until quotes. Eloquence doesn't have escaped quotes (\")
            # so no need to worry about that.
            ('[^"]+', String),
            ('"', String, '#pop'),
        ],
        'number': [
            # Numbers like 1, 1.1 or 1E6
            (r'\d+(\.\d+){0,1}(E\d+){0,1}', Number),
        ],
        'operator': [
            (words(('+', '-', '/', '*', '=', '[', ']', ';', ',', '>', '<', '(',
                    ')', '&', '.', ':', '^')), Operator),
            (words(('BINAND', 'NOT', 'OR', 'AND', 'XOR', 'EXOR')),
             Operator.Word),
        ],
        'identifier': [
            # Identifiers like My_string1$. We do a negative look-ahead to not
            # accidentally match an unknown keyword
            (r'[A-Z][a-z0-9_]*(?![A-Z])\${0,1}', Keyword.Type),
            # File handle
            (r'#', Keyword),
        ],
        'declaration': [
            (words(('COM', 'DIM', 'INTEGER', 'DINTEGER', 'REAL', 'FLOAT',
                    'SHORT'), prefix=r'\b', suffix=r'\b'),
             Keyword.Declaration)
        ]
    }
