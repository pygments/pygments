# -*- coding: utf-8 -*-
"""
    pygments.lexers.cobol
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for COBOL languages.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Error

__all__ = ['CobolLexer', 'CobolFreeformatLexer']


class CobolLexer(RegexLexer):
    """
    Lexer for OpenCOBOL code.

    .. versionadded:: 1.6
    """
    name = 'COBOL'
    aliases = ['cobol']
    filenames = ['*.cob', '*.COB', '*.cpy', '*.CPY']
    mimetypes = ['text/x-cobol']
    flags = re.IGNORECASE | re.MULTILINE

    # Data Types: by PICTURE and USAGE
    # Operators: **, *, +, -, /, <, >, <=, >=, =, <>
    # Logical (?): NOT, AND, OR

    # Reserved words:
    # http://opencobol.add1tocobol.com/#reserved-words
    # Intrinsics:
    # http://opencobol.add1tocobol.com/#does-opencobol-implement-any-intrinsic-functions

    tokens = {
        'root': [
            include('comment'),
            include('strings'),
            include('core'),
            include('nums'),
            (r'[a-z0-9]([_a-z0-9\-]*[a-z0-9]+)?', Name.Variable),
            # (r'[\s]+', Text),
            (r'[ \t]+', Text),
        ],
        'comment': [
            (r'(^.{6}[*/].*\n|^.{6}|\*>.*\n)', Comment),
        ],
        'core': [
            # Figurative constants
            (r'(^|(?<=[^0-9a-z_\-]))(ALL\s+)?'
             r'((ZEROES)|(HIGH-VALUE|LOW-VALUE|QUOTE|SPACE|ZERO)(S)?)'
             r'\s*($|(?=[^0-9a-z_\-]))',
             Name.Constant),

            # Reserved words STATEMENTS and other bolds
            (r'(^|(?<=[^0-9a-z_\-]))'
             r'(ACCEPT|ADD|ALLOCATE|CALL|CANCEL|CLOSE|COMPUTE|'
             r'CONFIGURATION|CONTINUE|'
             r'DATA|DELETE|DISPLAY|DIVIDE|DIVISION|ELSE|END|END-ACCEPT|'
             r'END-ADD|END-CALL|END-COMPUTE|END-DELETE|END-DISPLAY|'
             r'END-DIVIDE|END-EVALUATE|END-IF|END-MULTIPLY|END-OF-PAGE|'
             r'END-PERFORM|END-READ|END-RETURN|END-REWRITE|END-SEARCH|'
             r'END-START|END-STRING|END-SUBTRACT|END-UNSTRING|END-WRITE|'
             r'ENVIRONMENT|EVALUATE|EXIT|FD|FILE|FILE-CONTROL|FOREVER|'
             r'FREE|GENERATE|GO|GOBACK|'
             r'IDENTIFICATION|IF|INITIALIZE|'
             r'INITIATE|INPUT-OUTPUT|INSPECT|INVOKE|I-O-CONTROL|LINKAGE|'
             r'LOCAL-STORAGE|MERGE|MOVE|MULTIPLY|OPEN|'
             r'PERFORM|PROCEDURE|PROGRAM-ID|RAISE|READ|RELEASE|RESUME|'
             r'RETURN|REWRITE|SCREEN|'
             r'SD|SEARCH|SECTION|SET|SORT|START|STOP|STRING|SUBTRACT|'
             r'SUPPRESS|TERMINATE|THEN|UNLOCK|UNSTRING|USE|VALIDATE|'
             r'WORKING-STORAGE|WRITE)'
             r'\s*($|(?=[^0-9a-z_\-]))', Keyword.Reserved),

            # Reserved words
            (r'(^|(?<=[^0-9a-z_\-]))'
             r'(ACCESS|ADDRESS|ADVANCING|AFTER|ALL|'
             r'ALPHABET|ALPHABETIC|ALPHABETIC-LOWER|ALPHABETIC-UPPER|'
             r'ALPHANUMERIC|ALPHANUMERIC-EDITED|ALSO|ALTER|ALTERNATE'
             r'ANY|ARE|AREA|AREAS|ARGUMENT-NUMBER|ARGUMENT-VALUE|AS|'
             r'ASCENDING|ASSIGN|AT|AUTO|AUTO-SKIP|AUTOMATIC|AUTOTERMINATE|'
             r'BACKGROUND-COLOR|BASED|BEEP|BEFORE|BELL|'
             r'BLANK|'
             r'BLINK|BLOCK|BOTTOM|BY|BYTE-LENGTH|CHAINING|'
             r'CHARACTER|CHARACTERS|CLASS|CODE|CODE-SET|COL|COLLATING|'
             r'COLS|COLUMN|COLUMNS|COMMA|COMMAND-LINE|COMMIT|COMMON|'
             r'CONSTANT|CONTAINS|CONTENT|CONTROL|'
             r'CONTROLS|CONVERTING|COPY|CORR|CORRESPONDING|COUNT|CRT|'
             r'CURRENCY|CURSOR|CYCLE|DATE|DAY|DAY-OF-WEEK|DE|DEBUGGING|'
             r'DECIMAL-POINT|DECLARATIVES|DEFAULT|DELIMITED|'
             r'DELIMITER|DEPENDING|DESCENDING|DETAIL|DISK|'
             r'DOWN|DUPLICATES|DYNAMIC|EBCDIC|'
             r'ENTRY|ENVIRONMENT-NAME|ENVIRONMENT-VALUE|EOL|EOP|'
             r'EOS|ERASE|ERROR|ESCAPE|EXCEPTION|'
             r'EXCLUSIVE|EXTEND|EXTERNAL|'
             r'FILE-ID|FILLER|FINAL|FIRST|FIXED|FLOAT-LONG|FLOAT-SHORT|'
             r'FOOTING|FOR|FOREGROUND-COLOR|FORMAT|FROM|FULL|FUNCTION|'
             r'FUNCTION-ID|GIVING|GLOBAL|GROUP|'
             r'HEADING|HIGHLIGHT|I-O|ID|'
             r'IGNORE|IGNORING|IN|INDEX|INDEXED|INDICATE|'
             r'INITIAL|INITIALIZED|INPUT|'
             r'INTO|INTRINSIC|INVALID|IS|JUST|JUSTIFIED|KEY|LABEL|'
             r'LAST|LEADING|LEFT|LENGTH|LIMIT|LIMITS|LINAGE|'
             r'LINAGE-COUNTER|LINE|LINES|LOCALE|LOCK|'
             r'LOWLIGHT|MANUAL|MEMORY|MINUS|MODE|'
             r'MULTIPLE|NATIONAL|NATIONAL-EDITED|NATIVE|'
             r'NEGATIVE|NEXT|NO|NULL|NULLS|NUMBER|NUMBERS|NUMERIC|'
             r'NUMERIC-EDITED|OBJECT-COMPUTER|OCCURS|OF|OFF|OMITTED|ON|ONLY|'
             r'OPTIONAL|ORDER|ORGANIZATION|OTHER|OUTPUT|OVERFLOW|'
             r'OVERLINE|PACKED-DECIMAL|PADDING|PAGE|PARAGRAPH|'
             r'PLUS|POINTER|POSITION|POSITIVE|PRESENT|PREVIOUS|'
             r'PRINTER|PRINTING|PROCEDURE-POINTER|PROCEDURES|'
             r'PROCEED|PROGRAM|PROGRAM-POINTER|PROMPT|QUOTE|'
             r'QUOTES|RANDOM|RD|RECORD|RECORDING|RECORDS|RECURSIVE|'
             r'REDEFINES|REEL|REFERENCE|RELATIVE|REMAINDER|REMOVAL|'
             r'RENAMES|REPLACING|REPORT|REPORTING|REPORTS|REPOSITORY|'
             r'REQUIRED|RESERVE|RETURNING|REVERSE-VIDEO|REWIND|'
             r'RIGHT|ROLLBACK|ROUNDED|RUN|SAME|SCROLL|'
             r'SECURE|SEGMENT-LIMIT|SELECT|SENTENCE|SEPARATE|'
             r'SEQUENCE|SEQUENTIAL|SHARING|SIGN|SIGNED|SIGNED-INT|'
             r'SIGNED-LONG|SIGNED-SHORT|SIZE|SORT-MERGE|SOURCE|'
             r'SOURCE-COMPUTER|SPECIAL-NAMES|STANDARD|'
             r'STANDARD-1|STANDARD-2|STATUS|SUM|'
             r'SYMBOLIC|SYNC|SYNCHRONIZED|TALLYING|TAPE|'
             r'TEST|THROUGH|THRU|TIME|TIMES|TO|TOP|TRAILING|'
             r'TRANSFORM|TYPE|UNDERLINE|UNIT|UNSIGNED|'
             r'UNSIGNED-INT|UNSIGNED-LONG|UNSIGNED-SHORT|UNTIL|UP|'
             r'UPDATE|UPON|USAGE|USING|VALUE|VALUES|VARYING|WAIT|WHEN|'
             r'WITH|WORDS|YYYYDDD|YYYYMMDD)'
             r'\s*($|(?=[^0-9a-z_\-]))', Keyword.Pseudo),

            # inactive reserved words
            (r'(^|(?<=[^0-9a-z_\-]))'
             r'(ACTIVE-CLASS|ALIGNED|ANYCASE|ARITHMETIC|ATTRIBUTE|B-AND|'
             r'B-NOT|B-OR|B-XOR|BIT|BOOLEAN|CD|CENTER|CF|CH|CHAIN|CLASS-ID|'
             r'CLASSIFICATION|COMMUNICATION|CONDITION|DATA-POINTER|'
             r'DESTINATION|DISABLE|EC|EGI|EMI|ENABLE|END-RECEIVE|'
             r'ENTRY-CONVENTION|EO|ESI|EXCEPTION-OBJECT|EXPANDS|FACTORY|'
             r'FLOAT-BINARY-16|FLOAT-BINARY-34|FLOAT-BINARY-7|'
             r'FLOAT-DECIMAL-16|FLOAT-DECIMAL-34|FLOAT-EXTENDED|FORMAT|'
             r'FUNCTION-POINTER|GET|GROUP-USAGE|IMPLEMENTS|INFINITY|'
             r'INHERITS|INTERFACE|INTERFACE-ID|INVOKE|LC_ALL|LC_COLLATE|'
             r'LC_CTYPE|LC_MESSAGES|LC_MONETARY|LC_NUMERIC|LC_TIME|'
             r'LINE-COUNTER|MESSAGE|METHOD|METHOD-ID|NESTED|NONE|NORMAL|'
             r'OBJECT|OBJECT-REFERENCE|OPTIONS|OVERRIDE|PAGE-COUNTER|PF|PH|'
             r'PROPERTY|PROTOTYPE|PURGE|QUEUE|RAISE|RAISING|RECEIVE|'
             r'RELATION|REPLACE|REPRESENTS-NOT-A-NUMBER|RESET|RESUME|RETRY|'
             r'RF|RH|SECONDS|SEGMENT|SELF|SEND|SOURCES|STATEMENT|STEP|'
             r'STRONG|SUB-QUEUE-1|SUB-QUEUE-2|SUB-QUEUE-3|SUPER|SYMBOL|'
             r'SYSTEM-DEFAULT|TABLE|TERMINAL|TEXT|TYPEDEF|UCS-4|UNIVERSAL|'
             r'USER-DEFAULT|UTF-16|UTF-8|VAL-STATUS|VALID|VALIDATE|'
             r'VALIDATE-STATUS)\s*($|(?=[^0-9a-z_\-]))', Error),

            # Data Types
            (r'(^|(?<=[^0-9a-z_\-]))'
             r'(PIC\s+.+?(?=(\s|\.\s))|PICTURE\s+.+?(?=(\s|\.\s))|'
             r'(COMPUTATIONAL)(-[1-5X])?|(COMP)(-[1-5X])?|'
             r'BINARY-C-LONG|'
             r'BINARY-CHAR|BINARY-DOUBLE|BINARY-LONG|BINARY-SHORT|'
             r'BINARY)\s*($|(?=[^0-9a-z_\-]))', Keyword.Type),

            # Operators
            (r'(\*\*|\*|\+|-|/|<=|>=|<|>|==|/=|=)', Operator),

            # (r'(::)', Keyword.Declaration),

            (r'([(),;:&%.])', Punctuation),

            # Intrinsics
            (r'(^|(?<=[^0-9a-z_\-]))(ABS|ACOS|ANNUITY|ASIN|ATAN|BYTE-LENGTH|'
             r'CHAR|COMBINED-DATETIME|CONCATENATE|COS|CURRENT-DATE|'
             r'DATE-OF-INTEGER|DATE-TO-YYYYMMDD|DAY-OF-INTEGER|DAY-TO-YYYYDDD|'
             r'EXCEPTION-(?:FILE|LOCATION|STATEMENT|STATUS)|EXP10|EXP|E|'
             r'FACTORIAL|FRACTION-PART|INTEGER-OF-(?:DATE|DAY|PART)|INTEGER|'
             r'LENGTH|LOCALE-(?:DATE|TIME(?:-FROM-SECONDS)?)|LOG(?:10)?|'
             r'LOWER-CASE|MAX|MEAN|MEDIAN|MIDRANGE|MIN|MOD|NUMVAL(?:-C)?|'
             r'ORD(?:-MAX|-MIN)?|PI|PRESENT-VALUE|RANDOM|RANGE|REM|REVERSE|'
             r'SECONDS-FROM-FORMATTED-TIME|SECONDS-PAST-MIDNIGHT|SIGN|SIN|SQRT|'
             r'STANDARD-DEVIATION|STORED-CHAR-LENGTH|SUBSTITUTE(?:-CASE)?|'
             r'SUM|TAN|TEST-DATE-YYYYMMDD|TEST-DAY-YYYYDDD|TRIM|'
             r'UPPER-CASE|VARIANCE|WHEN-COMPILED|YEAR-TO-YYYY)\s*'
             r'($|(?=[^0-9a-z_\-]))', Name.Function),

            # Booleans
            (r'(^|(?<=[^0-9a-z_\-]))(true|false)\s*($|(?=[^0-9a-z_\-]))', Name.Builtin),
            # Comparing Operators
            (r'(^|(?<=[^0-9a-z_\-]))(equal|equals|ne|lt|le|gt|ge|'
             r'greater|less|than|not|and|or)\s*($|(?=[^0-9a-z_\-]))', Operator.Word),
        ],

        # \"[^\"\n]*\"|\'[^\'\n]*\'
        'strings': [
            # apparently strings can be delimited by EOL if they are continued
            # in the next line
            (r'"[^"\n]*("|\n)', String.Double),
            (r"'[^'\n]*('|\n)", String.Single),
        ],

        'nums': [
            (r'\d+(\s*|\.$|$)', Number.Integer),
            (r'[+-]?\d*\.\d+([eE][-+]?\d+)?', Number.Float),
            (r'[+-]?\d+\.\d*([eE][-+]?\d+)?', Number.Float),
        ],
    }


class CobolFreeformatLexer(CobolLexer):
    """
    Lexer for Free format OpenCOBOL code.

    .. versionadded:: 1.6
    """
    name = 'COBOLFree'
    aliases = ['cobolfree']
    filenames = ['*.cbl', '*.CBL']
    mimetypes = []
    flags = re.IGNORECASE | re.MULTILINE

    tokens = {
        'comment': [
            (r'(\*>.*\n|^\w*\*.*$)', Comment),
        ],
    }
