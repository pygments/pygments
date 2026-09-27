"""
    pygments.lexers.business
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for "business-oriented" languages.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, words, bygroups
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Error, Whitespace

from pygments.lexers._openedge_builtins import OPENEDGEKEYWORDS

__all__ = ['CobolLexer', 'CobolFreeformatLexer', 'ABAPLexer', 'OpenEdgeLexer',
           'GoodDataCLLexer', 'MaqlLexer', 'PLILexer']


class CobolLexer(RegexLexer):
    """
    Lexer for OpenCOBOL code.
    """
    name = 'COBOL'
    aliases = ['cobol']
    filenames = ['*.cob', '*.COB', '*.cpy', '*.CPY']
    mimetypes = ['text/x-cobol']
    url = 'https://en.wikipedia.org/wiki/COBOL'
    version_added = '1.6'

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
            (r'[a-z0-9]([\w\-]*[a-z0-9]+)?', Name.Variable),
            # (r'[\s]+', Text),
            (r'[ \t]+', Whitespace),
        ],
        'comment': [
            (r'(^.{6}[*/].*\n|^.{6}|\*>.*\n)', Comment),
        ],
        'core': [
            # Figurative constants
            (r'(^|(?<=[^\w\-]))(ALL\s+)?'
             r'((ZEROES)|(HIGH-VALUE|LOW-VALUE|QUOTE|SPACE|ZERO)(S)?)'
             r'\s*($|(?=[^\w\-]))',
             Name.Constant),

            # Reserved words STATEMENTS and other bolds
            (words((
                'ACCEPT', 'ADD', 'ALLOCATE', 'CALL', 'CANCEL', 'CLOSE', 'COMPUTE',
                'CONFIGURATION', 'CONTINUE', 'DATA', 'DELETE', 'DISPLAY', 'DIVIDE',
                'DIVISION', 'ELSE', 'END', 'END-ACCEPT',
                'END-ADD', 'END-CALL', 'END-COMPUTE', 'END-DELETE', 'END-DISPLAY',
                'END-DIVIDE', 'END-EVALUATE', 'END-IF', 'END-MULTIPLY', 'END-OF-PAGE',
                'END-PERFORM', 'END-READ', 'END-RETURN', 'END-REWRITE', 'END-SEARCH',
                'END-START', 'END-STRING', 'END-SUBTRACT', 'END-UNSTRING', 'END-WRITE',
                'ENVIRONMENT', 'EVALUATE', 'EXIT', 'FD', 'FILE', 'FILE-CONTROL', 'FOREVER',
                'FREE', 'GENERATE', 'GO', 'GOBACK', 'IDENTIFICATION', 'IF', 'INITIALIZE',
                'INITIATE', 'INPUT-OUTPUT', 'INSPECT', 'INVOKE', 'I-O-CONTROL', 'LINKAGE',
                'LOCAL-STORAGE', 'MERGE', 'MOVE', 'MULTIPLY', 'OPEN', 'PERFORM',
                'PROCEDURE', 'PROGRAM-ID', 'RAISE', 'READ', 'RELEASE', 'RESUME',
                'RETURN', 'REWRITE', 'SCREEN', 'SD', 'SEARCH', 'SECTION', 'SET',
                'SORT', 'START', 'STOP', 'STRING', 'SUBTRACT', 'SUPPRESS',
                'TERMINATE', 'THEN', 'UNLOCK', 'UNSTRING', 'USE', 'VALIDATE',
                'WORKING-STORAGE', 'WRITE'), prefix=r'(^|(?<=[^\w\-]))',
                suffix=r'\s*($|(?=[^\w\-]))'),
             Keyword.Reserved),

            # Reserved words
            (words((
                'ACCESS', 'ADDRESS', 'ADVANCING', 'AFTER', 'ALL',
                'ALPHABET', 'ALPHABETIC', 'ALPHABETIC-LOWER', 'ALPHABETIC-UPPER',
                'ALPHANUMERIC', 'ALPHANUMERIC-EDITED', 'ALSO', 'ALTER', 'ALTERNATE',
                'ANY', 'ARE', 'AREA', 'AREAS', 'ARGUMENT-NUMBER', 'ARGUMENT-VALUE', 'AS',
                'ASCENDING', 'ASSIGN', 'AT', 'AUTO', 'AUTO-SKIP', 'AUTOMATIC',
                'AUTOTERMINATE', 'BACKGROUND-COLOR', 'BASED', 'BEEP', 'BEFORE', 'BELL',
                'BLANK', 'BLINK', 'BLOCK', 'BOTTOM', 'BY', 'BYTE-LENGTH', 'CHAINING',
                'CHARACTER', 'CHARACTERS', 'CLASS', 'CODE', 'CODE-SET', 'COL',
                'COLLATING', 'COLS', 'COLUMN', 'COLUMNS', 'COMMA', 'COMMAND-LINE',
                'COMMIT', 'COMMON', 'CONSTANT', 'CONTAINS', 'CONTENT', 'CONTROL',
                'CONTROLS', 'CONVERTING', 'COPY', 'CORR', 'CORRESPONDING', 'COUNT', 'CRT',
                'CURRENCY', 'CURSOR', 'CYCLE', 'DATE', 'DAY', 'DAY-OF-WEEK', 'DE',
                'DEBUGGING', 'DECIMAL-POINT', 'DECLARATIVES', 'DEFAULT', 'DELIMITED',
                'DELIMITER', 'DEPENDING', 'DESCENDING', 'DETAIL', 'DISK',
                'DOWN', 'DUPLICATES', 'DYNAMIC', 'EBCDIC',
                'ENTRY', 'ENVIRONMENT-NAME', 'ENVIRONMENT-VALUE', 'EOL', 'EOP',
                'EOS', 'ERASE', 'ERROR', 'ESCAPE', 'EXCEPTION',
                'EXCLUSIVE', 'EXTEND', 'EXTERNAL', 'FILE-ID', 'FILLER', 'FINAL',
                'FIRST', 'FIXED', 'FLOAT-LONG', 'FLOAT-SHORT',
                'FOOTING', 'FOR', 'FOREGROUND-COLOR', 'FORMAT', 'FROM', 'FULL',
                'FUNCTION', 'FUNCTION-ID', 'GIVING', 'GLOBAL', 'GROUP',
                'HEADING', 'HIGHLIGHT', 'I-O', 'ID',
                'IGNORE', 'IGNORING', 'IN', 'INDEX', 'INDEXED', 'INDICATE',
                'INITIAL', 'INITIALIZED', 'INPUT', 'INTO', 'INTRINSIC', 'INVALID',
                'IS', 'JUST', 'JUSTIFIED', 'KEY', 'LABEL',
                'LAST', 'LEADING', 'LEFT', 'LENGTH', 'LIMIT', 'LIMITS', 'LINAGE',
                'LINAGE-COUNTER', 'LINE', 'LINES', 'LOCALE', 'LOCK',
                'LOWLIGHT', 'MANUAL', 'MEMORY', 'MINUS', 'MODE', 'MULTIPLE',
                'NATIONAL', 'NATIONAL-EDITED', 'NATIVE', 'NEGATIVE', 'NEXT', 'NO',
                'NULL', 'NULLS', 'NUMBER', 'NUMBERS', 'NUMERIC', 'NUMERIC-EDITED',
                'OBJECT-COMPUTER', 'OCCURS', 'OF', 'OFF', 'OMITTED', 'ON', 'ONLY',
                'OPTIONAL', 'ORDER', 'ORGANIZATION', 'OTHER', 'OUTPUT', 'OVERFLOW',
                'OVERLINE', 'PACKED-DECIMAL', 'PADDING', 'PAGE', 'PARAGRAPH',
                'PLUS', 'POINTER', 'POSITION', 'POSITIVE', 'PRESENT', 'PREVIOUS',
                'PRINTER', 'PRINTING', 'PROCEDURE-POINTER', 'PROCEDURES',
                'PROCEED', 'PROGRAM', 'PROGRAM-POINTER', 'PROMPT', 'QUOTE',
                'QUOTES', 'RANDOM', 'RD', 'RECORD', 'RECORDING', 'RECORDS', 'RECURSIVE',
                'REDEFINES', 'REEL', 'REFERENCE', 'RELATIVE', 'REMAINDER', 'REMOVAL',
                'RENAMES', 'REPLACING', 'REPORT', 'REPORTING', 'REPORTS', 'REPOSITORY',
                'REQUIRED', 'RESERVE', 'RETURNING', 'REVERSE-VIDEO', 'REWIND',
                'RIGHT', 'ROLLBACK', 'ROUNDED', 'RUN', 'SAME', 'SCROLL',
                'SECURE', 'SEGMENT-LIMIT', 'SELECT', 'SENTENCE', 'SEPARATE',
                'SEQUENCE', 'SEQUENTIAL', 'SHARING', 'SIGN', 'SIGNED', 'SIGNED-INT',
                'SIGNED-LONG', 'SIGNED-SHORT', 'SIZE', 'SORT-MERGE', 'SOURCE',
                'SOURCE-COMPUTER', 'SPECIAL-NAMES', 'STANDARD',
                'STANDARD-1', 'STANDARD-2', 'STATUS', 'SUBKEY', 'SUM',
                'SYMBOLIC', 'SYNC', 'SYNCHRONIZED', 'TALLYING', 'TAPE',
                'TEST', 'THROUGH', 'THRU', 'TIME', 'TIMES', 'TO', 'TOP', 'TRAILING',
                'TRANSFORM', 'TYPE', 'UNDERLINE', 'UNIT', 'UNSIGNED',
                'UNSIGNED-INT', 'UNSIGNED-LONG', 'UNSIGNED-SHORT', 'UNTIL', 'UP',
                'UPDATE', 'UPON', 'USAGE', 'USING', 'VALUE', 'VALUES', 'VARYING',
                'WAIT', 'WHEN', 'WITH', 'WORDS', 'YYYYDDD', 'YYYYMMDD'),
                prefix=r'(^|(?<=[^\w\-]))', suffix=r'\s*($|(?=[^\w\-]))'),
             Keyword.Pseudo),

            # inactive reserved words
            (words((
                'ACTIVE-CLASS', 'ALIGNED', 'ANYCASE', 'ARITHMETIC', 'ATTRIBUTE',
                'B-AND', 'B-NOT', 'B-OR', 'B-XOR', 'BIT', 'BOOLEAN', 'CD', 'CENTER',
                'CF', 'CH', 'CHAIN', 'CLASS-ID', 'CLASSIFICATION', 'COMMUNICATION',
                'CONDITION', 'DATA-POINTER', 'DESTINATION', 'DISABLE', 'EC', 'EGI',
                'EMI', 'ENABLE', 'END-RECEIVE', 'ENTRY-CONVENTION', 'EO', 'ESI',
                'EXCEPTION-OBJECT', 'EXPANDS', 'FACTORY', 'FLOAT-BINARY-16',
                'FLOAT-BINARY-34', 'FLOAT-BINARY-7', 'FLOAT-DECIMAL-16',
                'FLOAT-DECIMAL-34', 'FLOAT-EXTENDED', 'FORMAT', 'FUNCTION-POINTER',
                'GET', 'GROUP-USAGE', 'IMPLEMENTS', 'INFINITY', 'INHERITS',
                'INTERFACE', 'INTERFACE-ID', 'INVOKE', 'LC_ALL', 'LC_COLLATE',
                'LC_CTYPE', 'LC_MESSAGES', 'LC_MONETARY', 'LC_NUMERIC', 'LC_TIME',
                'LINE-COUNTER', 'MESSAGE', 'METHOD', 'METHOD-ID', 'NESTED', 'NONE',
                'NORMAL', 'OBJECT', 'OBJECT-REFERENCE', 'OPTIONS', 'OVERRIDE',
                'PAGE-COUNTER', 'PF', 'PH', 'PROPERTY', 'PROTOTYPE', 'PURGE',
                'QUEUE', 'RAISE', 'RAISING', 'RECEIVE', 'RELATION', 'REPLACE',
                'REPRESENTS-NOT-A-NUMBER', 'RESET', 'RESUME', 'RETRY', 'RF', 'RH',
                'SECONDS', 'SEGMENT', 'SELF', 'SEND', 'SOURCES', 'STATEMENT',
                'STEP', 'STRONG', 'SUB-QUEUE-1', 'SUB-QUEUE-2', 'SUB-QUEUE-3',
                'SUPER', 'SYMBOL', 'SYSTEM-DEFAULT', 'TABLE', 'TERMINAL', 'TEXT',
                'TYPEDEF', 'UCS-4', 'UNIVERSAL', 'USER-DEFAULT', 'UTF-16', 'UTF-8',
                'VAL-STATUS', 'VALID', 'VALIDATE', 'VALIDATE-STATUS'),
                   prefix=r'(^|(?<=[^\w\-]))', suffix=r'\s*($|(?=[^\w\-]))'),
             Error),

            # Data Types
            (r'(^|(?<=[^\w\-]))'
             r'(PIC\s+.+?(?=(\s|\.\s))|PICTURE\s+.+?(?=(\s|\.\s))|'
             r'(COMPUTATIONAL)(-[1-5X])?|(COMP)(-[1-5X])?|'
             r'BINARY-C-LONG|'
             r'BINARY-CHAR|BINARY-DOUBLE|BINARY-LONG|BINARY-SHORT|'
             r'BINARY)\s*($|(?=[^\w\-]))', Keyword.Type),

            # Operators
            (r'(\*\*|\*|\+|-|/|<=|>=|<|>|==|/=|=)', Operator),

            # (r'(::)', Keyword.Declaration),

            (r'([(),;:&%.])', Punctuation),

            # Intrinsics
            (r'(^|(?<=[^\w\-]))(ABS|ACOS|ANNUITY|ASIN|ATAN|BYTE-LENGTH|'
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
             r'($|(?=[^\w\-]))', Name.Function),

            # Booleans
            (r'(^|(?<=[^\w\-]))(true|false)\s*($|(?=[^\w\-]))', Name.Builtin),
            # Comparing Operators
            (r'(^|(?<=[^\w\-]))(equal|equals|ne|lt|le|gt|ge|'
             r'greater|less|than|not|and|or)\s*($|(?=[^\w\-]))', Operator.Word),
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
            (r'[+-]?\d*\.\d+(E[-+]?\d+)?', Number.Float),
            (r'[+-]?\d+\.\d*(E[-+]?\d+)?', Number.Float),
        ],
    }


class CobolFreeformatLexer(CobolLexer):
    """
    Lexer for Free format OpenCOBOL code.
    """
    name = 'COBOLFree'
    aliases = ['cobolfree']
    filenames = ['*.cbl', '*.CBL']
    mimetypes = []
    url = 'https://opencobol.add1tocobol.com'
    version_added = '1.6'

    flags = re.IGNORECASE | re.MULTILINE

    tokens = {
        'comment': [
            (r'(\*>.*\n|^\w*\*.*$)', Comment),
        ],
    }


class ABAPLexer(RegexLexer):
    """
    Lexer for ABAP, SAP's integrated language.
    """
    name = 'ABAP'
    aliases = ['abap']
    filenames = ['*.abap', '*.ABAP']
    mimetypes = ['text/x-abap']
    url = 'https://community.sap.com/topics/abap'
    version_added = '1.1'

    flags = re.IGNORECASE | re.MULTILINE

    tokens = {
        'common': [
            (r'\s+', Whitespace),
            (r'^\*.*$', Comment.Single),
            (r'\".*?\n', Comment.Single),
            (r'##\w+', Comment.Special),
        ],
        'variable-names': [
            (r'<\S+>', Name.Variable),
            (r'\w[\w~]*(?:(\[\])|->\*)?', Name.Variable),
        ],
        'root': [
            include('common'),
            # function calls
            (r'CALL\s+(?:BADI|CUSTOMER-FUNCTION|FUNCTION)',
             Keyword),
            (r'(CALL\s+(?:DIALOG|SCREEN|SUBSCREEN|SELECTION-SCREEN|'
             r'TRANSACTION|TRANSFORMATION))\b',
             Keyword),
            (r'(FORM|PERFORM)(\s+)(\w+)',
             bygroups(Keyword, Whitespace, Name.Function)),
            (r'(PERFORM)(\s+)(\()(\w+)(\))',
             bygroups(Keyword, Whitespace, Punctuation, Name.Variable, Punctuation)),
            (r'(MODULE)(\s+)(\S+)(\s+)(INPUT|OUTPUT)',
             bygroups(Keyword, Whitespace, Name.Function, Whitespace, Keyword)),

            # method implementation
            (r'(METHOD)(\s+)([\w~]+)',
             bygroups(Keyword, Whitespace, Name.Function)),
            # method calls
            (r'(\s+)([\w\-]+)([=\-]>)([\w\-~]+)',
             bygroups(Whitespace, Name.Variable, Operator, Name.Function)),
            # call methodnames returning style
            (r'(?<=[=-]>)([\w\-~]+)(?=\()', Name.Function),

            # text elements
            (r'(TEXT)(-)(\d{3})',
             bygroups(Keyword, Punctuation, Number.Integer)),
            (r'(TEXT)(-)(\w{3})',
             bygroups(Keyword, Punctuation, Name.Variable)),

            # keywords with dashes in them.
            # these need to be first, because for instance the -ID part
            # of MESSAGE-ID wouldn't get highlighted if MESSAGE was
            # first in the list of keywords.
            (r'(ADD-CORRESPONDING|AUTHORITY-CHECK|'
             r'CLASS-DATA|CLASS-EVENTS|CLASS-METHODS|CLASS-POOL|'
             r'DELETE-ADJACENT|DIVIDE-CORRESPONDING|'
             r'EDITOR-CALL|ENHANCEMENT-POINT|ENHANCEMENT-SECTION|EXIT-COMMAND|'
             r'FIELD-GROUPS|FIELD-SYMBOLS|FIELD-SYMBOL|FUNCTION-POOL|'
             r'INTERFACE-POOL|INVERTED-DATE|'
             r'LOAD-OF-PROGRAM|LOG-POINT|'
             r'MESSAGE-ID|MOVE-CORRESPONDING|MULTIPLY-CORRESPONDING|'
             r'NEW-LINE|NEW-PAGE|NEW-SECTION|NO-EXTENSION|'
             r'OUTPUT-LENGTH|PRINT-CONTROL|'
             r'SELECT-OPTIONS|START-OF-SELECTION|SUBTRACT-CORRESPONDING|'
             r'SYNTAX-CHECK|SYSTEM-EXCEPTIONS|'
             r'TYPE-POOL|TYPE-POOLS|NO-DISPLAY'
             r')\b', Keyword),

            # keyword kombinations
            (r'(?<![-\>])(CREATE\s+(PUBLIC|PRIVATE|DATA|OBJECT)|'
             r'(PUBLIC|PRIVATE|PROTECTED)\s+SECTION|'
             r'(TYPE|LIKE)\s+((LINE\s+OF|REF\s+TO|'
             r'(SORTED|STANDARD|HASHED)\s+TABLE\s+OF))?|'
             r'FROM\s+(DATABASE|MEMORY)|CALL\s+METHOD|'
             r'(GROUP|ORDER) BY|HAVING|SEPARATED BY|'
             r'GET\s+(BADI|BIT|CURSOR|DATASET|LOCALE|PARAMETER|'
             r'PF-STATUS|(PROPERTY|REFERENCE)\s+OF|'
             r'RUN\s+TIME|TIME\s+(STAMP)?)?|'
             r'SET\s+(BIT|BLANK\s+LINES|COUNTRY|CURSOR|DATASET|EXTENDED\s+CHECK|'
             r'HANDLER|HOLD\s+DATA|LANGUAGE|LEFT\s+SCROLL-BOUNDARY|'
             r'LOCALE|MARGIN|PARAMETER|PF-STATUS|PROPERTY\s+OF|'
             r'RUN\s+TIME\s+(ANALYZER|CLOCK\s+RESOLUTION)|SCREEN|'
             r'TITLEBAR|UPADTE\s+TASK\s+LOCAL|USER-COMMAND)|'
             r'CONVERT\s+((INVERTED-)?DATE|TIME|TIME\s+STAMP|TEXT)|'
             r'(CLOSE|OPEN)\s+(DATASET|CURSOR)|'
             r'(TO|FROM)\s+(DATA BUFFER|INTERNAL TABLE|MEMORY ID|'
             r'DATABASE|SHARED\s+(MEMORY|BUFFER))|'
             r'DESCRIBE\s+(DISTANCE\s+BETWEEN|FIELD|LIST|TABLE)|'
             r'FREE\s(MEMORY|OBJECT)?|'
             r'PROCESS\s+(BEFORE\s+OUTPUT|AFTER\s+INPUT|'
             r'ON\s+(VALUE-REQUEST|HELP-REQUEST))|'
             r'AT\s+(LINE-SELECTION|USER-COMMAND|END\s+OF|NEW)|'
             r'AT\s+SELECTION-SCREEN(\s+(ON(\s+(BLOCK|(HELP|VALUE)-REQUEST\s+FOR|'
             r'END\s+OF|RADIOBUTTON\s+GROUP))?|OUTPUT))?|'
             r'SELECTION-SCREEN:?\s+((BEGIN|END)\s+OF\s+((TABBED\s+)?BLOCK|LINE|'
             r'SCREEN)|COMMENT|FUNCTION\s+KEY|'
             r'INCLUDE\s+BLOCKS|POSITION|PUSHBUTTON|'
             r'SKIP|ULINE)|'
             r'LEAVE\s+(LIST-PROCESSING|PROGRAM|SCREEN|'
             r'TO LIST-PROCESSING|TO TRANSACTION)'
             r'(ENDING|STARTING)\s+AT|'
             r'FORMAT\s+(COLOR|INTENSIFIED|INVERSE|HOTSPOT|INPUT|FRAMES|RESET)|'
             r'AS\s+(CHECKBOX|SUBSCREEN|WINDOW)|'
             r'WITH\s+(((NON-)?UNIQUE)?\s+KEY|FRAME)|'
             r'(BEGIN|END)\s+OF|'
             r'DELETE(\s+ADJACENT\s+DUPLICATES\sFROM)?|'
             r'COMPARING(\s+ALL\s+FIELDS)?|'
             r'(INSERT|APPEND)(\s+INITIAL\s+LINE\s+(IN)?TO|\s+LINES\s+OF)?|'
             r'IN\s+((BYTE|CHARACTER)\s+MODE|PROGRAM)|'
             r'END-OF-(DEFINITION|PAGE|SELECTION)|'
             r'WITH\s+FRAME(\s+TITLE)|'
             r'(REPLACE|FIND)\s+((FIRST|ALL)\s+OCCURRENCES?\s+OF\s+)?(SUBSTRING|REGEX)?|'
             r'MATCH\s+(LENGTH|COUNT|LINE|OFFSET)|'
             r'(RESPECTING|IGNORING)\s+CASE|'
             r'IN\s+UPDATE\s+TASK|'
             r'(SOURCE|RESULT)\s+(XML)?|'
             r'REFERENCE\s+INTO|'

             # simple kombinations
             r'AND\s+(MARK|RETURN)|CLIENT\s+SPECIFIED|CORRESPONDING\s+FIELDS\s+OF|'
             r'IF\s+FOUND|FOR\s+EVENT|INHERITING\s+FROM|LEAVE\s+TO\s+SCREEN|'
             r'LOOP\s+AT\s+(SCREEN)?|LOWER\s+CASE|MATCHCODE\s+OBJECT|MODIF\s+ID|'
             r'MODIFY\s+SCREEN|NESTING\s+LEVEL|NO\s+INTERVALS|OF\s+STRUCTURE|'
             r'RADIOBUTTON\s+GROUP|RANGE\s+OF|REF\s+TO|SUPPRESS DIALOG|'
             r'TABLE\s+OF|UPPER\s+CASE|TRANSPORTING\s+NO\s+FIELDS|'
             r'VALUE\s+CHECK|VISIBLE\s+LENGTH|HEADER\s+LINE|COMMON\s+PART)\b', Keyword),

            # single word keywords.
            (r'(^|(?<=(\s|\.)))(ABBREVIATED|ABSTRACT|ADD|ALIASES|ALIGN|ALPHA|'
             r'ASSERT|AS|ASSIGN(ING)?|AT(\s+FIRST)?|'
             r'BACK|BLOCK|BREAK-POINT|'
             r'CASE|CAST|CATCH|CHANGING|CHECK|CLASS|CLEAR|COLLECT|COLOR|COMMIT|COND|CONV|'
             r'CREATE|COMMUNICATION|COMPONENTS?|COMPUTE|CONCATENATE|CONDENSE|'
             r'CONSTANTS|CONTEXTS|CONTINUE|CONTROLS|COUNTRY|CURRENCY|'
             r'DATA|DATE|DECIMALS|DEFAULT|DEFINE|DEFINITION|DEFERRED|DEMAND|'
             r'DETAIL|DIRECTORY|DIVIDE|DO|DUMMY|'
             r'ELSE(IF)?|ENDAT|ENDCASE|ENDCATCH|ENDCLASS|ENDDO|ENDFORM|ENDFUNCTION|'
             r'ENDIF|ENDINTERFACE|ENDLOOP|ENDMETHOD|ENDMODULE|ENDSELECT|ENDTRY|ENDWHILE|'
             r'ENHANCEMENT|EVENTS|EXACT|EXCEPTIONS?|EXIT|EXPONENT|EXPORT|EXPORTING|EXTRACT|'
             r'FETCH|FIELDS?|FOR|FORM|FORMAT|FREE|FROM|FUNCTION|'
             r'HIDE|'
             r'ID|IF|IMPORT|IMPLEMENTATION|IMPORTING|IN|INCLUDE|INCLUDING|'
             r'INDEX|INFOTYPES|INITIALIZATION|INTERFACE|INTERFACES|INTO|'
             r'LANGUAGE|LEAVE|LENGTH|LINES|LOAD|LOCAL|'
             r'JOIN|'
             r'KEY|'
             r'NEW|NEXT|'
             r'MAXIMUM|MESSAGE|METHOD[S]?|MINIMUM|MODULE|MODIFIER|MODIFY|MOVE|MULTIPLY|'
             r'NODES|NUMBER|'
             r'OBLIGATORY|OBJECT|OF|OFF|ON|OTHERS|OVERLAY|'
             r'PACK|PAD|PARAMETERS|PERCENTAGE|POSITION|PROGRAM|PROVIDE|PUBLIC|PUT|PF\d\d|'
             r'RAISE|RAISING|RANGES?|READ|RECEIVE|REDEFINITION|REFRESH|REJECT|REPORT|RESERVE|'
             r'REF|RESUME|RETRY|RETURN|RETURNING|RIGHT|ROLLBACK|REPLACE|'
             r'SCROLL|SEARCH|SELECT|SHIFT|SIGN|SINGLE|SIZE|SKIP|SORT|SPLIT|STATICS|STOP|'
             r'STYLE|SUBMATCHES|SUBMIT|SUBTRACT|SUM(?!\()|SUMMARY|SUMMING|SUPPLY|SWITCH|'
             r'TABLE|TABLES|TIMESTAMP|TIMES?|TIMEZONE|TITLE|\??TO|'
             r'TOP-OF-PAGE|TRANSFER|TRANSLATE|TRY|TYPES|'
             r'ULINE|UNDER|UNPACK|UPDATE|USING|'
             r'VALUE|VALUES|VIA|VARYING|VARY|'
             r'WAIT|WHEN|WHERE|WIDTH|WHILE|WITH|WINDOW|WRITE|XSD|ZERO)\b', Keyword),

            # builtins
            (r'(abs|acos|asin|atan|'
             r'boolc|boolx|bit_set|'
             r'char_off|charlen|ceil|cmax|cmin|condense|contains|'
             r'contains_any_of|contains_any_not_of|concat_lines_of|cos|cosh|'
             r'count|count_any_of|count_any_not_of|'
             r'dbmaxlen|distance|'
             r'escape|exp|'
             r'find|find_end|find_any_of|find_any_not_of|floor|frac|from_mixed|'
             r'insert|'
             r'lines|log|log10|'
             r'match|matches|'
             r'nmax|nmin|numofchar|'
             r'repeat|replace|rescale|reverse|round|'
             r'segment|shift_left|shift_right|sign|sin|sinh|sqrt|strlen|'
             r'substring|substring_after|substring_from|substring_before|substring_to|'
             r'tan|tanh|to_upper|to_lower|to_mixed|translate|trunc|'
             r'xstrlen)(\()\b', bygroups(Name.Builtin, Punctuation)),

            (r'&[0-9]', Name),
            (r'[0-9]+', Number.Integer),

            # operators which look like variable names before
            # parsing variable names.
            (r'(?<=(\s|.))(AND|OR|EQ|NE|GT|LT|GE|LE|CO|CN|CA|NA|CS|NOT|NS|CP|NP|'
             r'BYTE-CO|BYTE-CN|BYTE-CA|BYTE-NA|BYTE-CS|BYTE-NS|'
             r'IS\s+(NOT\s+)?(INITIAL|ASSIGNED|REQUESTED|BOUND))\b', Operator.Word),

            include('variable-names'),

            # standard operators after variable names,
            # because < and > are part of field symbols.
            (r'[?*<>=\-+&]', Operator),
            (r"'(''|[^'])*'", String.Single),
            (r"`([^`])*`", String.Single),
            (r"([|}])([^{}|]*?)([|{])",
             bygroups(Punctuation, String.Single, Punctuation)),
            (r'[/;:()\[\],.]', Punctuation),
            (r'(!)(\w+)', bygroups(Operator, Name)),
        ],
    }


class OpenEdgeLexer(RegexLexer):
    """
    Lexer for OpenEdge ABL (formerly Progress) source code.
    """
    name = 'OpenEdge ABL'
    aliases = ['openedge', 'abl', 'progress']
    filenames = ['*.p', '*.cls']
    mimetypes = ['text/x-openedge', 'application/x-openedge']
    url = 'https://www.progress.com/openedge/features/abl'
    version_added = '1.5'

    types = (r'(?i)(^|(?<=[^\w\-]))(CHARACTER|CHAR|CHARA|CHARAC|CHARACT|CHARACTE|'
             r'COM-HANDLE|DATE|DATETIME|DATETIME-TZ|'
             r'DECIMAL|DEC|DECI|DECIM|DECIMA|HANDLE|'
             r'INT64|INTEGER|INT|INTE|INTEG|INTEGE|'
             r'LOGICAL|LONGCHAR|MEMPTR|RAW|RECID|ROWID)\s*($|(?=[^\w\-]))')

    keywords = words(OPENEDGEKEYWORDS,
                     prefix=r'(?i)(^|(?<=[^\w\-]))',
                     suffix=r'\s*($|(?=[^\w\-]))')

    tokens = {
        'root': [
            (r'/\*', Comment.Multiline, 'comment'),
            (r'\{', Comment.Preproc, 'preprocessor'),
            (r'\s*&.*', Comment.Preproc),
            (r'0[xX][0-9a-fA-F]+[LlUu]*', Number.Hex),
            (r'(?i)(DEFINE|DEF|DEFI|DEFIN)\b', Keyword.Declaration),
            (types, Keyword.Type),
            (keywords, Name.Builtin),
            (r'"(\\\\|\\[^\\]|[^"\\])*"', String.Double),
            (r"'(\\\\|\\[^\\]|[^'\\])*'", String.Single),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'[0-9]+', Number.Integer),
            (r'\s+', Whitespace),
            (r'[+*/=-]', Operator),
            (r'[.:()]', Punctuation),
            (r'.', Name.Variable),  # Lazy catch-all
        ],
        'comment': [
            (r'[^*/]', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline)
        ],
        'preprocessor': [
            (r'[^{}]', Comment.Preproc),
            (r'\{', Comment.Preproc, '#push'),
            (r'\}', Comment.Preproc, '#pop'),
        ],
    }

    def analyse_text(text):
        """Try to identify OpenEdge ABL based on a few common constructs."""
        result = 0

        if 'END.' in text:
            result += 0.05

        if 'END PROCEDURE.' in text:
            result += 0.05

        if 'ELSE DO:' in text:
            result += 0.05

        return result


class GoodDataCLLexer(RegexLexer):
    """
    Lexer for GoodData-CL script files.
    """

    name = 'GoodData-CL'
    aliases = ['gooddata-cl']
    filenames = ['*.gdc']
    mimetypes = ['text/x-gooddata-cl']
    url = 'https://github.com/gooddata/GoodData-CL'
    version_added = '1.4'

    flags = re.IGNORECASE

    # Syntax:
    # https://github.com/gooddata/GoodData-CL/raw/master/cli/src/main/resources/com/gooddata/processor/COMMANDS.txt
    tokens = {
        'root': [
            # Comments
            (r'#.*', Comment.Single),
            # Function call
            (r'[a-z]\w*', Name.Function),
            # Argument list
            (r'\(', Punctuation, 'args-list'),
            # Punctuation
            (r';', Punctuation),
            # Space is not significant
            (r'\s+', Text)
        ],
        'args-list': [
            (r'\)', Punctuation, '#pop'),
            (r',', Punctuation),
            (r'[a-z]\w*', Name.Variable),
            (r'=', Operator),
            (r'"', String, 'string-literal'),
            (r'[0-9]+(?:\.[0-9]+)?(?:e[+-]?[0-9]{1,3})?', Number),
            # Space is not significant
            (r'\s', Whitespace)
        ],
        'string-literal': [
            (r'\\[tnrfbae"\\]', String.Escape),
            (r'"', String, '#pop'),
            (r'[^\\"]+', String)
        ]
    }


class MaqlLexer(RegexLexer):
    """
    Lexer for GoodData MAQL scripts.
    """

    name = 'MAQL'
    aliases = ['maql']
    filenames = ['*.maql']
    mimetypes = ['text/x-gooddata-maql', 'application/x-gooddata-maql']
    url = 'https://help.gooddata.com/doc/enterprise/en/dashboards-and-insights/maql-analytical-query-language'
    version_added = '1.4'

    flags = re.IGNORECASE
    tokens = {
        'root': [
            # IDENTITY
            (r'IDENTIFIER\b', Name.Builtin),
            # IDENTIFIER
            (r'\{[^}]+\}', Name.Variable),
            # NUMBER
            (r'[0-9]+(?:\.[0-9]+)?(?:e[+-]?[0-9]{1,3})?', Number),
            # STRING
            (r'"', String, 'string-literal'),
            #  RELATION
            (r'\<\>|\!\=', Operator),
            (r'\=|\>\=|\>|\<\=|\<', Operator),
            # :=
            (r'\:\=', Operator),
            # OBJECT
            (r'\[[^]]+\]', Name.Variable.Class),
            # keywords
            (words((
                'DIMENSION', 'DIMENSIONS', 'BOTTOM', 'METRIC', 'COUNT', 'OTHER',
                'FACT', 'WITH', 'TOP', 'OR', 'ATTRIBUTE', 'CREATE', 'PARENT',
                'FALSE', 'ROW', 'ROWS', 'FROM', 'ALL', 'AS', 'PF', 'COLUMN',
                'COLUMNS', 'DEFINE', 'REPORT', 'LIMIT', 'TABLE', 'LIKE', 'AND',
                'BY', 'BETWEEN', 'EXCEPT', 'SELECT', 'MATCH', 'WHERE', 'TRUE',
                'FOR', 'IN', 'WITHOUT', 'FILTER', 'ALIAS', 'WHEN', 'NOT', 'ON',
                'KEYS', 'KEY', 'FULLSET', 'PRIMARY', 'LABELS', 'LABEL',
                'VISUAL', 'TITLE', 'DESCRIPTION', 'FOLDER', 'ALTER', 'DROP',
                'ADD', 'DATASET', 'DATATYPE', 'INT', 'BIGINT', 'DOUBLE', 'DATE',
                'VARCHAR', 'DECIMAL', 'SYNCHRONIZE', 'TYPE', 'DEFAULT', 'ORDER',
                'ASC', 'DESC', 'HYPERLINK', 'INCLUDE', 'TEMPLATE', 'MODIFY'),
                suffix=r'\b'),
             Keyword),
            # FUNCNAME
            (r'[a-z]\w*\b', Name.Function),
            # Comments
            (r'#.*', Comment.Single),
            # Punctuation
            (r'[,;()]', Punctuation),
            # Space is not significant
            (r'\s+', Whitespace)
        ],
        'string-literal': [
            (r'\\[tnrfbae"\\]', String.Escape),
            (r'"', String, '#pop'),
            (r'[^\\"]+', String)
        ],
    }




# A PL/I identifier isn't limited to plain word characters. Per IBM's
# Enterprise PL/I Language Reference, Chapter 2 ("Alphabetic and
# extralingual characters" / "Identifiers"): the first character must
# be an alphabetic or "extralingual" character (IBM's own term for
# "#", "@", and "$") or, for an INTERNAL symbol, the break character
# "_"; subsequent characters may be alphabetic, extralingual, digit, or
# "_". Confirmed missing by real-world testing -- see the module
# docstring above for the specific real files this broke on.
_SYMBOL_START = r"[a-z_#@$]"
_SYMBOL_CHAR = r"[\w#@$]"
_SYMBOL = _SYMBOL_START + _SYMBOL_CHAR + r"*"


class PLILexer(RegexLexer):
    """
    PL/I is IBM's general-purpose, case-insensitive programming language,
    originally developed for mainframe use and still current in IBM
    Enterprise PL/I for z/OS. This lexer covers the classic/mainframe
    dialect; it has never previously existed in Pygments.
    """

    name = "PL/I"
    url = "https://www.ibm.com/docs/en/epfz"
    aliases = ["pli", "pl1"]
    filenames = ["*.pli", "*.pl1", "*.plx"]
    mimetypes = []
    version_added = "2.22"
    flags = re.IGNORECASE

    tokens = {
        "root": [
            (r"\s+", Whitespace),
            # A trailing Ctrl-Z (ASCII SUB, U+001A) is a legacy DOS/
            # mainframe-file-transfer end-of-file marker byte, not PL/I
            # source -- found trailing the real END statement in
            # code_samples/X501AA.PLI (zowe-pli-language-support
            # corpus), a plausible artifact in any PL/I file that has
            # passed through an old mainframe-to-PC transfer. Treated
            # as insignificant Text rather than an Error token.
            (r"\x1a", Text),
            (r"/\*", Comment.Multiline, "comment"),
            # Preprocessor ("macro facility") statements: %INCLUDE, %DCL,
            # %IF, %ACTIVATE, etc. This wildcard's coverage was checked
            # against IBM's complete, real "Preprocessor statements"
            # alphabetic index
            # (https://www.ibm.com/docs/en/SSY2V3_6.2/lr/prepst.html) --
            # every real preprocessor statement keyword (%ACTIVATE,
            # %DEACTIVATE, %DECLARE, %DO/%END, %GO TO, %IF, %INCLUDE,
            # %INSCAN, %ITERATE, %LEAVE, %NOTE, %REPLACE, %SELECT,
            # %XINCLUDE, %XINSCAN, and the unsupported-but-still-accepted
            # %CONTROL) already matches this wildcard, so this is now a
            # verified-complete design choice, not merely a
            # never-revisited placeholder. The %GO TO statement is the
            # one case not tokenized as a single unit (it becomes
            # Comment.Preproc("%GO") + Keyword.Reserved("TO") separately,
            # since "to" is already a recognized clause keyword) -- a
            # known minor cosmetic gap, not a correctness one.
            (r"%[a-z_]\w*", Comment.Preproc),
            # The %null statement (a bare "%;", the preprocessor
            # equivalent of a plain ";") is real and documented on that
            # same page -- confirmed as a genuine bug via direct testing
            # before this fix: a lone "%" not followed by a letter
            # matched no rule at all and fell through to an Error token.
            (r"%", Comment.Preproc),
            # Embedded EXEC SQL / EXEC CICS statements -- see the module
            # docstring's "Embedded EXEC SQL / EXEC CICS" section for the
            # full design rationale and how each decision was checked
            # against the real-world corpus. Enter a dedicated "exec"
            # state that brackets the region and leaves it on the
            # terminating ";" (or "END-EXEC"), rather than lexing the
            # embedded sub-language with PL/I's own rules.
            (
                r"(exec)(\s+)(sql|cics)\b",
                bygroups(Keyword.Reserved, Whitespace, Keyword.Reserved),
                "exec",
            ),
            # Bit-string and hex-string constants: '1010'B, '1F'X. These
            # must come before the generic character-string rule, since
            # both start with the same quote character -- only the
            # trailing radix letter distinguishes them, so order matters
            # (RegexLexer takes the first matching rule, not the most
            # specific).
            (r"'[01]+'B", Number.Bin),
            (r"'[0-9A-F]+'X", Number.Hex),
            (r"'", String, "string"),
            # Double-quoted strings: not used for ordinary PL/I
            # character constants (always single-quoted -- see the
            # vocabulary-sourcing note in the module docstring), but
            # real source uses them as the %INCLUDE file-spec argument
            # (e.g. `%INCLUDE "b.pli";`) -- see the module docstring for
            # sourcing. Accepted generically here (not scoped to just
            # after %INCLUDE) since ordinary PL/I code never uses a bare
            # '"' at all, so there's no real ambiguity to introduce.
            (r'"', String, "string_double"),
            # Numeric literals: decimal fixed (123, 123.45) and decimal
            # float with an exponent (1.5E10, 1.5E+10, 1.5E-10).
            (r"[0-9]+\.[0-9]+[Ee][+-]?[0-9]+", Number.Float),
            (r"[0-9]+[Ee][+-]?[0-9]+", Number.Float),
            (r"[0-9]+\.[0-9]+", Number.Float),
            (r"[0-9]+", Number.Integer),
            # Labels: identifier immediately followed by a colon on the
            # same line, e.g. "loop: DO ...;". Same-line whitespace only
            # ([ \t]*, not \s*) -- see the ooRexx lexer's own fix for
            # why a bare \s* here would be a real bug, not just style:
            # it can cross a newline and swallow a token meant for the
            # following line.
            (
                r"(" + _SYMBOL + r")([ \t]*)(:)",
                bygroups(Name.Label, Whitespace, Punctuation),
            ),
            # ¬/^: settled per the module docstring above. ¬=, ¬<, ¬>
            # (the atomic negated-relational operators) must be listed
            # before the bare ¬/^ rule, so they aren't split into two
            # tokens. The final bare rule below is intentionally
            # position-agnostic: it matches ¬ (or ^) whenever not
            # immediately followed by =/</>, which covers BOTH real
            # grammatical positions ¬ has -- prefix (logical NOT) and
            # infix (bitwise XOR, e.g. "A ¬ B") -- as a single generic
            # Operator token either way. Disambiguating which semantic
            # meaning applies is a parser-level, position-dependent
            # concern, not a lexer one.
            (r"[¬^]=", Operator),
            (r"[¬^]<", Operator),
            (r"[¬^]>", Operator),
            (r"[¬^]", Operator),
            include("operator"),
            include("attribute"),
            include("keyword"),
            include("function"),
            (_SYMBOL, Text),
        ],
        "operator": [
            # Compound assignment operators (+=, -=, *=, /=, |=, &=,
            # ||=, **=) and the locator-qualifier operators (->, =>,
            # pointer/handle-based member access, e.g. p->field) are
            # real IBM Enterprise PL/I additions confirmed directly
            # against current docs -- "Compound assignment statements"
            # (https://www.ibm.com/docs/en/epfz/6.2.0?topic=statements-compound-assignment)
            # and "Expressions and references"
            # (https://www.ibm.com/docs/en/epfz/6.2.0?topic=reference-expressions-references).
            # Longer sequences must be ordered before their own
            # prefixes (e.g. "**=" before "**" before "*=" before "*"),
            # since RegexLexer takes the first list entry that matches,
            # not the longest.
            (r"\*\*=|\*\*", Operator),
            # "!!" as an alternate spelling of "||" (concatenation):
            # IBM's Enterprise PL/I Language Reference, Chapter 2,
            # "Special characters", Note 1: "The or (|)... symbol[has]
            # variant code points. You can use the compiler option[]
            # OR... to define [an] alternate symbol to represent [this]
            # operator" -- the same code-page-variance mechanism the
            # module docstring above already documents for ¬/^. Found
            # in real-world source using exactly this alternate for
            # concatenation (POSREP = POSREP !! DEL !! REPTAB(I);, in
            # the zowe-pli-language-support corpus's X501AA.PLI) which
            # previously produced Error tokens on every "!". Scoped
            # narrowly to "!!" (composite concatenation), matching what
            # was actually observed, rather than treating a bare "!"
            # as a general alternate for "|" everywhere -- that broader
            # substitution isn't evidenced in any real source found.
            (r"\|\|=|!!=|\|\||!!", Operator),
            (r"->|=>", Operator),
            # <> is a documented alternate spelling of ¬= (not-equal in
            # ordinary comparisons; "exclusive-or and assign" in the
            # compound-assignment table specifically) -- confirmed on
            # both of the pages cited above.
            (r"<>", Operator),
            (r"[-+*/|&]=", Operator),
            # ":" is a generic punctuation/operator character, not just
            # part of a label. Two distinct, entirely standard PL/I
            # constructs use a bare ":" outside a label -- see the
            # module docstring for sourcing and real-world examples: an
            # array dimension's lower:upper bound pair (e.g.
            # "DCL A(0:1000) FIXED;") and a condition prefix (e.g.
            # "(NOZERODIVIDE): stmt;"). Both produced Error tokens
            # before this fix. The label rule above is listed earlier in
            # "root" and so still wins for the "identifier immediately
            # followed by a colon" shape it specifically matches.
            (r"[-+*/=<>&|.,;():]", Operator),
        ],
        "attribute": [
            # DCL attribute keywords -- see module docstring for the
            # specific IBM 6.2 pages each of these is sourced from.
            (
                words(
                    (
                        # "Data attributes" category index (35 names).
                        "area", "binary", "bit", "character", "complex",
                        "decimal", "dimension", "entry", "file", "fixed",
                        "float", "format", "graphic", "handle", "label",
                        "locates", "nonvarying", "offset", "ordinal",
                        "picture", "pointer", "precision", "real",
                        "returns", "signed", "structure", "task", "type",
                        "uchar", "unsigned", "union", "varying",
                        "varying4", "varyingz", "widechar", "widepic",
                        # "Storage control" chapter, named in prose.
                        "static", "automatic", "controlled", "based",
                        "assignable", "nonassignable", "normal",
                        "abnormal", "bigendian", "littleendian",
                        "hexadec", "ieee", "connected", "nonconnected",
                        "defined", "position", "initial",
                        # Individually-confirmed attribute pages.
                        "aligned", "unaligned", "internal", "external",
                        "builtin", "condition", "generic", "value",
                        # Two more full attribute names, missing from the
                        # sets above and confirmed missing by real-world
                        # testing (real corpus files declaring
                        # SEQUENTIAL/BUFFERED files): "sequential" and
                        # "buffered"/"unbuffered" (with "environment",
                        # confirmed as a real attribute in IBM's own
                        # "ENV (ENVIRONMENT) attribute" index entry, used
                        # on file declarations to pass device/access
                        # information to the operating system).
                        "sequential", "buffered", "unbuffered",
                        "environment",
                        # REFER: the self-defining-data option, real and
                        # separately confirmed ("REFER option
                        # (self-defining data)", and "For BASED data,
                        # length must be a restricted expression, unless
                        # the string is a member of a structure or a
                        # union and the REFER option is used"). Confirmed
                        # missing by real-world testing -- used in three
                        # of this project's own quickfix/ test files
                        # (DCL-ambiguity edge cases the corpus below is
                        # specifically testing), e.g.
                        # "DCL 1 A, 2 N BIN, 2 B(N) CHAR(1) REFER(N);".
                        "refer",
                        # IBM's own documented short forms for many of
                        # the names above -- confirmed directly from the
                        # Enterprise PL/I Language Reference's own index,
                        # which lists each as "FULLNAME (ABBREV)
                        # attribute" (Tables 9 and 12, "Abbreviations for
                        # coded arithmetic/string data attributes", plus
                        # the same "X (Y) attribute" pattern recurring
                        # through the rest of the index for the
                        # remainder). Confirmed missing by real-world
                        # testing: BIN, CHAR, VAR, PIC, and INIT in
                        # particular are used far more often than their
                        # full spellings in real mainframe PL/I (e.g.
                        # code_samples/CALC.pli, DDINFO.pli, CHART.pli,
                        # PLEAREP.pli in the corpus below all use them).
                        "auto", "bin", "buf", "char", "cplx", "conn",
                        "ctl", "dec", "def", "dim", "env", "ext", "init",
                        "int", "nonvar", "pic", "pos", "prec", "ptr",
                        "seql", "unbuf", "var", "varz", "wchar",
                    ),
                    suffix=r"\b",
                ),
                Keyword.Type,
            ),
        ],
        "keyword": [
            # Statement/directive keywords, from the complete alphabetic
            # "Statements and directives" index (see module docstring).
            # %-prefixed directives (INCLUDE, LINE, NOPRINT, etc.) are
            # already handled by the generic "%[a-z_]\w*" rule above and
            # so are deliberately not repeated here.
            (
                words(
                    (
                        "allocate", "assert", "attach", "begin", "call",
                        "cancel", "thread", "close", "declare", "dcl",
                        "default", "define", "alias", "delay", "delete",
                        "detach", "display", "do", "end", "exit", "fetch",
                        "flush", "free", "get", "go", "if", "then",
                        "else", "iterate", "leave", "locate", "null",
                        "on", "open", "otherwise", "package", "procedure",
                        "proc", "put", "qualify", "read", "reinit",
                        "release", "resignal", "return", "revert",
                        "rewrite", "select", "signal", "stop", "wait",
                        "when", "write", "xdeclare", "xdefine",
                        "xprocedure",
                        # Common clause keywords used within statements
                        # (DO ... TO ... BY ..., READ ... INTO(...)),
                        # not "statements" themselves in IBM's index but
                        # real reserved words -- "into" directly confirmed
                        # in IBM's own example: "read file(In) into(Input)".
                        "to", "by", "from", "into",
                        # DO-statement Type 2/3 do-group clause keywords,
                        # confirmed directly on the DO-statement syntax
                        # diagram (Enterprise PL/I Language Reference,
                        # Ch. 9): "DO WHILE(exp4); ... UNTIL(exp5)" for
                        # Type 2, and "specification: exp1 TO exp2
                        # WHILE(exp4) BY exp3 UNTIL(exp5) REPEAT exp6"
                        # plus UPTHRU/DOWNTHRU (an ordinal-range option,
                        # "Example of DO with UPTHRU and DOWNTHRU") for
                        # Type 3. Confirmed missing by real-world
                        # testing: WHILE alone appears throughout the
                        # corpus below (e.g. code_samples/FILE.pli,
                        # preprocessor/do.pli).
                        "while", "until", "repeat", "upthru", "downthru",
                        # GET/PUT statement data-specification and
                        # layout-control keywords, confirmed directly on
                        # the "Data specification options" and "Options
                        # of data transmission statements" syntax
                        # sections (same reference, Ch. 13): "If a GET or
                        # PUT statement includes a data list that is not
                        # preceded by one of the keywords LIST, DATA, or
                        # EDIT, LIST is the default" (also documents
                        # COPY); Table 34 "Options and format items for
                        # PRINT files" documents PAGE/LINE/SKIP/COLUMN as
                        # PUT statement options; STRING is documented
                        # separately as the GET/PUT STRING statement
                        # option. Confirmed missing by real-world
                        # testing: SKIP and LIST in particular are
                        # near-ubiquitous ("PUT SKIP LIST(...)" appears
                        # throughout the corpus below, e.g.
                        # code_samples/FILE.pli, INSERT.pli, PLI0000.pli).
                        "list", "data", "edit", "copy", "skip", "page",
                        "line", "column", "string",
                        # IGNORE: a documented data-transmission-
                        # statement option ("IGNORE option of data
                        # transmission statements") -- confirmed missing
                        # by real-world testing (code_samples/PTASK32.pli,
                        # PTASK34.pli both use "ON ERROR IGNORE").
                        "ignore",
                        # SYSTEM: the documented implicit-system-handling
                        # ON-unit action (e.g. "on finish system;", shown
                        # directly in IBM's own worked example in
                        # Chapter 17, "Conditions").
                        "system",
                        # PACKAGE-statement clause keywords, confirmed on
                        # IBM's "Packages" chapter
                        # (https://www.ibm.com/docs/en/SSY2V3_6.2/lr/lsh-package.html),
                        # whose syntax diagram and worked example use both
                        # -- "package-name: PACKAGE EXPORTS(...)
                        # RESERVES(...) OPTIONS(...); ... END package-name;".
                        # No dedicated lexer state is needed for PACKAGE the
                        # way ooRexx's ::CLASS/::METHOD needed one: unlike
                        # those, PACKAGE has no distinguishing sigil and no
                        # qualified-name/member-access syntax (it's a pure
                        # block-scoping construct -- exported names are
                        # referenced as ordinary external procedure names,
                        # not package.procedure-style) -- it uses the exact
                        # same generic keyword-statement grammar as
                        # PROCEDURE/DO/BEGIN, already handled. %PACKAGE is
                        # NOT a real distinct directive -- confirmed absent
                        # from the complete alphabetic "Statements and
                        # directives" index already pulled (see above).
                        "exports", "reserves",
                        # OPTIONS and RECURSIVE: real, extremely common
                        # PROCEDURE/ENTRY/BEGIN/PACKAGE-statement
                        # keywords, confirmed missing by real-world
                        # testing despite being present in nearly every
                        # real procedure (e.g. "PROC OPTIONS(MAIN);" is
                        # close to universal). Confirmed directly against
                        # IBM's Enterprise PL/I Language Reference's own
                        # syntax diagrams: "OPTIONS option and attribute"
                        # (Ch. 6, p.131) shows OPTIONS on PACKAGE,
                        # PROCEDURE, ENTRY, and BEGIN statements; the
                        # PROCEDURE-statement syntax diagram there
                        # separately shows "entry-label: PROCEDURE
                        # (parameter) returns-option OPTIONS(options)
                        # RECURSIVE scope-attribute;" -- RECURSIVE is its
                        # own attribute alongside, not inside, OPTIONS(),
                        # per "A procedure that is invoked recursively
                        # must have the RECURSIVE attribute specified in
                        # the PROCEDURE statement."
                        "options", "recursive",
                    ),
                    suffix=r"\b",
                ),
                Keyword.Reserved,
            ),
            # PROCEDURE/ENTRY/BEGIN/PACKAGE statement OPTIONS(...) values
            # -- the complete syntax-diagram list from the same "OPTIONS
            # option and attribute" reference section cited just above
            # (PROCEDURE-statement diagram, p.131-133): ASSEMBLER,
            # COBOL, FORTRAN, FETCHABLE, MAIN, NOEXECOPS, BYADDR,
            # BYVALUE, NOCHARGRAPHIC, CHARGRAPHIC, DESCRIPTOR,
            # NODESCRIPTOR, DLLINTERNAL, FROMALIEN, LINKAGE, NOMAP,
            # NOMAPIN, NOMAPOUT, NOINLINE, INLINE, ORDER, REORDER,
            # IRREDUCIBLE, REDUCIBLE, REENTRANT, RETCODE, WINMAIN, plus
            # their own documented abbreviations (ASSEMBLER -> ASM,
            # CHARGRAPHIC -> CHARG, NOCHARGRAPHIC -> NOCHARG, again per
            # that same reference section, not guessed). Confirmed
            # missing by real-world testing: MAIN and REORDER in
            # particular appear on the overwhelming majority of real
            # PROCEDURE statements in the corpus below (e.g.
            # code_samples/CALC.pli, CHART.pli, PDUMP/*.pli all use
            # "OPTIONS(MAIN)" or "OPTIONS(MAIN REORDER)"), and fell
            # through as plain Text before this fix.
            (
                words(
                    (
                        "assembler", "asm", "cobol", "fortran",
                        "fetchable", "main", "noexecops", "byaddr",
                        "byvalue", "nochargraphic", "nocharg",
                        "chargraphic", "charg", "descriptor",
                        "nodescriptor", "dllinternal", "fromalien",
                        "linkage", "nomap", "nomapin", "nomapout",
                        "noinline", "inline", "order", "reorder",
                        "irreducible", "reducible", "reentrant",
                        "retcode", "winmain",
                    ),
                    suffix=r"\b",
                ),
                Keyword.Reserved,
            ),
            # Condition names, used in ON/SIGNAL/REVERT statements and
            # condition prefixes (e.g. "ON ENDFILE(f) ...",
            # "(NOSIZE): stmt;"). This is the complete, exhaustive list
            # from Chapter 17, "Conditions", of the Enterprise PL/I
            # Language Reference, which covers exactly these 23 names in
            # alphabetic order (CONDITION itself, the 24th, is already
            # covered via the "attribute" state's CONDITION attribute
            # entry -- the two uses share the same word). Confirmed
            # missing by real-world testing: ENDFILE and CONVERSION in
            # particular appear directly in the corpus below (e.g.
            # code_samples/FILE.pli's "ON ENDFILE", MACROS.pli's "ON
            # CONVERSION").
            (
                words(
                    (
                        "anycondition", "area", "attention", "conversion",
                        "endfile", "endpage", "error", "finish",
                        "fixedoverflow", "invalidop", "key", "name",
                        "overflow", "record", "size", "storage",
                        "stringrange", "stringsize", "subscriptrange",
                        "transmit", "undefinedfile", "underflow",
                        "zerodivide",
                    ),
                    suffix=r"\b",
                ),
                Keyword.Reserved,
            ),
        ],
        "function": [
            # Built-in functions (BIFs): the complete list of 420 names
            # from IBM's alphabetic BIF reference (see module docstring).
            (
                words(
                    (
                        'abs', 'acos', 'add', 'adddays', 'addr', 'addrdata', 'all',
                        'allcompare', 'alloc31', 'allocate', 'allocation', 'allocnext',
                        'allocsize', 'any', 'asin', 'atan', 'atand', 'atanh', 'automatic',
                        'availablearea', 'base64decode', 'base64decode16', 'base64decode8',
                        'base64encode', 'base64encode16', 'base64encode8', 'between',
                        'betweenexclusive', 'betweenleftexclusive', 'binary',
                        'binaryvalue', 'binsearch', 'binsearchx', 'bit', 'bitlocation',
                        'bool', 'byte', 'bytelength', 'cds', 'ceil', 'centerleft',
                        'centerright', 'centreleft', 'centreright', 'character',
                        'chargraphic', 'charval', 'checkstg', 'checksum', 'codepage',
                        'collapse', 'collate', 'compare', 'complex', 'conjg', 'copy',
                        'cos', 'cosd', 'cosh', 'count', 'cs', 'currentsize',
                        'currentstorage', 'datafield', 'date', 'datetime', 'days',
                        'daystodate', 'daystomicrosecs', 'daystosecs', 'decimal',
                        'dimension', 'divide', 'edit', 'empty', 'entryaddr', 'epsilon',
                        'erf', 'erfc', 'exp', 'exponent', 'fileddint', 'fileddtest',
                        'fileddword', 'fileid', 'filenew', 'fileopen', 'fileread',
                        'fileseek', 'filetell', 'filewrite', 'fixed', 'fixedbin',
                        'fixeddec', 'float', 'floatbin', 'floatdec', 'floor',
                        'foldedfullmatch', 'foldedsimplematch', 'fracval', 'gamma',
                        'getenv', 'getjclsymbol', 'getsysint', 'getsysword', 'graphic',
                        'gtca', 'handle', 'hbound', 'hboundacross', 'hex', 'hex8',
                        'hexdecode', 'hexdecode8', 'hexencode', 'hexencode8', 'heximage',
                        'heximage8', 'high', 'huge', 'iand', 'iclz', 'ieor', 'ifthenelse',
                        'imag', 'inarray', 'index', 'indexr', 'indicators', 'inlist',
                        'inot', 'ior', 'ipopcnt', 'irll', 'irrl', 'isfinite', 'isigned',
                        'isinf', 'isjclsymbol', 'isleap', 'isll', 'ismain', 'isnan',
                        'isnormal', 'isrl', 'iszero', 'iunsigned', 'jsongetarrayend',
                        'jsongetarraystart', 'jsongetcolon', 'jsongetcomma',
                        'jsongetmember', 'jsongetobjectend', 'jsongetobjectstart',
                        'jsongetvalue', 'jsonputarrayend', 'jsonputarraystart',
                        'jsonputcolon', 'jsonputcomma', 'jsonputmember',
                        'jsonputobjectend', 'jsonputobjectstart', 'jsonputvalue',
                        'jsonvalid', 'juliantosmf', 'lastday', 'lbound', 'lboundacross',
                        'left', 'length', 'lineno', 'location', 'locstg', 'locval', 'log',
                        'log10', 'log2', 'loggamma', 'low', 'lowerascii', 'lowercase',
                        'lowerlatin1', 'mainname', 'max', 'maxdate', 'maxexp', 'maxlength',
                        'maxval', 'memcollapse', 'memconvert', 'memcu12', 'memcu14',
                        'memcu21', 'memcu24', 'memcu41', 'memcu42', 'memindex',
                        'memreplace', 'memsearch', 'memsearchr', 'memsqueeze',
                        'memuvalid16', 'memuvalid8', 'memverify', 'memverifyr',
                        'microsecs', 'microsecstodate', 'microsecstodays', 'min',
                        'mindate', 'minexp', 'minval', 'mod', 'mpstr', 'multiply', 'null',
                        'nullentry', 'offset', 'offsetadd', 'offsetdiff', 'offsetsubtract',
                        'offsetvalue', 'omitted', 'onactual', 'onarea', 'onchar',
                        # ONCODE: confirmed missing from the original
                        # "complete" 420-name list -- a real gap in that
                        # sourcing pass, not a since-added name. IBM's
                        # own "Condition handling" chapter singles it
                        # out specifically ("The ONCODE built-in function
                        # is particularly useful here, as it can be used
                        # to identify the specific circumstances that
                        # raised the condition[]"), and real-world
                        # testing found it used directly in the corpus
                        # below (code_samples/MACROS.pli's "ON ERROR
                        # ONCODE").
                        'oncode',
                        'oncondcond', 'oncondid', 'oncount', 'onexpected', 'onfile',
                        'ongsource', 'onhbound', 'onjsonname', 'onkey', 'onlbound',
                        'online', 'onloc', 'onoffset', 'onoperator', 'onpackage',
                        'onprocedure', 'onsource', 'onsubcode', 'onsubcode2',
                        'onsubscript', 'ontext', 'onuchar', 'onusource', 'onwchar',
                        'onwsource', 'ordinalname', 'ordinalpred', 'ordinalsucc',
                        'packagename', 'pageno', 'picspec', 'places', 'pliascii',
                        'pliattn', 'plicanc', 'plickpt', 'plidelete', 'plidump',
                        'pliebcdic', 'plifill', 'plifree', 'plimove', 'pliover',
                        'pliparse', 'plirest', 'pliretc', 'pliretv', 'plisaxa', 'plisaxb',
                        'plisaxc', 'plisaxd', 'plisrta', 'plisrtb', 'plisrtc', 'plisrtd',
                        'plistck', 'plistcke', 'plistckelocal', 'plistckeutc', 'plistckf',
                        'plistcklocal', 'plistckp', 'plistckplocal', 'plistckputc',
                        'plistckutc', 'plitran11', 'plitran12', 'plitran21', 'plitran22',
                        'pointer', 'pointeradd', 'pointerdiff', 'pointersubtract',
                        'pointervalue', 'poly', 'precision', 'precval', 'pred', 'present',
                        'procedurename', 'prod', 'putenv', 'quicksort', 'quicksortx',
                        'radix', 'random', 'rank', 'real', 'regex', 'rem', 'repattern',
                        'repeat', 'replace', 'reverse', 'right', 'round',
                        'roundawayfromzero', 'roundtoeven', 'samekey', 'scale', 'scaleval',
                        'scrubout', 'search', 'searchr', 'secs', 'secstodate',
                        'secstodays', 'sign', 'signed', 'sin', 'sind', 'sinh', 'size',
                        'smftojulian', 'sourcefile', 'sourceline', 'sqrt', 'sqrtf',
                        'squeeze', 'stackaddr', 'stcketodate', 'stcktodate', 'storage',
                        'string', 'substr', 'subto', 'subtract', 'succ', 'sum', 'sysnull',
                        'system', 'tally', 'tan', 'tand', 'tanh', 'threadid', 'time',
                        'timestamp', 'tiny', 'translate', 'trim', 'trunc', 'type', 'uhigh',
                        'ulength', 'ulength16', 'ulength8', 'ulow', 'unallocated', 'unhex',
                        'unsigned', 'unspec', 'upos', 'upperascii', 'uppercase',
                        'upperlatin1', 'usubstr', 'usupplementary', 'utcdatetime',
                        'utcmicrosecs', 'utcsecs', 'utf8', 'utf8stg', 'utf8tochar',
                        'utf8towchar', 'uuid', 'uuid4', 'uvalid', 'uwidth', 'valid',
                        'validdate', 'validvalue', 'varglist', 'vargsize', 'verify',
                        'verifyr', 'wcharval', 'weekday', 'wherediff', 'whigh', 'widechar',
                        'wlow', 'wscollapse', 'wscollapse16', 'wsreplace', 'wsreplace16',
                        'xmlchar', 'xmlscrub', 'xmlscrub16', 'xmluchar', 'y4date',
                        'y4julian', 'y4year',
                    ),
                    suffix=r"(\s*)(\()",
                ),
                bygroups(Name.Builtin, Whitespace, Operator),
            ),
            # ONCODE/ONLOC/ONCHAR/ONSOURCE used with no trailing "(" at
            # all -- confirmed as real, not a typo, by real-world
            # testing: code_samples/MACROS.pli both declares them
            # explicitly ("DCL (ALLOCATION,INDEX,ONLOC,ONCODE,ONCHAR,
            # ONSOURCE) BUILTIN;") and then uses each bare, as a plain
            # value, inside a PUT EDIT data list ("PUT SKIP EDIT(...,
            # ONLOC, ..., ONCODE, ..., ONSOURCE)(A)"). These four are
            # specifically the built-in functions IBM's "Condition
            # handling" chapter describes as taking no arguments and
            # returning information about the currently-raised
            # condition, so this isn't specific to these four literal
            # names by accident -- the same allowance plausibly applies
            # to the rest of the "on*" condition-inquiry BIF family
            # above (onkey, onfile, onsubscript, etc.), but only these
            # four were directly observed used this way in the corpus,
            # so only these four are added here rather than
            # generalizing on assumption.
            (
                words(
                    ("oncode", "onloc", "onchar", "onsource"),
                    suffix=r"\b",
                ),
                Name.Builtin,
            ),
            # Preprocessor-only built-in functions: confirmed, by direct
            # comparison against the runtime BIF list above, to be a
            # genuinely separate set -- not simply a subset of the
            # runtime BIFs, per IBM's complete "Preprocessor built-in
            # functions" list
            # (https://www.ibm.com/docs/en/SSY2V3_6.2/lr/prbif.html).
            # 17 of that page's 34 names (COMMENT, COMPILEDATE,
            # COMPILETIME, COPYRIGHT, COUNTER, MACCOL, MACLMAR, MACNAME,
            # MACRMAR, PARMSET, QUOTE, SERVICE, SYSDIMSIZE,
            # SYSOFFSETSIZE, SYSPARM, SYSPOINTERSIZE, SYSVERSION) do not
            # appear at all in the runtime list; the other 17 (SUBSTR,
            # LENGTH, MAX, MIN, etc.) already do and so aren't repeated
            # here. Not modeled: the semantic rule that a BIF name can be
            # shadowed by a same-named user-declared preprocessor
            # procedure (requires symbol-table tracking, a parser-level
            # concern, out of scope for a lexer). Also not confirmed:
            # IBM's page notes that 17 of these (the argument-less ones,
            # e.g. SYSPARM, COMPILEDATE, COUNTER) "must not be given a
            # null argument" -- if real source ever invokes them bare,
            # with no parentheses at all, this rule's suffix=r"(\()"
            # requirement means they'd fall through to plain Text
            # instead of Name.Builtin. Not verified either way; flagged
            # rather than assumed.
            (
                words(
                    (
                        'comment', 'compiledate', 'compiletime', 'copyright',
                        'counter', 'maccol', 'maclmar', 'macname', 'macrmar',
                        'parmset', 'quote', 'service', 'sysdimsize',
                        'sysoffsetsize', 'sysparm', 'syspointersize',
                        'sysversion',
                    ),
                    suffix=r"(\s*)(\()",
                ),
                bygroups(Name.Builtin, Whitespace, Operator),
            ),
        ],
        "exec": [
            # Dedicated state for an embedded EXEC SQL / EXEC CICS
            # statement -- see the module docstring's "Embedded EXEC SQL
            # / EXEC CICS" section for the design and its sourcing
            # against the real-world corpus.
            #
            # Terminated by a plain ";" (every one of the ~50 embedded
            # statements in the real-world corpus ends this way). Listed
            # first so it wins over the generic-punctuation rule below.
            (r";", Punctuation, "#pop"),
            # "END-EXEC" (optionally followed by ";") is the terminator
            # the ISO embedded-SQL standard and COBOL use; accepted as an
            # alternate even though no PL/I file in the corpus uses it. A
            # trailing ";" then falls through to root as an ordinary
            # statement terminator.
            (r"end-exec\b", Keyword.Reserved, "#pop"),
            (r"\s+", Whitespace),
            # A "/* ... */" comment can appear mid-statement; the shared
            # "comment" state pops straight back here, so a ";" inside
            # the comment can't end the region early.
            (r"/\*", Comment.Multiline, "comment"),
            # Host-variable reference (SQL): ":name". Any following
            # ".qualifier" or ":indicator" falls through to the generic
            # rules below. Real in the corpus: ":DEPT", ":STATEMENT",
            # ":SQLDA", ":TIMESTAMP", ":BUF1_CLOB".
            (
                r"(:)(\s*)(" + _SYMBOL + r")",
                bygroups(Punctuation, Whitespace, Name.Variable),
            ),
            # String literals -- recognized so an embedded ";" or "*/"
            # inside one doesn't end the region/comment. Corpus:
            # EXEC CICS FILE('VSR404'); EXEC SQL ... VALUES ('Igor',...).
            (r"'", String, "string"),
            (r'"', String, "string_double"),
            (r"[0-9]+(?:\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?", Number),
            # Everything else stays coarse on purpose: SQL keywords, CICS
            # command verbs and option keywords, and table/column/file
            # names all become a single generic Name (see the module
            # docstring for why this matches the CFamilyLexer 'macro'
            # precedent and this lexer's own preprocessor handling).
            (_SYMBOL, Name),
            (r"[(),.]", Punctuation),
            (r"[-+*/=<>|&:]", Operator),
            # Catch-all: never emit an Error token from inside an
            # embedded region (e.g. dynamic SQL's "?" parameter marker),
            # consistent with how the rest of the lexer degrades
            # unrecognized input to Text.
            (r".", Text),
        ],
        "string": [
            (r"[^'\n]+", String),
            (r"''", String),
            (r"'", String, "#pop"),
            (r"\n", Text, "#pop"),  # Stray linefeed also terminates strings.
        ],
        "string_double": [
            (r'[^"\n]+', String),
            (r'""', String),
            (r'"', String, "#pop"),
            (r"\n", Text, "#pop"),  # Stray linefeed also terminates strings.
        ],
        "comment": [
            (r"[^*]+", Comment.Multiline),
            (r"\*/", Comment.Multiline, "#pop"),
            (r"\*", Comment.Multiline),
        ],
    }
