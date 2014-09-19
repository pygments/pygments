# -*- coding: utf-8 -*-
"""
    pygments.lexers.other
    ~~~~~~~~~~~~~~~~~~~~~

    Lexers for other languages.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
from pygments.lexer import ExtendedRegexLexer, RegexLexer, include, bygroups, \
    default, using, this, combined
from pygments.token import Generic, Comment, String, Text, Number, Keyword, Name, \
    Error, Operator, Punctuation, Literal, Whitespace

# backwards compatibility
from pygments.lexers.sql import SqlLexer, MySqlLexer, SqliteConsoleLexer
from pygments.lexers.shell import BashLexer, BashSessionLexer, BatchLexer, \
    TcshLexer
from pygments.lexers.robotframework import RobotFrameworkLexer
from pygments.lexers.testing import GherkinLexer
from pygments.lexers.esoteric import BrainfuckLexer, BefungeLexer
from pygments.lexers.prolog import LogtalkLexer
from pygments.lexers.misc.snobol import SnobolLexer
from pygments.lexers.misc.rebol import RebolLexer
from pygments.lexers.configs import KconfigLexer
from pygments.lexers.modeling import ModelicaLexer
from pygments.lexers.scripting import AppleScriptLexer
from pygments.lexers.graphics import PostScriptLexer, GnuplotLexer, \
    AsymptoteLexer, PovrayLexer
from pygments.lexers.business import ABAPLexer, OpenEdgeLexer


__all__ = ['RedcodeLexer', 'MOOCodeLexer', 'SmalltalkLexer', 'NewspeakLexer',
           'AutohotkeyLexer', 'GoodDataCLLexer', 'MaqlLexer', 'ProtoBufLexer',
           'HybrisLexer', 'AwkLexer', 'Cfengine3Lexer', 'ECLLexer',
           'UrbiscriptLexer', 'BroLexer', 'MscgenLexer', 'VGLLexer',
           'SourcePawnLexer', 'PuppetLexer', 'NSISLexer', 'RPMSpecLexer',
           'CbmBasicV2Lexer', 'AutoItLexer', 'RexxLexer', 'APLLexer',
           'AmbientTalkLexer', 'PawnLexer', 'RslLexer', 'PanLexer', 'RedLexer',
           'AlloyLexer']


class ECLLexer(RegexLexer):
    """
    Lexer for the declarative big-data `ECL
    <http://hpccsystems.com/community/docs/ecl-language-reference/html>`_
    language.

    .. versionadded:: 1.5
    """

    name = 'ECL'
    aliases = ['ecl']
    filenames = ['*.ecl']
    mimetypes = ['application/x-ecl']

    flags = re.IGNORECASE | re.MULTILINE

    tokens = {
        'root': [
            include('whitespace'),
            include('statements'),
        ],
        'whitespace': [
            (r'\s+', Text),
            (r'\/\/.*', Comment.Single),
            (r'/(\\\n)?\*(.|\n)*?\*(\\\n)?/', Comment.Multiline),
        ],
        'statements': [
            include('types'),
            include('keywords'),
            include('functions'),
            include('hash'),
            (r'"', String, 'string'),
            (r'\'', String, 'string'),
            (r'(\d+\.\d*|\.\d+|\d+)e[+-]?\d+[lu]*', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-f]+[lu]*', Number.Hex),
            (r'0[0-7]+[lu]*', Number.Oct),
            (r'\d+[LlUu]*', Number.Integer),
            (r'\*/', Error),
            (r'[~!%^&*+=|?:<>/-]+', Operator),
            (r'[{}()\[\],.;]', Punctuation),
            (r'[a-z_]\w*', Name),
        ],
        'hash': [
            (r'^#.*$', Comment.Preproc),
        ],
        'types': [
            (r'(RECORD|END)\D', Keyword.Declaration),
            (r'((?:ASCII|BIG_ENDIAN|BOOLEAN|DATA|DECIMAL|EBCDIC|INTEGER|PATTERN|'
             r'QSTRING|REAL|RECORD|RULE|SET OF|STRING|TOKEN|UDECIMAL|UNICODE|'
             r'UNSIGNED|VARSTRING|VARUNICODE)\d*)(\s+)',
             bygroups(Keyword.Type, Text)),
        ],
        'keywords': [
            (r'(APPLY|ASSERT|BUILD|BUILDINDEX|EVALUATE|FAIL|KEYDIFF|KEYPATCH|'
             r'LOADXML|NOTHOR|NOTIFY|OUTPUT|PARALLEL|SEQUENTIAL|SOAPCALL|WAIT'
             r'CHECKPOINT|DEPRECATED|FAILCODE|FAILMESSAGE|FAILURE|GLOBAL|'
             r'INDEPENDENT|ONWARNING|PERSIST|PRIORITY|RECOVERY|STORED|SUCCESS|'
             r'WAIT|WHEN)\b', Keyword.Reserved),
            # These are classed differently, check later
            (r'(ALL|AND|ANY|AS|ATMOST|BEFORE|BEGINC\+\+|BEST|BETWEEN|CASE|CONST|'
             r'COUNTER|CSV|DESCEND|ENCRYPT|ENDC\+\+|ENDMACRO|EXCEPT|EXCLUSIVE|'
             r'EXPIRE|EXPORT|EXTEND|FALSE|FEW|FIRST|FLAT|FULL|FUNCTION|GROUP|'
             r'HEADER|HEADING|HOLE|IFBLOCK|IMPORT|IN|JOINED|KEEP|KEYED|LAST|'
             r'LEFT|LIMIT|LOAD|LOCAL|LOCALE|LOOKUP|MACRO|MANY|MAXCOUNT|'
             r'MAXLENGTH|MIN SKEW|MODULE|INTERFACE|NAMED|NOCASE|NOROOT|NOSCAN|'
             r'NOSORT|NOT|OF|ONLY|OPT|OR|OUTER|OVERWRITE|PACKED|PARTITION|'
             r'PENALTY|PHYSICALLENGTH|PIPE|QUOTE|RELATIONSHIP|REPEAT|RETURN|'
             r'RIGHT|SCAN|SELF|SEPARATOR|SERVICE|SHARED|SKEW|SKIP|SQL|STORE|'
             r'TERMINATOR|THOR|THRESHOLD|TOKEN|TRANSFORM|TRIM|TRUE|TYPE|'
             r'UNICODEORDER|UNSORTED|VALIDATE|VIRTUAL|WHOLE|WILD|WITHIN|XML|'
             r'XPATH|__COMPRESSED__)\b', Keyword.Reserved),
        ],
        'functions': [
            (r'(ABS|ACOS|ALLNODES|ASCII|ASIN|ASSTRING|ATAN|ATAN2|AVE|CASE|'
             r'CHOOSE|CHOOSEN|CHOOSESETS|CLUSTERSIZE|COMBINE|CORRELATION|COS|'
             r'COSH|COUNT|COVARIANCE|CRON|DATASET|DEDUP|DEFINE|DENORMALIZE|'
             r'DISTRIBUTE|DISTRIBUTED|DISTRIBUTION|EBCDIC|ENTH|ERROR|EVALUATE|'
             r'EVENT|EVENTEXTRA|EVENTNAME|EXISTS|EXP|FAILCODE|FAILMESSAGE|'
             r'FETCH|FROMUNICODE|GETISVALID|GLOBAL|GRAPH|GROUP|HASH|HASH32|'
             r'HASH64|HASHCRC|HASHMD5|HAVING|IF|INDEX|INTFORMAT|ISVALID|'
             r'ITERATE|JOIN|KEYUNICODE|LENGTH|LIBRARY|LIMIT|LN|LOCAL|LOG|LOOP|'
             r'MAP|MATCHED|MATCHLENGTH|MATCHPOSITION|MATCHTEXT|MATCHUNICODE|'
             r'MAX|MERGE|MERGEJOIN|MIN|NOLOCAL|NONEMPTY|NORMALIZE|PARSE|PIPE|'
             r'POWER|PRELOAD|PROCESS|PROJECT|PULL|RANDOM|RANGE|RANK|RANKED|'
             r'REALFORMAT|RECORDOF|REGEXFIND|REGEXREPLACE|REGROUP|REJECTED|'
             r'ROLLUP|ROUND|ROUNDUP|ROW|ROWDIFF|SAMPLE|SET|SIN|SINH|SIZEOF|'
             r'SOAPCALL|SORT|SORTED|SQRT|STEPPED|STORED|SUM|TABLE|TAN|TANH|'
             r'THISNODE|TOPN|TOUNICODE|TRANSFER|TRIM|TRUNCATE|TYPEOF|UNGROUP|'
             r'UNICODEORDER|VARIANCE|WHICH|WORKUNIT|XMLDECODE|XMLENCODE|'
             r'XMLTEXT|XMLUNICODE)\b', Name.Function),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\'', String, '#pop'),
            (r'[^"\']+', String),
        ],
    }


class RedcodeLexer(RegexLexer):
    """
    A simple Redcode lexer based on ICWS'94.
    Contributed by Adam Blinkinsop <blinks@acm.org>.

    .. versionadded:: 0.8
    """
    name = 'Redcode'
    aliases = ['redcode']
    filenames = ['*.cw']

    opcodes = ['DAT','MOV','ADD','SUB','MUL','DIV','MOD',
               'JMP','JMZ','JMN','DJN','CMP','SLT','SPL',
               'ORG','EQU','END']
    modifiers = ['A','B','AB','BA','F','X','I']

    tokens = {
        'root': [
            # Whitespace:
            (r'\s+', Text),
            (r';.*$', Comment.Single),
            # Lexemes:
            #  Identifiers
            (r'\b(%s)\b' % '|'.join(opcodes), Name.Function),
            (r'\b(%s)\b' % '|'.join(modifiers), Name.Decorator),
            (r'[A-Za-z_][A-Za-z_0-9]+', Name),
            #  Operators
            (r'[-+*/%]', Operator),
            (r'[#$@<>]', Operator), # mode
            (r'[.,]', Punctuation), # mode
            #  Numbers
            (r'[-+]?\d+', Number.Integer),
        ],
    }


class MOOCodeLexer(RegexLexer):
    """
    For `MOOCode <http://www.moo.mud.org/>`_ (the MOO scripting
    language).

    .. versionadded:: 0.9
    """
    name = 'MOOCode'
    filenames = ['*.moo']
    aliases = ['moocode', 'moo']
    mimetypes = ['text/x-moocode']

    tokens = {
        'root' : [
            # Numbers
            (r'(0|[1-9][0-9_]*)', Number.Integer),
            # Strings
            (r'"(\\\\|\\"|[^"])*"', String),
            # exceptions
            (r'(E_PERM|E_DIV)', Name.Exception),
            # db-refs
            (r'((#[-0-9]+)|(\$[a-z_A-Z0-9]+))', Name.Entity),
            # Keywords
            (r'\b(if|else|elseif|endif|for|endfor|fork|endfork|while'
             r'|endwhile|break|continue|return|try'
             r'|except|endtry|finally|in)\b', Keyword),
            # builtins
            (r'(random|length)', Name.Builtin),
            # special variables
            (r'(player|caller|this|args)', Name.Variable.Instance),
            # skip whitespace
            (r'\s+', Text),
            (r'\n', Text),
            # other operators
            (r'([!;=,{}&\|:\.\[\]@\(\)\<\>\?]+)', Operator),
            # function call
            (r'([a-z_A-Z0-9]+)(\()', bygroups(Name.Function, Operator)),
            # variables
            (r'([a-zA-Z_0-9]+)', Text),
        ]
    }


class SmalltalkLexer(RegexLexer):
    """
    For `Smalltalk <http://www.smalltalk.org/>`_ syntax.
    Contributed by Stefan Matthias Aust.
    Rewritten by Nils Winter.

    .. versionadded:: 0.10
    """
    name = 'Smalltalk'
    filenames = ['*.st']
    aliases = ['smalltalk', 'squeak', 'st']
    mimetypes = ['text/x-smalltalk']

    tokens = {
        'root' : [
            (r'(<)(\w+:)(.*?)(>)', bygroups(Text, Keyword, Text, Text)),
            include('squeak fileout'),
            include('whitespaces'),
            include('method definition'),
            (r'(\|)([\w\s]*)(\|)', bygroups(Operator, Name.Variable, Operator)),
            include('objects'),
            (r'\^|\:=|\_', Operator),
            # temporaries
            (r'[\]({}.;!]', Text),
        ],
        'method definition' : [
            # Not perfect can't allow whitespaces at the beginning and the
            # without breaking everything
            (r'([a-zA-Z]+\w*:)(\s*)(\w+)',
             bygroups(Name.Function, Text, Name.Variable)),
            (r'^(\b[a-zA-Z]+\w*\b)(\s*)$', bygroups(Name.Function, Text)),
            (r'^([-+*/\\~<>=|&!?,@%]+)(\s*)(\w+)(\s*)$',
             bygroups(Name.Function, Text, Name.Variable, Text)),
        ],
        'blockvariables' : [
            include('whitespaces'),
            (r'(:)(\s*)(\w+)',
             bygroups(Operator, Text, Name.Variable)),
            (r'\|', Operator, '#pop'),
            default('#pop'), # else pop
        ],
        'literals' : [
            (r"'(''|[^'])*'", String, 'afterobject'),
            (r'\$.', String.Char, 'afterobject'),
            (r'#\(', String.Symbol, 'parenth'),
            (r'\)', Text, 'afterobject'),
            (r'(\d+r)?-?\d+(\.\d+)?(e-?\d+)?', Number, 'afterobject'),
        ],
        '_parenth_helper' : [
            include('whitespaces'),
            (r'(\d+r)?-?\d+(\.\d+)?(e-?\d+)?', Number),
            (r'[-+*/\\~<>=|&#!?,@%\w:]+', String.Symbol),
            # literals
            (r"'(''|[^'])*'", String),
            (r'\$.', String.Char),
            (r'#*\(', String.Symbol, 'inner_parenth'),
        ],
        'parenth' : [
            # This state is a bit tricky since
            # we can't just pop this state
            (r'\)', String.Symbol, ('root', 'afterobject')),
            include('_parenth_helper'),
        ],
        'inner_parenth': [
            (r'\)', String.Symbol, '#pop'),
            include('_parenth_helper'),
        ],
        'whitespaces' : [
            # skip whitespace and comments
            (r'\s+', Text),
            (r'"(""|[^"])*"', Comment),
        ],
        'objects' : [
            (r'\[', Text, 'blockvariables'),
            (r'\]', Text, 'afterobject'),
            (r'\b(self|super|true|false|nil|thisContext)\b',
             Name.Builtin.Pseudo, 'afterobject'),
            (r'\b[A-Z]\w*(?!:)\b', Name.Class, 'afterobject'),
            (r'\b[a-z]\w*(?!:)\b', Name.Variable, 'afterobject'),
            (r'#("(""|[^"])*"|[-+*/\\~<>=|&!?,@%]+|[\w:]+)',
             String.Symbol, 'afterobject'),
            include('literals'),
        ],
        'afterobject' : [
            (r'! !$', Keyword , '#pop'), # squeak chunk delimiter
            include('whitespaces'),
            (r'\b(ifTrue:|ifFalse:|whileTrue:|whileFalse:|timesRepeat:)',
             Name.Builtin, '#pop'),
            (r'\b(new\b(?!:))', Name.Builtin),
            (r'\:=|\_', Operator, '#pop'),
            (r'\b[a-zA-Z]+\w*:', Name.Function, '#pop'),
            (r'\b[a-zA-Z]+\w*', Name.Function),
            (r'\w+:?|[-+*/\\~<>=|&!?,@%]+', Name.Function, '#pop'),
            (r'\.', Punctuation, '#pop'),
            (r';', Punctuation),
            (r'[\])}]', Text),
            (r'[\[({]', Text, '#pop'),
        ],
        'squeak fileout' : [
            # Squeak fileout format (optional)
            (r'^"(""|[^"])*"!', Keyword),
            (r"^'(''|[^'])*'!", Keyword),
            (r'^(!)(\w+)( commentStamp: )(.*?)( prior: .*?!\n)(.*?)(!)',
                bygroups(Keyword, Name.Class, Keyword, String, Keyword, Text, Keyword)),
            (r"^(!)(\w+(?: class)?)( methodsFor: )('(?:''|[^'])*')(.*?!)",
                bygroups(Keyword, Name.Class, Keyword, String, Keyword)),
            (r'^(\w+)( subclass: )(#\w+)'
             r'(\s+instanceVariableNames: )(.*?)'
             r'(\s+classVariableNames: )(.*?)'
             r'(\s+poolDictionaries: )(.*?)'
             r'(\s+category: )(.*?)(!)',
                bygroups(Name.Class, Keyword, String.Symbol, Keyword, String, Keyword,
                         String, Keyword, String, Keyword, String, Keyword)),
            (r'^(\w+(?: class)?)(\s+instanceVariableNames: )(.*?)(!)',
                bygroups(Name.Class, Keyword, String, Keyword)),
            (r'(!\n)(\].*)(! !)$', bygroups(Keyword, Text, Keyword)),
            (r'! !$', Keyword),
        ],
    }


class NewspeakLexer(RegexLexer):
    """
    For `Newspeak <http://newspeaklanguage.org/>` syntax.
    """
    name = 'Newspeak'
    filenames = ['*.ns2']
    aliases = ['newspeak', ]
    mimetypes = ['text/x-newspeak']

    tokens = {
       'root' : [
           (r'\b(Newsqueak2)\b',Keyword.Declaration),
           (r"'[^']*'",String),
           (r'\b(class)(\s+)(\w+)(\s*)',
            bygroups(Keyword.Declaration,Text,Name.Class,Text)),
           (r'\b(mixin|self|super|private|public|protected|nil|true|false)\b',
            Keyword),
           (r'(\w+\:)(\s*)([a-zA-Z_]\w+)',
            bygroups(Name.Function,Text,Name.Variable)),
           (r'(\w+)(\s*)(=)',
            bygroups(Name.Attribute,Text,Operator)),
           (r'<\w+>', Comment.Special),
           include('expressionstat'),
           include('whitespace')
        ],

       'expressionstat': [
          (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
          (r'\d+', Number.Integer),
          (r':\w+',Name.Variable),
          (r'(\w+)(::)', bygroups(Name.Variable, Operator)),
          (r'\w+:', Name.Function),
          (r'\w+', Name.Variable),
          (r'\(|\)', Punctuation),
          (r'\[|\]', Punctuation),
          (r'\{|\}', Punctuation),

          (r'(\^|\+|\/|~|\*|<|>|=|@|%|\||&|\?|!|,|-|:)', Operator),
          (r'\.|;', Punctuation),
          include('whitespace'),
          include('literals'),
       ],
       'literals': [
         (r'\$.', String),
         (r"'[^']*'", String),
         (r"#'[^']*'", String.Symbol),
         (r"#\w+:?", String.Symbol),
         (r"#(\+|\/|~|\*|<|>|=|@|%|\||&|\?|!|,|-)+", String.Symbol)

       ],
       'whitespace' : [
         (r'\s+', Text),
         (r'"[^"]*"', Comment)
       ]
    }


class AutohotkeyLexer(RegexLexer):
    """
    For `autohotkey <http://www.autohotkey.com/>`_ source code.

    .. versionadded:: 1.4
    """
    name = 'autohotkey'
    aliases = ['ahk', 'autohotkey']
    filenames = ['*.ahk', '*.ahkl']
    mimetypes = ['text/x-autohotkey']

    tokens = {
        'root': [
            (r'^(\s*)(/\*)', bygroups(Text, Comment.Multiline),
                             'incomment'),
            (r'^(\s*)(\()', bygroups(Text, Generic), 'incontinuation'),
            (r'\s+;.*?$', Comment.Singleline),
            (r'^;.*?$', Comment.Singleline),
            (r'[]{}(),;[]', Punctuation),
            (r'(in|is|and|or|not)\b', Operator.Word),
            (r'\%[a-zA-Z_#@$][\w#@$]*\%', Name.Variable),
            (r'!=|==|:=|\.=|<<|>>|[-~+/*%=<>&^|?:!.]', Operator),
            include('commands'),
            include('labels'),
            include('builtInFunctions'),
            include('builtInVariables'),
            (r'"', String, combined('stringescape', 'dqs')),
            include('numbers'),
            (r'[a-zA-Z_#@$][\w#@$]*', Name),
            (r'\\|\'', Text),
            (r'\`([\,\%\`abfnrtv\-\+;])', String.Escape),
            include('garbage'),
        ],
        'incomment': [
            (r'^\s*\*/', Comment.Multiline, '#pop'),
            (r'[^*/]', Comment.Multiline),
            (r'[*/]', Comment.Multiline)
        ],
        'incontinuation': [
            (r'^\s*\)', Generic, '#pop'),
            (r'[^)]', Generic),
            (r'[)]', Generic),
        ],
        'commands': [
            (r'(?i)^(\s*)(global|local|static|'
             r'#AllowSameLineComments|#ClipboardTimeout|#CommentFlag|'
             r'#ErrorStdOut|#EscapeChar|#HotkeyInterval|#HotkeyModifierTimeout|'
             r'#Hotstring|#IfWinActive|#IfWinExist|#IfWinNotActive|'
             r'#IfWinNotExist|#IncludeAgain|#Include|#InstallKeybdHook|'
             r'#InstallMouseHook|#KeyHistory|#LTrim|#MaxHotkeysPerInterval|'
             r'#MaxMem|#MaxThreads|#MaxThreadsBuffer|#MaxThreadsPerHotkey|'
             r'#NoEnv|#NoTrayIcon|#Persistent|#SingleInstance|#UseHook|'
             r'#WinActivateForce|AutoTrim|BlockInput|Break|Click|ClipWait|'
             r'Continue|Control|ControlClick|ControlFocus|ControlGetFocus|'
             r'ControlGetPos|ControlGetText|ControlGet|ControlMove|ControlSend|'
             r'ControlSendRaw|ControlSetText|CoordMode|Critical|'
             r'DetectHiddenText|DetectHiddenWindows|Drive|DriveGet|'
             r'DriveSpaceFree|Edit|Else|EnvAdd|EnvDiv|EnvGet|EnvMult|EnvSet|'
             r'EnvSub|EnvUpdate|Exit|ExitApp|FileAppend|'
             r'FileCopy|FileCopyDir|FileCreateDir|FileCreateShortcut|'
             r'FileDelete|FileGetAttrib|FileGetShortcut|FileGetSize|'
             r'FileGetTime|FileGetVersion|FileInstall|FileMove|FileMoveDir|'
             r'FileRead|FileReadLine|FileRecycle|FileRecycleEmpty|'
             r'FileRemoveDir|FileSelectFile|FileSelectFolder|FileSetAttrib|'
             r'FileSetTime|FormatTime|GetKeyState|Gosub|Goto|GroupActivate|'
             r'GroupAdd|GroupClose|GroupDeactivate|Gui|GuiControl|'
             r'GuiControlGet|Hotkey|IfEqual|IfExist|IfGreaterOrEqual|IfGreater|'
             r'IfInString|IfLess|IfLessOrEqual|IfMsgBox|IfNotEqual|IfNotExist|'
             r'IfNotInString|IfWinActive|IfWinExist|IfWinNotActive|'
             r'IfWinNotExist|If |ImageSearch|IniDelete|IniRead|IniWrite|'
             r'InputBox|Input|KeyHistory|KeyWait|ListHotkeys|ListLines|'
             r'ListVars|Loop|Menu|MouseClickDrag|MouseClick|MouseGetPos|'
             r'MouseMove|MsgBox|OnExit|OutputDebug|Pause|PixelGetColor|'
             r'PixelSearch|PostMessage|Process|Progress|Random|RegDelete|'
             r'RegRead|RegWrite|Reload|Repeat|Return|RunAs|RunWait|Run|'
             r'SendEvent|SendInput|SendMessage|SendMode|SendPlay|SendRaw|Send|'
             r'SetBatchLines|SetCapslockState|SetControlDelay|'
             r'SetDefaultMouseSpeed|SetEnv|SetFormat|SetKeyDelay|'
             r'SetMouseDelay|SetNumlockState|SetScrollLockState|'
             r'SetStoreCapslockMode|SetTimer|SetTitleMatchMode|'
             r'SetWinDelay|SetWorkingDir|Shutdown|Sleep|Sort|SoundBeep|'
             r'SoundGet|SoundGetWaveVolume|SoundPlay|SoundSet|'
             r'SoundSetWaveVolume|SplashImage|SplashTextOff|SplashTextOn|'
             r'SplitPath|StatusBarGetText|StatusBarWait|StringCaseSense|'
             r'StringGetPos|StringLeft|StringLen|StringLower|StringMid|'
             r'StringReplace|StringRight|StringSplit|StringTrimLeft|'
             r'StringTrimRight|StringUpper|Suspend|SysGet|Thread|ToolTip|'
             r'Transform|TrayTip|URLDownloadToFile|While|WinActivate|'
             r'WinActivateBottom|WinClose|WinGetActiveStats|WinGetActiveTitle|'
             r'WinGetClass|WinGetPos|WinGetText|WinGetTitle|WinGet|WinHide|'
             r'WinKill|WinMaximize|WinMenuSelectItem|WinMinimizeAllUndo|'
             r'WinMinimizeAll|WinMinimize|WinMove|WinRestore|WinSetTitle|'
             r'WinSet|WinShow|WinWaitActive|WinWaitClose|WinWaitNotActive|'
             r'WinWait)\b', bygroups(Text, Name.Builtin)),
        ],
        'builtInFunctions': [
            (r'(?i)(Abs|ACos|Asc|ASin|ATan|Ceil|Chr|Cos|DllCall|Exp|FileExist|'
             r'Floor|GetKeyState|IL_Add|IL_Create|IL_Destroy|InStr|IsFunc|'
             r'IsLabel|Ln|Log|LV_Add|LV_Delete|LV_DeleteCol|LV_GetCount|'
             r'LV_GetNext|LV_GetText|LV_Insert|LV_InsertCol|LV_Modify|'
             r'LV_ModifyCol|LV_SetImageList|Mod|NumGet|NumPut|OnMessage|'
             r'RegExMatch|RegExReplace|RegisterCallback|Round|SB_SetIcon|'
             r'SB_SetParts|SB_SetText|Sin|Sqrt|StrLen|SubStr|Tan|TV_Add|'
             r'TV_Delete|TV_GetChild|TV_GetCount|TV_GetNext|TV_Get|'
             r'TV_GetParent|TV_GetPrev|TV_GetSelection|TV_GetText|TV_Modify|'
             r'VarSetCapacity|WinActive|WinExist|Object|ComObjActive|'
             r'ComObjArray|ComObjEnwrap|ComObjUnwrap|ComObjParameter|'
             r'ComObjType|ComObjConnect|ComObjCreate|ComObjGet|ComObjError|'
             r'ComObjValue|Insert|MinIndex|MaxIndex|Remove|SetCapacity|'
             r'GetCapacity|GetAddress|_NewEnum|FileOpen|Read|Write|ReadLine|'
             r'WriteLine|ReadNumType|WriteNumType|RawRead|RawWrite|Seek|Tell|'
             r'Close|Next|IsObject|StrPut|StrGet|Trim|LTrim|RTrim)\b',
             Name.Function),
        ],
        'builtInVariables': [
            (r'(?i)(A_AhkPath|A_AhkVersion|A_AppData|A_AppDataCommon|'
             r'A_AutoTrim|A_BatchLines|A_CaretX|A_CaretY|A_ComputerName|'
             r'A_ControlDelay|A_Cursor|A_DDDD|A_DDD|A_DD|A_DefaultMouseSpeed|'
             r'A_Desktop|A_DesktopCommon|A_DetectHiddenText|'
             r'A_DetectHiddenWindows|A_EndChar|A_EventInfo|A_ExitReason|'
             r'A_FormatFloat|A_FormatInteger|A_Gui|A_GuiEvent|A_GuiControl|'
             r'A_GuiControlEvent|A_GuiHeight|A_GuiWidth|A_GuiX|A_GuiY|A_Hour|'
             r'A_IconFile|A_IconHidden|A_IconNumber|A_IconTip|A_Index|'
             r'A_IPAddress1|A_IPAddress2|A_IPAddress3|A_IPAddress4|A_ISAdmin|'
             r'A_IsCompiled|A_IsCritical|A_IsPaused|A_IsSuspended|A_KeyDelay|'
             r'A_Language|A_LastError|A_LineFile|A_LineNumber|A_LoopField|'
             r'A_LoopFileAttrib|A_LoopFileDir|A_LoopFileExt|A_LoopFileFullPath|'
             r'A_LoopFileLongPath|A_LoopFileName|A_LoopFileShortName|'
             r'A_LoopFileShortPath|A_LoopFileSize|A_LoopFileSizeKB|'
             r'A_LoopFileSizeMB|A_LoopFileTimeAccessed|A_LoopFileTimeCreated|'
             r'A_LoopFileTimeModified|A_LoopReadLine|A_LoopRegKey|'
             r'A_LoopRegName|A_LoopRegSubkey|A_LoopRegTimeModified|'
             r'A_LoopRegType|A_MDAY|A_Min|A_MM|A_MMM|A_MMMM|A_Mon|A_MouseDelay|'
             r'A_MSec|A_MyDocuments|A_Now|A_NowUTC|A_NumBatchLines|A_OSType|'
             r'A_OSVersion|A_PriorHotkey|A_ProgramFiles|A_Programs|'
             r'A_ProgramsCommon|A_ScreenHeight|A_ScreenWidth|A_ScriptDir|'
             r'A_ScriptFullPath|A_ScriptName|A_Sec|A_Space|A_StartMenu|'
             r'A_StartMenuCommon|A_Startup|A_StartupCommon|A_StringCaseSense|'
             r'A_Tab|A_Temp|A_ThisFunc|A_ThisHotkey|A_ThisLabel|A_ThisMenu|'
             r'A_ThisMenuItem|A_ThisMenuItemPos|A_TickCount|A_TimeIdle|'
             r'A_TimeIdlePhysical|A_TimeSincePriorHotkey|A_TimeSinceThisHotkey|'
             r'A_TitleMatchMode|A_TitleMatchModeSpeed|A_UserName|A_WDay|'
             r'A_WinDelay|A_WinDir|A_WorkingDir|A_YDay|A_YEAR|A_YWeek|A_YYYY|'
             r'Clipboard|ClipboardAll|ComSpec|ErrorLevel|ProgramFiles|True|'
             r'False|A_IsUnicode|A_FileEncoding|A_OSVersion|A_PtrSize)\b',
             Name.Variable),
        ],
        'labels': [
            # hotkeys and labels
            # technically, hotkey names are limited to named keys and buttons
            (r'(^\s*)([^:\s\(\"]+?:{1,2})', bygroups(Text, Name.Label)),
            (r'(^\s*)(::[^:\s]+?::)', bygroups(Text, Name.Label)),
        ],
        'numbers': [
            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
            (r'\d+[eE][+-]?[0-9]+', Number.Float),
            (r'0\d+', Number.Oct),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+', Number.Integer)
        ],
        'stringescape': [
            (r'\"\"|\`([\,\%\`abfnrtv])', String.Escape),
        ],
        'strings': [
            (r'[^"\n]+', String),
        ],
        'dqs': [
            (r'"', String, '#pop'),
            include('strings')
        ],
        'garbage': [
            (r'[^\S\n]', Text),
            # (r'.', Text),      # no cheating
        ],
    }


class MaqlLexer(RegexLexer):
    """
    Lexer for `GoodData MAQL
    <https://secure.gooddata.com/docs/html/advanced.metric.tutorial.html>`_
    scripts.

    .. versionadded:: 1.4
    """

    name = 'MAQL'
    aliases = ['maql']
    filenames = ['*.maql']
    mimetypes = ['text/x-gooddata-maql','application/x-gooddata-maql']

    flags = re.IGNORECASE
    tokens = {
        'root': [
            # IDENTITY
            (r'IDENTIFIER\b', Name.Builtin),
            # IDENTIFIER
            (r'\{[^}]+\}', Name.Variable),
            # NUMBER
            (r'[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]{1,3})?', Literal.Number),
            # STRING
            (r'"', Literal.String, 'string-literal'),
            #  RELATION
            (r'\<\>|\!\=', Operator),
            (r'\=|\>\=|\>|\<\=|\<', Operator),
            # :=
            (r'\:\=', Operator),
            # OBJECT
            (r'\[[^]]+\]', Name.Variable.Class),
            # keywords
            (r'(DIMENSIONS?|BOTTOM|METRIC|COUNT|OTHER|FACT|WITH|TOP|OR|'
             r'ATTRIBUTE|CREATE|PARENT|FALSE|ROWS?|FROM|ALL|AS|PF|'
             r'COLUMNS?|DEFINE|REPORT|LIMIT|TABLE|LIKE|AND|BY|'
             r'BETWEEN|EXCEPT|SELECT|MATCH|WHERE|TRUE|FOR|IN|'
             r'WITHOUT|FILTER|ALIAS|ORDER|FACT|WHEN|NOT|ON|'
             r'KEYS|KEY|FULLSET|PRIMARY|LABELS|LABEL|VISUAL|'
             r'TITLE|DESCRIPTION|FOLDER|ALTER|DROP|ADD|DATASET|'
             r'DATATYPE|INT|BIGINT|DOUBLE|DATE|VARCHAR|DECIMAL|'
             r'SYNCHRONIZE|TYPE|DEFAULT|ORDER|ASC|DESC|HYPERLINK|'
             r'INCLUDE|TEMPLATE|MODIFY)\b', Keyword),
            # FUNCNAME
            (r'[a-z]\w*\b', Name.Function),
            # Comments
            (r'#.*', Comment.Single),
            # Punctuation
            (r'[,;\(\)]', Punctuation),
            # Space is not significant
            (r'\s+', Text)
        ],
        'string-literal': [
            (r'\\[tnrfbae"\\]', String.Escape),
            (r'"', Literal.String, '#pop'),
            (r'[^\\"]+', Literal.String)
        ],
    }


class GoodDataCLLexer(RegexLexer):
    """
    Lexer for `GoodData-CL <http://github.com/gooddata/GoodData-CL/raw/master/cli/src/main/resources/com/gooddata/processor/COMMANDS.txt>`_
    script files.

    .. versionadded:: 1.4
    """

    name = 'GoodData-CL'
    aliases = ['gooddata-cl']
    filenames = ['*.gdc']
    mimetypes = ['text/x-gooddata-cl']

    flags = re.IGNORECASE
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
            (r'"', Literal.String, 'string-literal'),
            (r'[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]{1,3})?', Literal.Number),
            # Space is not significant
            (r'\s', Text)
        ],
        'string-literal': [
            (r'\\[tnrfbae"\\]', String.Escape),
            (r'"', Literal.String, '#pop'),
            (r'[^\\"]+', Literal.String)
        ]
    }


class ProtoBufLexer(RegexLexer):
    """
    Lexer for `Protocol Buffer <http://code.google.com/p/protobuf/>`_
    definition files.

    .. versionadded:: 1.4
    """

    name = 'Protocol Buffer'
    aliases = ['protobuf', 'proto']
    filenames = ['*.proto']

    tokens = {
        'root': [
            (r'[ \t]+', Text),
            (r'[,;{}\[\]\(\)]', Punctuation),
            (r'/(\\\n)?/(\n|(.|\n)*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?\*(.|\n)*?\*(\\\n)?/', Comment.Multiline),
            (r'\b(import|option|optional|required|repeated|default|packed|'
             r'ctype|extensions|to|max|rpc|returns|oneof)\b', Keyword),
            (r'(int32|int64|uint32|uint64|sint32|sint64|'
             r'fixed32|fixed64|sfixed32|sfixed64|'
             r'float|double|bool|string|bytes)\b', Keyword.Type),
            (r'(true|false)\b', Keyword.Constant),
            (r'(package)(\s+)', bygroups(Keyword.Namespace, Text), 'package'),
            (r'(message|extend)(\s+)',
             bygroups(Keyword.Declaration, Text), 'message'),
            (r'(enum|group|service)(\s+)',
             bygroups(Keyword.Declaration, Text), 'type'),
            (r'\".*\"', String),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[LlUu]*', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'(\-?(inf|nan))', Number.Float),
            (r'0x[0-9a-fA-F]+[LlUu]*', Number.Hex),
            (r'0[0-7]+[LlUu]*', Number.Oct),
            (r'\d+[LlUu]*', Number.Integer),
            (r'[+-=]', Operator),
            (r'([a-zA-Z_][\w\.]*)([ \t]*)(=)',
             bygroups(Name.Attribute, Text, Operator)),
            ('[a-zA-Z_][\w\.]*', Name),
        ],
        'package': [
            (r'[a-zA-Z_]\w*', Name.Namespace, '#pop')
        ],
        'message': [
            (r'[a-zA-Z_]\w*', Name.Class, '#pop')
        ],
        'type': [
            (r'[a-zA-Z_]\w*', Name, '#pop')
        ],
    }


class HybrisLexer(RegexLexer):
    """
    For `Hybris <http://www.hybris-lang.org>`_ source code.

    .. versionadded:: 1.4
    """

    name = 'Hybris'
    aliases = ['hybris', 'hy']
    filenames = ['*.hy', '*.hyb']
    mimetypes = ['text/x-hybris', 'application/x-hybris']

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'root': [
            # method names
            (r'^(\s*(?:function|method|operator\s+)+?)'
             r'([a-zA-Z_]\w*)'
             r'(\s*)(\()', bygroups(Keyword, Name.Function, Text, Operator)),
            (r'[^\S\n]+', Text),
            (r'//.*?\n', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline),
            (r'@[a-zA-Z_][\w\.]*', Name.Decorator),
            (r'(break|case|catch|next|default|do|else|finally|for|foreach|of|'
             r'unless|if|new|return|switch|me|throw|try|while)\b', Keyword),
            (r'(extends|private|protected|public|static|throws|function|method|'
             r'operator)\b', Keyword.Declaration),
            (r'(true|false|null|__FILE__|__LINE__|__VERSION__|__LIB_PATH__|'
             r'__INC_PATH__)\b', Keyword.Constant),
            (r'(class|struct)(\s+)',
             bygroups(Keyword.Declaration, Text), 'class'),
            (r'(import|include)(\s+)',
             bygroups(Keyword.Namespace, Text), 'import'),
            (r'(gc_collect|gc_mm_items|gc_mm_usage|gc_collect_threshold|'
             r'urlencode|urldecode|base64encode|base64decode|sha1|crc32|sha2|'
             r'md5|md5_file|acos|asin|atan|atan2|ceil|cos|cosh|exp|fabs|floor|'
             r'fmod|log|log10|pow|sin|sinh|sqrt|tan|tanh|isint|isfloat|ischar|'
             r'isstring|isarray|ismap|isalias|typeof|sizeof|toint|tostring|'
             r'fromxml|toxml|binary|pack|load|eval|var_names|var_values|'
             r'user_functions|dyn_functions|methods|call|call_method|mknod|'
             r'mkfifo|mount|umount2|umount|ticks|usleep|sleep|time|strtime|'
             r'strdate|dllopen|dlllink|dllcall|dllcall_argv|dllclose|env|exec|'
             r'fork|getpid|wait|popen|pclose|exit|kill|pthread_create|'
             r'pthread_create_argv|pthread_exit|pthread_join|pthread_kill|'
             r'smtp_send|http_get|http_post|http_download|socket|bind|listen|'
             r'accept|getsockname|getpeername|settimeout|connect|server|recv|'
             r'send|close|print|println|printf|input|readline|serial_open|'
             r'serial_fcntl|serial_get_attr|serial_get_ispeed|serial_get_ospeed|'
             r'serial_set_attr|serial_set_ispeed|serial_set_ospeed|serial_write|'
             r'serial_read|serial_close|xml_load|xml_parse|fopen|fseek|ftell|'
             r'fsize|fread|fwrite|fgets|fclose|file|readdir|pcre_replace|size|'
             r'pop|unmap|has|keys|values|length|find|substr|replace|split|trim|'
             r'remove|contains|join)\b', Name.Builtin),
            (r'(MethodReference|Runner|Dll|Thread|Pipe|Process|Runnable|'
             r'CGI|ClientSocket|Socket|ServerSocket|File|Console|Directory|'
             r'Exception)\b', Keyword.Type),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r"'\\.'|'[^\\]'|'\\u[0-9a-f]{4}'", String.Char),
            (r'(\.)([a-zA-Z_]\w*)',
             bygroups(Operator, Name.Attribute)),
            (r'[a-zA-Z_]\w*:', Name.Label),
            (r'[a-zA-Z_\$]\w*', Name),
            (r'[~\^\*!%&\[\]\(\)\{\}<>\|+=:;,./?\-@]+', Operator),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'0x[0-9a-f]+', Number.Hex),
            (r'[0-9]+L?', Number.Integer),
            (r'\n', Text),
        ],
        'class': [
            (r'[a-zA-Z_]\w*', Name.Class, '#pop')
        ],
        'import': [
            (r'[\w.]+\*?', Name.Namespace, '#pop')
        ],
    }


class AwkLexer(RegexLexer):
    """
    For Awk scripts.

    .. versionadded:: 1.5
    """

    name = 'Awk'
    aliases = ['awk', 'gawk', 'mawk', 'nawk']
    filenames = ['*.awk']
    mimetypes = ['application/x-awk']

    tokens = {
        'commentsandwhitespace': [
            (r'\s+', Text),
            (r'#.*$', Comment.Single)
        ],
        'slashstartsregex': [
            include('commentsandwhitespace'),
            (r'/(\\.|[^[/\\\n]|\[(\\.|[^\]\\\n])*])+/'
             r'\B', String.Regex, '#pop'),
            (r'(?=/)', Text, ('#pop', 'badregex')),
            default('#pop')
        ],
        'badregex': [
            (r'\n', Text, '#pop')
        ],
        'root': [
            (r'^(?=\s|/)', Text, 'slashstartsregex'),
            include('commentsandwhitespace'),
            (r'\+\+|--|\|\||&&|in\b|\$|!?~|'
             r'(\*\*|[-<>+*%\^/!=])=?', Operator, 'slashstartsregex'),
            (r'[{(\[;,]', Punctuation, 'slashstartsregex'),
            (r'[})\].]', Punctuation),
            (r'(break|continue|do|while|exit|for|if|else|'
             r'return)\b', Keyword, 'slashstartsregex'),
            (r'function\b', Keyword.Declaration, 'slashstartsregex'),
            (r'(atan2|cos|exp|int|log|rand|sin|sqrt|srand|gensub|gsub|index|'
             r'length|match|split|sprintf|sub|substr|tolower|toupper|close|'
             r'fflush|getline|next|nextfile|print|printf|strftime|systime|'
             r'delete|system)\b', Keyword.Reserved),
            (r'(ARGC|ARGIND|ARGV|CONVFMT|ENVIRON|ERRNO|FIELDWIDTHS|FILENAME|FNR|FS|'
             r'IGNORECASE|NF|NR|OFMT|OFS|ORFS|RLENGTH|RS|RSTART|RT|'
             r'SUBSEP)\b', Name.Builtin),
            (r'[$a-zA-Z_]\w*', Name.Other),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            (r'[0-9]+', Number.Integer),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r"'(\\\\|\\'|[^'])*'", String.Single),
        ]
    }


class Cfengine3Lexer(RegexLexer):
    """
    Lexer for `CFEngine3 <http://cfengine.org>`_ policy files.

    .. versionadded:: 1.5
    """

    name = 'CFEngine3'
    aliases = ['cfengine3', 'cf3']
    filenames = ['*.cf']
    mimetypes = []

    tokens = {
        'root': [
            (r'#.*?\n', Comment),
            (r'(body)(\s+)(\S+)(\s+)(control)',
             bygroups(Keyword, Text, Keyword, Text, Keyword)),
            (r'(body|bundle)(\s+)(\S+)(\s+)(\w+)(\()',
             bygroups(Keyword, Text, Keyword, Text, Name.Function, Punctuation),
             'arglist'),
            (r'(body|bundle)(\s+)(\S+)(\s+)(\w+)',
             bygroups(Keyword, Text, Keyword, Text, Name.Function)),
            (r'(")([^"]+)(")(\s+)(string|slist|int|real)(\s*)(=>)(\s*)',
             bygroups(Punctuation,Name.Variable,Punctuation,
                      Text,Keyword.Type,Text,Operator,Text)),
            (r'(\S+)(\s*)(=>)(\s*)',
             bygroups(Keyword.Reserved,Text,Operator,Text)),
            (r'"', String, 'string'),
            (r'(\w+)(\()', bygroups(Name.Function, Punctuation)),
            (r'([\w.!&|\(\)]+)(::)', bygroups(Name.Class, Punctuation)),
            (r'(\w+)(:)', bygroups(Keyword.Declaration,Punctuation)),
            (r'@[\{\(][^\)\}]+[\}\)]', Name.Variable),
            (r'[(){},;]', Punctuation),
            (r'=>', Operator),
            (r'->', Operator),
            (r'\d+\.\d+', Number.Float),
            (r'\d+', Number.Integer),
            (r'\w+', Name.Function),
            (r'\s+', Text),
        ],
        'string': [
            (r'\$[\{\(]', String.Interpol, 'interpol'),
            (r'\\.', String.Escape),
            (r'"', String, '#pop'),
            (r'\n', String),
            (r'.', String),
        ],
        'interpol': [
            (r'\$[\{\(]', String.Interpol, '#push'),
            (r'[\}\)]', String.Interpol, '#pop'),
            (r'[^\$\{\(\)\}]+', String.Interpol),
        ],
        'arglist': [
            (r'\)', Punctuation, '#pop'),
            (r',', Punctuation),
            (r'\w+', Name.Variable),
            (r'\s+', Text),
        ],
    }


class UrbiscriptLexer(ExtendedRegexLexer):
    """
    For UrbiScript source code.

    .. versionadded:: 1.5
    """

    name = 'UrbiScript'
    aliases = ['urbiscript']
    filenames = ['*.u']
    mimetypes = ['application/x-urbiscript']

    flags = re.DOTALL

    ## TODO
    # - handle Experimental and deprecated tags with specific tokens
    # - handle Angles and Durations with specific tokens

    def blob_callback(lexer, match, ctx):
        text_before_blob = match.group(1)
        blob_start = match.group(2)
        blob_size_str = match.group(3)
        blob_size = int(blob_size_str)
        yield match.start(), String, text_before_blob
        ctx.pos += len(text_before_blob)

        # if blob size doesn't match blob format (example : "\B(2)(aaa)")
        # yield blob as a string
        if ctx.text[match.end() + blob_size] != ")":
            result = "\\B(" + blob_size_str + ")("
            yield match.start(), String, result
            ctx.pos += len(result)
            return

        # if blob is well formated, yield as Escape
        blob_text = blob_start + ctx.text[match.end():match.end()+blob_size] + ")"
        yield match.start(), String.Escape, blob_text
        ctx.pos = match.end() + blob_size + 1 # +1 is the ending ")"

    tokens = {
        'root': [
            (r'\s+', Text),
            # comments
            (r'//.*?\n', Comment),
            (r'/\*', Comment.Multiline, 'comment'),
            (r'(?:every|for|loop|while)(?:;|&|\||,)',Keyword),
            (r'(?:assert|at|break|case|catch|closure|compl|continue|'
             r'default|else|enum|every|external|finally|for|freezeif|if|new|'
             r'onleave|return|stopif|switch|this|throw|timeout|try|'
             r'waituntil|whenever|while)\b', Keyword),
            (r'(?:asm|auto|bool|char|const_cast|delete|double|dynamic_cast|'
             r'explicit|export|extern|float|friend|goto|inline|int|'
             r'long|mutable|namespace|register|reinterpret_cast|short|'
             r'signed|sizeof|static_cast|struct|template|typedef|typeid|'
             r'typename|union|unsigned|using|virtual|volatile|'
             r'wchar_t)\b', Keyword.Reserved),
            # deprecated keywords, use a meaningfull token when available
            (r'(?:emit|foreach|internal|loopn|static)\b', Keyword),
            # ignored keywords, use a meaningfull token when available
            (r'(?:private|protected|public)\b', Keyword),
            (r'(?:var|do|const|function|class)\b', Keyword.Declaration),
            (r'(?:true|false|nil|void)\b', Keyword.Constant),
            (r'(?:Barrier|Binary|Boolean|CallMessage|Channel|Code|'
             r'Comparable|Container|Control|Date|Dictionary|Directory|'
             r'Duration|Enumeration|Event|Exception|Executable|File|Finalizable|'
             r'Float|FormatInfo|Formatter|Global|Group|Hash|InputStream|'
             r'IoService|Job|Kernel|Lazy|List|Loadable|Lobby|Location|Logger|Math|'
             r'Mutex|nil|Object|Orderable|OutputStream|Pair|Path|Pattern|Position|'
             r'Primitive|Process|Profile|PseudoLazy|PubSub|RangeIterable|Regexp|'
             r'Semaphore|Server|Singleton|Socket|StackFrame|Stream|String|System|'
             r'Tag|Timeout|Traceable|TrajectoryGenerator|Triplet|Tuple'
             r'|UObject|UValue|UVar)\b', Name.Builtin),
            (r'(?:this)\b', Name.Builtin.Pseudo),
            # don't match single | and &
            (r'(?:[-=+*%/<>~^:]+|\.&?|\|\||&&)', Operator),
            (r'(?:and_eq|and|bitand|bitor|in|not|not_eq|or_eq|or|xor_eq|xor)\b',
             Operator.Word),
            (r'[{}\[\]()]+', Punctuation),
            (r'(?:;|\||,|&|\?|!)+', Punctuation),
            (r'[$a-zA-Z_]\w*', Name.Other),
            (r'0x[0-9a-fA-F]+', Number.Hex),
            # Float, Integer, Angle and Duration
            (r'(?:[0-9]+(?:(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)?'
             r'((?:rad|deg|grad)|(?:ms|s|min|h|d))?)\b', Number.Float),
            # handle binary blob in strings
            (r'"', String.Double, "string.double"),
            (r"'", String.Single, "string.single"),
        ],
        'string.double': [
            (r'((?:\\\\|\\"|[^"])*?)(\\B\((\d+)\)\()', blob_callback),
            (r'(\\\\|\\"|[^"])*?"', String.Double, '#pop'),
        ],
        'string.single': [
            (r"((?:\\\\|\\'|[^'])*?)(\\B\((\d+)\)\()", blob_callback),
            (r"(\\\\|\\'|[^'])*?'", String.Single, '#pop'),
        ],
        # from http://pygments.org/docs/lexerdevelopment/#changing-states
        'comment': [
            (r'[^*/]', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ]
    }


class BroLexer(RegexLexer):
    """
    For `Bro <http://bro-ids.org/>`_ scripts.

    .. versionadded:: 1.5
    """
    name = 'Bro'
    aliases = ['bro']
    filenames = ['*.bro']

    _hex = r'[0-9a-fA-F_]+'
    _float = r'((\d*\.?\d+)|(\d+\.?\d*))([eE][-+]?\d+)?'
    _h = r'[A-Za-z0-9][-A-Za-z0-9]*'

    tokens = {
        'root': [
            # Whitespace
            (r'^@.*?\n', Comment.Preproc),
            (r'#.*?\n', Comment.Single),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text),
            # Keywords
            (r'(add|alarm|break|case|const|continue|delete|do|else|enum|event'
             r'|export|for|function|if|global|hook|local|module|next'
             r'|of|print|redef|return|schedule|switch|type|when|while)\b', Keyword),
            (r'(addr|any|bool|count|counter|double|file|int|interval|net'
             r'|pattern|port|record|set|string|subnet|table|time|timer'
             r'|vector)\b', Keyword.Type),
            (r'(T|F)\b', Keyword.Constant),
            (r'(&)((?:add|delete|expire)_func|attr|(?:create|read|write)_expire'
             r'|default|disable_print_hook|raw_output|encrypt|group|log'
             r'|mergeable|optional|persistent|priority|redef'
             r'|rotate_(?:interval|size)|synchronized)\b', bygroups(Punctuation,
                 Keyword)),
            (r'\s+module\b', Keyword.Namespace),
            # Addresses, ports and networks
            (r'\d+/(tcp|udp|icmp|unknown)\b', Number),
            (r'(\d+\.){3}\d+', Number),
            (r'(' + _hex + r'){7}' + _hex, Number),
            (r'0x' + _hex + r'(' + _hex + r'|:)*::(' + _hex + r'|:)*', Number),
            (r'((\d+|:)(' + _hex + r'|:)*)?::(' + _hex + r'|:)*', Number),
            (r'(\d+\.\d+\.|(\d+\.){2}\d+)', Number),
            # Hostnames
            (_h + r'(\.' + _h + r')+', String),
            # Numeric
            (_float + r'\s+(day|hr|min|sec|msec|usec)s?\b', Literal.Date),
            (r'0[xX]' + _hex, Number.Hex),
            (_float, Number.Float),
            (r'\d+', Number.Integer),
            (r'/', String.Regex, 'regex'),
            (r'"', String, 'string'),
            # Operators
            (r'[!%*/+:<=>?~|-]', Operator),
            (r'([-+=&|]{2}|[+=!><-]=)', Operator),
            (r'(in|match)\b', Operator.Word),
            (r'[{}()\[\]$.,;]', Punctuation),
            # Identfier
            (r'([_a-zA-Z]\w*)(::)', bygroups(Name, Name.Namespace)),
            (r'[a-zA-Z_][a-zA-Z_0-9]*', Name)
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String),
            (r'\\\n', String),
            (r'\\', String)
        ],
        'regex': [
            (r'/', String.Regex, '#pop'),
            (r'\\[\\nt/]', String.Regex), # String.Escape is too intense here.
            (r'[^\\/\n]+', String.Regex),
            (r'\\\n', String.Regex),
            (r'\\', String.Regex)
        ]
    }


class CbmBasicV2Lexer(RegexLexer):
    """
    For CBM BASIC V2 sources.

    .. versionadded:: 1.6
    """
    name = 'CBM BASIC V2'
    aliases = ['cbmbas']
    filenames = ['*.bas']

    flags = re.IGNORECASE

    tokens = {
        'root': [
            (r'rem.*\n', Comment.Single),
            (r'\s+', Text),
            (r'new|run|end|for|to|next|step|go(to|sub)?|on|return|stop|cont'
             r'|if|then|input#?|read|wait|load|save|verify|poke|sys|print#?'
             r'|list|clr|cmd|open|close|get#?', Keyword.Reserved),
            (r'data|restore|dim|let|def|fn', Keyword.Declaration),
            (r'tab|spc|sgn|int|abs|usr|fre|pos|sqr|rnd|log|exp|cos|sin|tan|atn'
             r'|peek|len|val|asc|(str|chr|left|right|mid)\$', Name.Builtin),
            (r'[-+*/^<>=]', Operator),
            (r'not|and|or', Operator.Word),
            (r'"[^"\n]*.', String),
            (r'\d+|[-+]?\d*\.\d*(e[-+]?\d+)?', Number.Float),
            (r'[\(\),:;]', Punctuation),
            (r'\w+[$%]?', Name),
        ]
    }

    def analyse_text(self, text):
        # if it starts with a line number, it shouldn't be a "modern" Basic
        # like VB.net
        if re.match(r'\d+', text):
            return True


class MscgenLexer(RegexLexer):
    """
    For `Mscgen <http://www.mcternan.me.uk/mscgen/>`_ files.

    .. versionadded:: 1.6
    """
    name = 'Mscgen'
    aliases = ['mscgen', 'msc']
    filenames = ['*.msc']

    _var = r'(\w+|"(?:\\"|[^"])*")'

    tokens = {
        'root': [
            (r'msc\b', Keyword.Type),
            # Options
            (r'(hscale|HSCALE|width|WIDTH|wordwraparcs|WORDWRAPARCS'
             r'|arcgradient|ARCGRADIENT)\b', Name.Property),
            # Operators
            (r'(abox|ABOX|rbox|RBOX|box|BOX|note|NOTE)\b', Operator.Word),
            (r'(\.|-|\|){3}', Keyword),
            (r'(?:-|=|\.|:){2}'
             r'|<<=>>|<->|<=>|<<>>|<:>'
             r'|->|=>>|>>|=>|:>|-x|-X'
             r'|<-|<<=|<<|<=|<:|x-|X-|=', Operator),
            # Names
            (r'\*', Name.Builtin),
            (_var, Name.Variable),
            # Other
            (r'\[', Punctuation, 'attrs'),
            (r'\{|\}|,|;', Punctuation),
            include('comments')
        ],
        'attrs': [
            (r'\]', Punctuation, '#pop'),
            (_var + r'(\s*)(=)(\s*)' + _var,
             bygroups(Name.Attribute, Text.Whitespace, Operator, Text.Whitespace,
                      String)),
            (r',', Punctuation),
            include('comments')
        ],
        'comments': [
            (r'(?://|#).*?\n', Comment.Single),
            (r'/\*(?:.|\n)*?\*/', Comment.Multiline),
            (r'[ \t\r\n]+', Text.Whitespace)
        ]
    }


class VGLLexer(RegexLexer):
    """
    For `SampleManager VGL <http://www.thermoscientific.com/samplemanager>`_
    source code.

    .. versionadded:: 1.6
    """
    name = 'VGL'
    aliases = ['vgl']
    filenames = ['*.rpf']

    flags = re.MULTILINE | re.DOTALL | re.IGNORECASE

    tokens = {
        'root': [
            (r'\{[^\}]*\}', Comment.Multiline),
            (r'declare', Keyword.Constant),
            (r'(if|then|else|endif|while|do|endwhile|and|or|prompt|object'
             r'|create|on|line|with|global|routine|value|endroutine|constant'
             r'|global|set|join|library|compile_option|file|exists|create|copy'
             r'|delete|enable|windows|name|notprotected)(?! *[=<>.,()])',
             Keyword),
            (r'(true|false|null|empty|error|locked)', Keyword.Constant),
            (r'[~\^\*\#!%&\[\]\(\)<>\|+=:;,./?-]', Operator),
            (r'"[^"]*"', String),
            (r'(\.)([a-z_\$][\w\$]*)', bygroups(Operator, Name.Attribute)),
            (r'[0-9][0-9]*(\.[0-9]+(e[+\-]?[0-9]+)?)?', Number),
            (r'[a-z_\$][\w\$]*', Name),
            (r'[\r\n]+', Text),
            (r'\s+', Text)
        ]
    }


class SourcePawnLexer(RegexLexer):
    """
    For SourcePawn source code with preprocessor directives.

    .. versionadded:: 1.6
    """
    name = 'SourcePawn'
    aliases = ['sp']
    filenames = ['*.sp']
    mimetypes = ['text/x-sourcepawn']

    #: optional Comment or Whitespace
    _ws = r'(?:\s|//.*?\n|/\*.*?\*/)+'

    tokens = {
        'root': [
            # preprocessor directives: without whitespace
            ('^#if\s+0', Comment.Preproc, 'if0'),
            ('^#', Comment.Preproc, 'macro'),
            # or with whitespace
            ('^' + _ws + r'#if\s+0', Comment.Preproc, 'if0'),
            ('^' + _ws + '#', Comment.Preproc, 'macro'),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'/(\\\n)?/(\n|(.|\n)*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?\*(.|\n)*?\*(\\\n)?/', Comment.Multiline),
            (r'[{}]', Punctuation),
            (r'L?"', String, 'string'),
            (r"L?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", String.Char),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[LlUu]*', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+[LlUu]*', Number.Hex),
            (r'0[0-7]+[LlUu]*', Number.Oct),
            (r'\d+[LlUu]*', Number.Integer),
            (r'\*/', Error),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'[()\[\],.;]', Punctuation),
            (r'(case|const|continue|native|'
             r'default|else|enum|for|if|new|operator|'
             r'public|return|sizeof|static|decl|struct|switch)\b', Keyword),
            (r'(bool|Float)\b', Keyword.Type),
            (r'(true|false)\b', Keyword.Constant),
            ('[a-zA-Z_]\w*', Name),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
        'macro': [
            (r'[^/\n]+', Comment.Preproc),
            (r'/\*(.|\n)*?\*/', Comment.Multiline),
            (r'//.*?\n', Comment.Single, '#pop'),
            (r'/', Comment.Preproc),
            (r'(?<=\\)\n', Comment.Preproc),
            (r'\n', Comment.Preproc, '#pop'),
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment.Preproc, '#push'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment.Preproc, '#pop'),
            (r'.*?\n', Comment),
        ]
    }

    SM_TYPES = set(('Action', 'bool', 'Float', 'Plugin', 'String', 'any',
                'AdminFlag', 'OverrideType', 'OverrideRule', 'ImmunityType',
                'GroupId', 'AdminId', 'AdmAccessMode', 'AdminCachePart',
                'CookieAccess', 'CookieMenu', 'CookieMenuAction', 'NetFlow',
                'ConVarBounds', 'QueryCookie', 'ReplySource',
                'ConVarQueryResult', 'ConVarQueryFinished', 'Function',
                'Action', 'Identity', 'PluginStatus', 'PluginInfo', 'DBResult',
                'DBBindType', 'DBPriority', 'PropType', 'PropFieldType',
                'MoveType', 'RenderMode', 'RenderFx', 'EventHookMode',
                'EventHook', 'FileType', 'FileTimeMode', 'PathType',
                'ParamType', 'ExecType', 'DialogType', 'Handle', 'KvDataTypes',
                'NominateResult', 'MapChange', 'MenuStyle', 'MenuAction',
                'MenuSource', 'RegexError', 'SDKCallType', 'SDKLibrary',
                'SDKFuncConfSource', 'SDKType', 'SDKPassMethod', 'RayType',
                'TraceEntityFilter', 'ListenOverride', 'SortOrder', 'SortType',
                'SortFunc2D', 'APLRes', 'FeatureType', 'FeatureStatus',
                'SMCResult', 'SMCError', 'TFClassType', 'TFTeam', 'TFCond',
                'TFResourceType', 'Timer', 'TopMenuAction', 'TopMenuObjectType',
                'TopMenuPosition', 'TopMenuObject', 'UserMsg'))

    def __init__(self, **options):
        self.smhighlighting = get_bool_opt(options,
                'sourcemod', True)

        self._functions = set()
        if self.smhighlighting:
            from pygments.lexers._sourcemodbuiltins import FUNCTIONS
            self._functions.update(FUNCTIONS)
        RegexLexer.__init__(self, **options)

    def get_tokens_unprocessed(self, text):
        for index, token, value in \
            RegexLexer.get_tokens_unprocessed(self, text):
            if token is Name:
                if self.smhighlighting:
                    if value in self.SM_TYPES:
                        token = Keyword.Type
                    elif value in self._functions:
                        token = Name.Builtin
            yield index, token, value


class PuppetLexer(RegexLexer):
    """
    For `Puppet <http://puppetlabs.com/>`__ configuration DSL.

    .. versionadded:: 1.6
    """
    name = 'Puppet'
    aliases = ['puppet']
    filenames = ['*.pp']

    tokens = {
        'root': [
            include('comments'),
            include('keywords'),
            include('names'),
            include('numbers'),
            include('operators'),
            include('strings'),

            (r'[]{}:(),;[]', Punctuation),
            (r'[^\S\n]+', Text),
        ],

        'comments': [
            (r'\s*#.*$', Comment),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
        ],

        'operators': [
            (r'(=>|\?|<|>|=|\+|-|/|\*|~|!|\|)', Operator),
            (r'(in|and|or|not)\b', Operator.Word),
        ],

        'names': [
            ('[a-zA-Z_]\w*', Name.Attribute),
            (r'(\$\S+)(\[)(\S+)(\])', bygroups(Name.Variable, Punctuation,
                                               String, Punctuation)),
            (r'\$\S+', Name.Variable),
        ],

        'numbers': [
            # Copypasta from the Python lexer
            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?j?', Number.Float),
            (r'\d+[eE][+-]?[0-9]+j?', Number.Float),
            (r'0[0-7]+j?', Number.Oct),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+j?', Number.Integer)
        ],

        'keywords': [
            # Left out 'group' and 'require'
            # Since they're often used as attributes
            (r'(?i)(absent|alert|alias|audit|augeas|before|case|check|class|'
             r'computer|configured|contained|create_resources|crit|cron|debug|'
             r'default|define|defined|directory|else|elsif|emerg|err|exec|'
             r'extlookup|fail|false|file|filebucket|fqdn_rand|generate|host|if|'
             r'import|include|info|inherits|inline_template|installed|'
             r'interface|k5login|latest|link|loglevel|macauthorization|'
             r'mailalias|maillist|mcx|md5|mount|mounted|nagios_command|'
             r'nagios_contact|nagios_contactgroup|nagios_host|'
             r'nagios_hostdependency|nagios_hostescalation|nagios_hostextinfo|'
             r'nagios_hostgroup|nagios_service|nagios_servicedependency|'
             r'nagios_serviceescalation|nagios_serviceextinfo|'
             r'nagios_servicegroup|nagios_timeperiod|node|noop|notice|notify|'
             r'package|present|purged|realize|regsubst|resources|role|router|'
             r'running|schedule|scheduled_task|search|selboolean|selmodule|'
             r'service|sha1|shellquote|split|sprintf|ssh_authorized_key|sshkey|'
             r'stage|stopped|subscribe|tag|tagged|template|tidy|true|undef|'
             r'unmounted|user|versioncmp|vlan|warning|yumrepo|zfs|zone|'
             r'zpool)\b', Keyword),
        ],

        'strings': [
            (r'"([^"])*"', String),
            (r"'(\\'|[^'])*'", String),
        ],

    }


class NSISLexer(RegexLexer):
    """
    For `NSIS <http://nsis.sourceforge.net/>`_ scripts.

    .. versionadded:: 1.6
    """
    name = 'NSIS'
    aliases = ['nsis', 'nsi', 'nsh']
    filenames = ['*.nsi', '*.nsh']
    mimetypes = ['text/x-nsis']

    flags = re.IGNORECASE

    tokens = {
        'root': [
            (r'[;\#].*\n', Comment),
            (r"'.*?'", String.Single),
            (r'"', String.Double, 'str_double'),
            (r'`', String.Backtick, 'str_backtick'),
            include('macro'),
            include('interpol'),
            include('basic'),
            (r'\$\{[a-z_|][\w|]*\}', Keyword.Pseudo),
            (r'/[a-z_]\w*', Name.Attribute),
            ('.', Text),
        ],
        'basic': [
            (r'(\n)(Function)(\s+)([._a-z][.\w]*)\b',
             bygroups(Text, Keyword, Text, Name.Function)),
            (r'\b([_a-z]\w*)(::)([a-z][a-z0-9]*)\b',
             bygroups(Keyword.Namespace, Punctuation, Name.Function)),
            (r'\b([_a-z]\w*)(:)', bygroups(Name.Label, Punctuation)),
            (r'(\b[ULS]|\B)([\!\<\>=]?=|\<\>?|\>)\B', Operator),
            (r'[|+-]', Operator),
            (r'\\', Punctuation),
            (r'\b(Abort|Add(?:BrandingImage|Size)|'
             r'Allow(?:RootDirInstall|SkipFiles)|AutoCloseWindow|'
             r'BG(?:Font|Gradient)|BrandingText|BringToFront|Call(?:InstDLL)?|'
             r'(?:Sub)?Caption|ChangeUI|CheckBitmap|ClearErrors|CompletedText|'
             r'ComponentText|CopyFiles|CRCCheck|'
             r'Create(?:Directory|Font|Shortcut)|Delete(?:INI(?:Sec|Str)|'
             r'Reg(?:Key|Value))?|DetailPrint|DetailsButtonText|'
             r'Dir(?:Show|Text|Var|Verify)|(?:Disabled|Enabled)Bitmap|'
             r'EnableWindow|EnumReg(?:Key|Value)|Exch|Exec(?:Shell|Wait)?|'
             r'ExpandEnvStrings|File(?:BufSize|Close|ErrorText|Open|'
             r'Read(?:Byte)?|Seek|Write(?:Byte)?)?|'
             r'Find(?:Close|First|Next|Window)|FlushINI|Function(?:End)?|'
             r'Get(?:CurInstType|CurrentAddress|DlgItem|DLLVersion(?:Local)?|'
             r'ErrorLevel|FileTime(?:Local)?|FullPathName|FunctionAddress|'
             r'InstDirError|LabelAddress|TempFileName)|'
             r'Goto|HideWindow|Icon|'
             r'If(?:Abort|Errors|FileExists|RebootFlag|Silent)|'
             r'InitPluginsDir|Install(?:ButtonText|Colors|Dir(?:RegKey)?)|'
             r'Inst(?:ProgressFlags|Type(?:[GS]etText)?)|Int(?:CmpU?|Fmt|Op)|'
             r'IsWindow|LangString(?:UP)?|'
             r'License(?:BkColor|Data|ForceSelection|LangString|Text)|'
             r'LoadLanguageFile|LockWindow|Log(?:Set|Text)|MessageBox|'
             r'MiscButtonText|Name|Nop|OutFile|(?:Uninst)?Page(?:Ex(?:End)?)?|'
             r'PluginDir|Pop|Push|Quit|Read(?:(?:Env|INI|Reg)Str|RegDWORD)|'
             r'Reboot|(?:Un)?RegDLL|Rename|RequestExecutionLevel|ReserveFile|'
             r'Return|RMDir|SearchPath|Section(?:Divider|End|'
             r'(?:(?:Get|Set)(?:Flags|InstTypes|Size|Text))|Group(?:End)?|In)?|'
             r'SendMessage|Set(?:AutoClose|BrandingImage|Compress(?:ionLevel|'
             r'or(?:DictSize)?)?|CtlColors|CurInstType|DatablockOptimize|'
             r'DateSave|Details(?:Print|View)|Error(?:s|Level)|FileAttributes|'
             r'Font|OutPath|Overwrite|PluginUnload|RebootFlag|ShellVarContext|'
             r'Silent|StaticBkColor)|'
             r'Show(?:(?:I|Uni)nstDetails|Window)|Silent(?:Un)?Install|Sleep|'
             r'SpaceTexts|Str(?:CmpS?|Cpy|Len)|SubSection(?:End)?|'
             r'Uninstall(?:ButtonText|(?:Sub)?Caption|EXEName|Icon|Text)|'
             r'UninstPage|Var|VI(?:AddVersionKey|ProductVersion)|WindowIcon|'
             r'Write(?:INIStr|Reg(:?Bin|DWORD|(?:Expand)?Str)|Uninstaller)|'
             r'XPStyle)\b', Keyword),
            (r'\b(CUR|END|(?:FILE_ATTRIBUTE_)?'
             r'(?:ARCHIVE|HIDDEN|NORMAL|OFFLINE|READONLY|SYSTEM|TEMPORARY)|'
             r'HK(CC|CR|CU|DD|LM|PD|U)|'
             r'HKEY_(?:CLASSES_ROOT|CURRENT_(?:CONFIG|USER)|DYN_DATA|'
             r'LOCAL_MACHINE|PERFORMANCE_DATA|USERS)|'
             r'ID(?:ABORT|CANCEL|IGNORE|NO|OK|RETRY|YES)|'
             r'MB_(?:ABORTRETRYIGNORE|DEFBUTTON[1-4]|'
             r'ICON(?:EXCLAMATION|INFORMATION|QUESTION|STOP)|'
             r'OK(?:CANCEL)?|RETRYCANCEL|RIGHT|SETFOREGROUND|TOPMOST|USERICON|'
             r'YESNO(?:CANCEL)?)|SET|SHCTX|'
             r'SW_(?:HIDE|SHOW(?:MAXIMIZED|MINIMIZED|NORMAL))|'
             r'admin|all|auto|both|bottom|bzip2|checkbox|colored|current|false|'
             r'force|hide|highest|if(?:diff|newer)|lastused|leave|left|'
             r'listonly|lzma|nevershow|none|normal|off|on|pop|push|'
             r'radiobuttons|right|show|silent|silentlog|smooth|textonly|top|'
             r'true|try|user|zlib)\b', Name.Constant),
        ],
        'macro': [
            (r'\!(addincludedir(?:dir)?|addplugindir|appendfile|cd|define|'
             r'delfilefile|echo(?:message)?|else|endif|error|execute|'
             r'if(?:macro)?n?(?:def)?|include|insertmacro|macro(?:end)?|packhdr|'
             r'search(?:parse|replace)|system|tempfilesymbol|undef|verbose|'
             r'warning)\b', Comment.Preproc),
        ],
        'interpol': [
            (r'\$(R?[0-9])', Name.Builtin.Pseudo),    # registers
            (r'\$(ADMINTOOLS|APPDATA|CDBURN_AREA|COOKIES|COMMONFILES(?:32|64)|'
            r'DESKTOP|DOCUMENTS|EXE(?:DIR|FILE|PATH)|FAVORITES|FONTS|HISTORY|'
            r'HWNDPARENT|INTERNET_CACHE|LOCALAPPDATA|MUSIC|NETHOOD|PICTURES|'
            r'PLUGINSDIR|PRINTHOOD|PROFILE|PROGRAMFILES(?:32|64)|QUICKLAUNCH|'
            r'RECENT|RESOURCES(?:_LOCALIZED)?|SENDTO|SM(?:PROGRAMS|STARTUP)|'
            r'STARTMENU|SYSDIR|TEMP(?:LATES)?|VIDEOS|WINDIR|\{NSISDIR\})',
             Name.Builtin),
            (r'\$(CMDLINE|INSTDIR|OUTDIR|LANGUAGE)', Name.Variable.Global),
            (r'\$[a-z_]\w*', Name.Variable),
        ],
        'str_double': [
            (r'"', String, '#pop'),
            (r'\$(\\[nrt"]|\$)', String.Escape),
            include('interpol'),
            (r'.', String.Double),
        ],
        'str_backtick': [
            (r'`', String, '#pop'),
            (r'\$(\\[nrt"]|\$)', String.Escape),
            include('interpol'),
            (r'.', String.Double),
        ],
    }


class RPMSpecLexer(RegexLexer):
    """
    For RPM ``.spec`` files.

    .. versionadded:: 1.6
    """

    name = 'RPMSpec'
    aliases = ['spec']
    filenames = ['*.spec']
    mimetypes = ['text/x-rpm-spec']

    _directives = ('(?:package|prep|build|install|clean|check|pre[a-z]*|'
                   'post[a-z]*|trigger[a-z]*|files)')

    tokens = {
        'root': [
            (r'#.*\n', Comment),
            include('basic'),
        ],
        'description': [
            (r'^(%' + _directives + ')(.*)$',
             bygroups(Name.Decorator, Text), '#pop'),
            (r'\n', Text),
            (r'.', Text),
        ],
        'changelog': [
            (r'\*.*\n', Generic.Subheading),
            (r'^(%' + _directives + ')(.*)$',
             bygroups(Name.Decorator, Text), '#pop'),
            (r'\n', Text),
            (r'.', Text),
        ],
        'string': [
            (r'"', String.Double, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            include('interpol'),
            (r'.', String.Double),
        ],
        'basic': [
            include('macro'),
            (r'(?i)^(Name|Version|Release|Epoch|Summary|Group|License|Packager|'
             r'Vendor|Icon|URL|Distribution|Prefix|Patch[0-9]*|Source[0-9]*|'
             r'Requires\(?[a-z]*\)?|[a-z]+Req|Obsoletes|Suggests|Provides|Conflicts|'
             r'Build[a-z]+|[a-z]+Arch|Auto[a-z]+)(:)(.*)$',
             bygroups(Generic.Heading, Punctuation, using(this))),
            (r'^%description', Name.Decorator, 'description'),
            (r'^%changelog', Name.Decorator, 'changelog'),
            (r'^(%' + _directives + ')(.*)$', bygroups(Name.Decorator, Text)),
            (r'%(attr|defattr|dir|doc(?:dir)?|setup|config(?:ure)?|'
             r'make(?:install)|ghost|patch[0-9]+|find_lang|exclude|verify)',
             Keyword),
            include('interpol'),
            (r"'.*?'", String.Single),
            (r'"', String.Double, 'string'),
            (r'.', Text),
        ],
        'macro': [
            (r'%define.*\n', Comment.Preproc),
            (r'%\{\!\?.*%define.*\}', Comment.Preproc),
            (r'(%(?:if(?:n?arch)?|else(?:if)?|endif))(.*)$',
             bygroups(Comment.Preproc, Text)),
        ],
        'interpol': [
            (r'%\{?__[a-z_]+\}?', Name.Function),
            (r'%\{?_([a-z_]+dir|[a-z_]+path|prefix)\}?', Keyword.Pseudo),
            (r'%\{\?\w+\}', Name.Variable),
            (r'\$\{?RPM_[A-Z0-9_]+\}?', Name.Variable.Global),
            (r'%\{[a-zA-Z]\w+\}', Keyword.Constant),
        ]
    }


class AutoItLexer(RegexLexer):
    """
    For `AutoIt <http://www.autoitscript.com/site/autoit/>`_ files.

    AutoIt is a freeware BASIC-like scripting language
    designed for automating the Windows GUI and general scripting

    .. versionadded:: 1.6
    """
    name = 'AutoIt'
    aliases = ['autoit']
    filenames = ['*.au3']
    mimetypes = ['text/x-autoit']

    # Keywords, functions, macros from au3.keywords.properties
    # which can be found in AutoIt installed directory, e.g.
    # c:\Program Files (x86)\AutoIt3\SciTE\au3.keywords.properties

    keywords = """\
    #include-once #include #endregion #forcedef #forceref #region
    and byref case continueloop dim do else elseif endfunc endif
    endselect exit exitloop for func global
    if local next not or return select step
    then to until wend while exit""".split()

    functions = """\
    abs acos adlibregister adlibunregister asc ascw asin assign atan
    autoitsetoption autoitwingettitle autoitwinsettitle beep binary binarylen
    binarymid binarytostring bitand bitnot bitor bitrotate bitshift bitxor
    blockinput break call cdtray ceiling chr chrw clipget clipput consoleread
    consolewrite consolewriteerror controlclick controlcommand controldisable
    controlenable controlfocus controlgetfocus controlgethandle controlgetpos
    controlgettext controlhide controllistview controlmove controlsend
    controlsettext controlshow controltreeview cos dec dircopy dircreate
    dirgetsize dirmove dirremove dllcall dllcalladdress dllcallbackfree
    dllcallbackgetptr dllcallbackregister dllclose dllopen dllstructcreate
    dllstructgetdata dllstructgetptr dllstructgetsize dllstructsetdata
    drivegetdrive drivegetfilesystem drivegetlabel drivegetserial drivegettype
    drivemapadd drivemapdel drivemapget drivesetlabel drivespacefree
    drivespacetotal drivestatus envget envset envupdate eval execute exp
    filechangedir fileclose filecopy filecreatentfslink filecreateshortcut
    filedelete fileexists filefindfirstfile filefindnextfile fileflush
    filegetattrib filegetencoding filegetlongname filegetpos filegetshortcut
    filegetshortname filegetsize filegettime filegetversion fileinstall filemove
    fileopen fileopendialog fileread filereadline filerecycle filerecycleempty
    filesavedialog fileselectfolder filesetattrib filesetpos filesettime
    filewrite filewriteline floor ftpsetproxy guicreate guictrlcreateavi
    guictrlcreatebutton guictrlcreatecheckbox guictrlcreatecombo
    guictrlcreatecontextmenu guictrlcreatedate guictrlcreatedummy
    guictrlcreateedit guictrlcreategraphic guictrlcreategroup guictrlcreateicon
    guictrlcreateinput guictrlcreatelabel guictrlcreatelist
    guictrlcreatelistview guictrlcreatelistviewitem guictrlcreatemenu
    guictrlcreatemenuitem guictrlcreatemonthcal guictrlcreateobj
    guictrlcreatepic guictrlcreateprogress guictrlcreateradio
    guictrlcreateslider guictrlcreatetab guictrlcreatetabitem
    guictrlcreatetreeview guictrlcreatetreeviewitem guictrlcreateupdown
    guictrldelete guictrlgethandle guictrlgetstate guictrlread guictrlrecvmsg
    guictrlregisterlistviewsort guictrlsendmsg guictrlsendtodummy
    guictrlsetbkcolor guictrlsetcolor guictrlsetcursor guictrlsetdata
    guictrlsetdefbkcolor guictrlsetdefcolor guictrlsetfont guictrlsetgraphic
    guictrlsetimage guictrlsetlimit guictrlsetonevent guictrlsetpos
    guictrlsetresizing guictrlsetstate guictrlsetstyle guictrlsettip guidelete
    guigetcursorinfo guigetmsg guigetstyle guiregistermsg guisetaccelerators
    guisetbkcolor guisetcoord guisetcursor guisetfont guisethelp guiseticon
    guisetonevent guisetstate guisetstyle guistartgroup guiswitch hex hotkeyset
    httpsetproxy httpsetuseragent hwnd inetclose inetget inetgetinfo inetgetsize
    inetread inidelete iniread inireadsection inireadsectionnames
    inirenamesection iniwrite iniwritesection inputbox int isadmin isarray
    isbinary isbool isdeclared isdllstruct isfloat ishwnd isint iskeyword
    isnumber isobj isptr isstring log memgetstats mod mouseclick mouseclickdrag
    mousedown mousegetcursor mousegetpos mousemove mouseup mousewheel msgbox
    number objcreate objcreateinterface objevent objevent objget objname
    onautoitexitregister onautoitexitunregister opt ping pixelchecksum
    pixelgetcolor pixelsearch pluginclose pluginopen processclose processexists
    processgetstats processlist processsetpriority processwait processwaitclose
    progressoff progresson progressset ptr random regdelete regenumkey
    regenumval regread regwrite round run runas runaswait runwait send
    sendkeepactive seterror setextended shellexecute shellexecutewait shutdown
    sin sleep soundplay soundsetwavevolume splashimageon splashoff splashtexton
    sqrt srandom statusbargettext stderrread stdinwrite stdioclose stdoutread
    string stringaddcr stringcompare stringformat stringfromasciiarray
    stringinstr stringisalnum stringisalpha stringisascii stringisdigit
    stringisfloat stringisint stringislower stringisspace stringisupper
    stringisxdigit stringleft stringlen stringlower stringmid stringregexp
    stringregexpreplace stringreplace stringright stringsplit stringstripcr
    stringstripws stringtoasciiarray stringtobinary stringtrimleft
    stringtrimright stringupper tan tcpaccept tcpclosesocket tcpconnect
    tcplisten tcpnametoip tcprecv tcpsend tcpshutdown tcpstartup timerdiff
    timerinit tooltip traycreateitem traycreatemenu traygetmsg trayitemdelete
    trayitemgethandle trayitemgetstate trayitemgettext trayitemsetonevent
    trayitemsetstate trayitemsettext traysetclick trayseticon traysetonevent
    traysetpauseicon traysetstate traysettooltip traytip ubound udpbind
    udpclosesocket udpopen udprecv udpsend udpshutdown udpstartup vargettype
    winactivate winactive winclose winexists winflash wingetcaretpos
    wingetclasslist wingetclientsize wingethandle wingetpos wingetprocess
    wingetstate wingettext wingettitle winkill winlist winmenuselectitem
    winminimizeall winminimizeallundo winmove winsetontop winsetstate
    winsettitle winsettrans winwait winwaitactive winwaitclose
    winwaitnotactive""".split()

    macros = """\
    @appdatacommondir @appdatadir @autoitexe @autoitpid @autoitversion
    @autoitx64 @com_eventobj @commonfilesdir @compiled @computername @comspec
    @cpuarch @cr @crlf @desktopcommondir @desktopdepth @desktopdir
    @desktopheight @desktoprefresh @desktopwidth @documentscommondir @error
    @exitcode @exitmethod @extended @favoritescommondir @favoritesdir
    @gui_ctrlhandle @gui_ctrlid @gui_dragfile @gui_dragid @gui_dropid
    @gui_winhandle @homedrive @homepath @homeshare @hotkeypressed @hour
    @ipaddress1 @ipaddress2 @ipaddress3 @ipaddress4 @kblayout @lf
    @logondnsdomain @logondomain @logonserver @mday @min @mon @msec @muilang
    @mydocumentsdir @numparams @osarch @osbuild @oslang @osservicepack @ostype
    @osversion @programfilesdir @programscommondir @programsdir @scriptdir
    @scriptfullpath @scriptlinenumber @scriptname @sec @startmenucommondir
    @startmenudir @startupcommondir @startupdir @sw_disable @sw_enable @sw_hide
    @sw_lock @sw_maximize @sw_minimize @sw_restore @sw_show @sw_showdefault
    @sw_showmaximized @sw_showminimized @sw_showminnoactive @sw_showna
    @sw_shownoactivate @sw_shownormal @sw_unlock @systemdir @tab @tempdir
    @tray_id @trayiconflashing @trayiconvisible @username @userprofiledir @wday
    @windowsdir @workingdir @yday @year""".split()

    tokens = {
        'root': [
            (r';.*\n', Comment.Single),
            (r'(#comments-start|#cs).*?(#comments-end|#ce)', Comment.Multiline),
            (r'[\[\]{}(),;]', Punctuation),
            (r'(and|or|not)\b', Operator.Word),
            (r'[\$|@][a-zA-Z_]\w*', Name.Variable),
            (r'!=|==|:=|\.=|<<|>>|[-~+/*%=<>&^|?:!.]', Operator),
            include('commands'),
            include('labels'),
            include('builtInFunctions'),
            include('builtInMarcros'),
            (r'"', String, combined('stringescape', 'dqs')),
            include('numbers'),
            (r'[a-zA-Z_#@$][\w#@$]*', Name),
            (r'\\|\'', Text),
            (r'\`([\,\%\`abfnrtv\-\+;])', String.Escape),
            (r'_\n', Text),  # Line continuation
            include('garbage'),
        ],
        'commands': [
            (r'(?i)(\s*)(%s)\b' % '|'.join(keywords),
            bygroups(Text, Name.Builtin)),
        ],
        'builtInFunctions': [
            (r'(?i)(%s)\b' % '|'.join(functions),
             Name.Function),
        ],
        'builtInMarcros': [
            (r'(?i)(%s)\b' % '|'.join(macros),
             Name.Variable.Global),
        ],
        'labels': [
            # sendkeys
            (r'(^\s*)({\S+?})', bygroups(Text, Name.Label)),
        ],
        'numbers': [
            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
            (r'\d+[eE][+-]?[0-9]+', Number.Float),
            (r'0\d+', Number.Oct),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+', Number.Integer)
        ],
        'stringescape': [
            (r'\"\"|\`([\,\%\`abfnrtv])', String.Escape),
        ],
        'strings': [
            (r'[^"\n]+', String),
        ],
        'dqs': [
            (r'"', String, '#pop'),
            include('strings')
        ],
        'garbage': [
            (r'[^\S\n]', Text),
        ],
    }


class RexxLexer(RegexLexer):
    """
    `Rexx <http://www.rexxinfo.org/>`_ is a scripting language available for
    a wide range of different platforms with its roots found on mainframe
    systems. It is popular for I/O- and data based tasks and can act as glue
    language to bind different applications together.

    .. versionadded:: 2.0
    """
    name = 'Rexx'
    aliases = ['rexx', 'arexx']
    filenames = ['*.rexx', '*.rex', '*.rx', '*.arexx']
    mimetypes = ['text/x-rexx']
    flags = re.IGNORECASE

    tokens = {
        'root': [
            (r'\s', Whitespace),
            (r'/\*', Comment.Multiline, 'comment'),
            (r'"', String, 'string_double'),
            (r"'", String, 'string_single'),
            (r'[0-9]+(\.[0-9]+)?(e[+-]?[0-9])?', Number),
            (r'([a-z_]\w*)(\s*)(:)(\s*)(procedure)\b',
             bygroups(Name.Function, Whitespace, Operator, Whitespace,
                      Keyword.Declaration)),
            (r'([a-z_]\w*)(\s*)(:)',
             bygroups(Name.Label, Whitespace, Operator)),
            include('function'),
            include('keyword'),
            include('operator'),
            (r'[a-z_]\w*', Text),
        ],
        'function': [
            (r'(abbrev|abs|address|arg|b2x|bitand|bitor|bitxor|c2d|c2x|'
             r'center|charin|charout|chars|compare|condition|copies|d2c|'
             r'd2x|datatype|date|delstr|delword|digits|errortext|form|'
             r'format|fuzz|insert|lastpos|left|length|linein|lineout|lines|'
             r'max|min|overlay|pos|queued|random|reverse|right|sign|'
             r'sourceline|space|stream|strip|substr|subword|symbol|time|'
             r'trace|translate|trunc|value|verify|word|wordindex|'
             r'wordlength|wordpos|words|x2b|x2c|x2d|xrange)(\s*)(\()',
             bygroups(Name.Builtin, Whitespace, Operator)),
        ],
        'keyword': [
            (r'(address|arg|by|call|do|drop|else|end|exit|for|forever|if|'
             r'interpret|iterate|leave|nop|numeric|off|on|options|parse|'
             r'pull|push|queue|return|say|select|signal|to|then|trace|until|'
             r'while)\b', Keyword.Reserved),
        ],
        'operator': [
            (r'(-|//|/|\(|\)|\*\*|\*|\\<<|\\<|\\==|\\=|\\>>|\\>|\\|\|\||\||'
             r'&&|&|%|\+|<<=|<<|<=|<>|<|==|=|><|>=|>>=|>>|>|¬<<|¬<|¬==|¬=|'
             r'¬>>|¬>|¬|\.|,)', Operator),
        ],
        'string_double': [
            (r'[^"\n]+', String),
            (r'""', String),
            (r'"', String, '#pop'),
            (r'\n', Text, '#pop'),  # Stray linefeed also terminates strings.
        ],
        'string_single': [
            (r'[^\'\n]', String),
            (r'\'\'', String),
            (r'\'', String, '#pop'),
            (r'\n', Text, '#pop'),  # Stray linefeed also terminates strings.
        ],
        'comment': [
            (r'[^*]+', Comment.Multiline),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'\*', Comment.Multiline),
        ]
    }

    _c = lambda s: re.compile(s, re.MULTILINE)
    _ADDRESS_COMMAND_PATTERN = _c(r'^\s*address\s+command\b')
    _ADDRESS_PATTERN = _c(r'^\s*address\s+')
    _DO_WHILE_PATTERN = _c(r'^\s*do\s+while\b')
    _IF_THEN_DO_PATTERN = _c(r'^\s*if\b.+\bthen\s+do\s*$')
    _PROCEDURE_PATTERN = _c(r'^\s*([a-z_]\w*)(\s*)(:)(\s*)(procedure)\b')
    _ELSE_DO_PATTERN = _c(r'\belse\s+do\s*$')
    _PARSE_ARG_PATTERN = _c(r'^\s*parse\s+(upper\s+)?(arg|value)\b')
    PATTERNS_AND_WEIGHTS = (
        (_ADDRESS_COMMAND_PATTERN, 0.2),
        (_ADDRESS_PATTERN, 0.05),
        (_DO_WHILE_PATTERN, 0.1),
        (_ELSE_DO_PATTERN, 0.1),
        (_IF_THEN_DO_PATTERN, 0.1),
        (_PROCEDURE_PATTERN, 0.5),
        (_PARSE_ARG_PATTERN, 0.2),
    )

    def analyse_text(text):
        """
        Check for inital comment and patterns that distinguish Rexx from other
        C-like languages.
        """
        if re.search(r'/\*\**\s*rexx', text, re.IGNORECASE):
            # Header matches MVS Rexx requirements, this is certainly a Rexx
            # script.
            return 1.0
        elif text.startswith('/*'):
            # Header matches general Rexx requirements; the source code might
            # still be any language using C comments such as C++, C# or Java.
            lowerText = text.lower()
            result = sum(weight
                         for (pattern, weight) in RexxLexer.PATTERNS_AND_WEIGHTS
                         if pattern.search(lowerText)) + 0.01
            return min(result, 1.0)


class APLLexer(RegexLexer):
    """
    A simple APL lexer.

    .. versionadded:: 2.0
    """
    name = 'APL'
    aliases = ['apl']
    filenames = ['*.apl']

    tokens = {
        'root': [
            # Whitespace
            # ==========
            (r'\s+', Text),
            #
            # Comment
            # =======
            # '⍝' is traditional; '#' is supported by GNU APL and NGN (but not Dyalog)
            (u'[⍝#].*$', Comment.Single),
            #
            # Strings
            # =======
            (r'\'((\'\')|[^\'])*\'', String.Single),
            (r'"(("")|[^"])*"', String.Double), # supported by NGN APL
            #
            # Punctuation
            # ===========
            # This token type is used for diamond and parenthesis
            # but not for bracket and ; (see below)
            (u'[⋄◇()]', Punctuation),
            #
            # Array indexing
            # ==============
            # Since this token type is very important in APL, it is not included in
            # the punctuation token type but rather in the following one
            (r'[\[\];]', String.Regex),
            #
            # Distinguished names
            # ===================
            # following IBM APL2 standard
            (u'⎕[A-Za-zΔ∆⍙][A-Za-zΔ∆⍙_¯0-9]*', Name.Function),
            #
            # Labels
            # ======
            # following IBM APL2 standard
            # (u'[A-Za-zΔ∆⍙][A-Za-zΔ∆⍙_¯0-9]*:', Name.Label),
            #
            # Variables
            # =========
            # following IBM APL2 standard
            (u'[A-Za-zΔ∆⍙][A-Za-zΔ∆⍙_¯0-9]*', Name.Variable),
            #
            # Numbers
            # =======
            (u'¯?(0[Xx][0-9A-Fa-f]+|[0-9]*\.?[0-9]+([Ee][+¯]?[0-9]+)?|¯|∞)'
             u'([Jj]¯?(0[Xx][0-9A-Fa-f]+|[0-9]*\.?[0-9]+([Ee][+¯]?[0-9]+)?|¯|∞))?',
             Number),
            #
            # Operators
            # ==========
            (u'[\.\\\/⌿⍀¨⍣⍨⍠⍤∘]', Name.Attribute), # closest token type
            (u'[+\-×÷⌈⌊∣|⍳?*⍟○!⌹<≤=>≥≠≡≢∊⍷∪∩~∨∧⍱⍲⍴,⍪⌽⊖⍉↑↓⊂⊃⌷⍋⍒⊤⊥⍕⍎⊣⊢⍁⍂≈⌸⍯↗]',
             Operator),
            #
            # Constant
            # ========
            (u'⍬', Name.Constant),
            #
            # Quad symbol
            # ===========
            (u'[⎕⍞]', Name.Variable.Global),
            #
            # Arrows left/right
            # =================
            (u'[←→]', Keyword.Declaration),
            #
            # D-Fn
            # ====
            (u'[⍺⍵⍶⍹∇:]', Name.Builtin.Pseudo),
            (r'[{}]', Keyword.Type),
        ],
    }


class AmbientTalkLexer(RegexLexer):
    """
    Lexer for `AmbientTalk <https://code.google.com/p/ambienttalk>`_ source code.

    .. versionadded:: 2.0
    """
    name = 'AmbientTalk'
    filenames = ['*.at']
    aliases = ['at', 'ambienttalk', 'ambienttalk/2']
    mimetypes = ['text/x-ambienttalk']

    flags = re.MULTILINE | re.DOTALL

    builtin = ['if:', 'then:', 'else:', 'when:', 'whenever:', 'discovered:',
        'disconnected:', 'reconnected:', 'takenOffline:', 'becomes:',
        'export:', 'as:', 'object:', 'actor:', 'mirror:', 'taggedAs:',
        'mirroredBy:', 'is:']
    tokens = {
        'root' : [
            (r'\s+', Text),
            (r'//.*?\n', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline),
            (r'(def|deftype|import|alias|exclude)\b', Keyword),
            (r"(%s)" % "|".join(builtin), Name.Builtin),
            (r'(true|false|nil)\b', Keyword.Constant),
            (r'(~|lobby|jlobby|/)\.', Keyword.Constant, 'namespace'),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r'\|', Punctuation, 'arglist'),
            (r'<:|[\^\*!%&<>+=,./?-]|:=', Operator),
            (r"`[a-zA-Z_]\w*", String.Symbol),
            (r"[a-zA-Z_]\w*:", Name.Function),
            (r"[\{\}()\[\];`]", Punctuation),
            (r'(self|super)\b', Name.Variable.Instance),
            (r"[a-zA-Z_]\w*", Name.Variable),
            (r"@[a-zA-Z_]\w*", Name.Class),
            (r"@\[", Name.Class, 'annotations'),
            include('numbers'),
        ],
        'numbers' : [
            (r'(\d+\.\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
            (r'\d+', Number.Integer)
        ],
        'namespace': [
            (r'[a-zA-Z_]\w*\.', Name.Namespace),
            (r'[a-zA-Z_]\w*:', Name.Function , '#pop'),
            (r'[a-zA-Z_]\w*(?!\.)', Name.Function , '#pop')
        ],
        'annotations' : [
            (r"(.*?)\]", Name.Class, '#pop')
        ],
        'arglist' : [
            (r'\|', Punctuation, '#pop'),
            (r'\s*(,)\s*', Punctuation),
            (r'[a-zA-Z_]\w*', Name.Variable),
        ],
    }


class PawnLexer(RegexLexer):
    """
    For Pawn source code
    """

    name = 'Pawn'
    aliases = ['pawn']
    filenames = ['*.p', '*.pwn', '*.inc']
    mimetypes = ['text/x-pawn']

    #: optional Comment or Whitespace
    _ws = r'(?:\s|//.*?\n|/[*][\w\W]*?[*]/)+'

    tokens = {
        'root': [
            # preprocessor directives: without whitespace
            ('^#if\s+0', Comment.Preproc, 'if0'),
            ('^#', Comment.Preproc, 'macro'),
            # or with whitespace
            ('^' + _ws + r'#if\s+0', Comment.Preproc, 'if0'),
            ('^' + _ws + '#', Comment.Preproc, 'macro'),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'/(\\\n)?/(\n|(.|\n)*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?\*[\w\W]*?\*(\\\n)?/', Comment.Multiline),
            (r'[{}]', Punctuation),
            (r'L?"', String, 'string'),
            (r"L?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", String.Char),
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[LlUu]*', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+[LlUu]*', Number.Hex),
            (r'0[0-7]+[LlUu]*', Number.Oct),
            (r'\d+[LlUu]*', Number.Integer),
            (r'\*/', Error),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'[()\[\],.;]', Punctuation),
            (r'(switch|case|default|const|new|static|char|continue|break|'
             r'if|else|for|while|do|operator|enum|'
             r'public|return|sizeof|tagof|state|goto)\b', Keyword),
            (r'(bool|Float)\b', Keyword.Type),
            (r'(true|false)\b', Keyword.Constant),
            ('[a-zA-Z_]\w*', Name),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String), # all other characters
            (r'\\\n', String), # line continuation
            (r'\\', String), # stray backslash
        ],
        'macro': [
            (r'[^/\n]+', Comment.Preproc),
            (r'/\*(.|\n)*?\*/', Comment.Multiline),
            (r'//.*?\n', Comment.Single, '#pop'),
            (r'/', Comment.Preproc),
            (r'(?<=\\)\n', Comment.Preproc),
            (r'\n', Comment.Preproc, '#pop'),
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment.Preproc, '#push'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment.Preproc, '#pop'),
            (r'.*?\n', Comment),
        ]
    }


class RslLexer(RegexLexer):
    """
    `RSL <http://en.wikipedia.org/wiki/RAISE>`_ is the formal specification
    language used in RAISE (Rigorous Approach to Industrial Software Engineering)
    method.

    .. versionadded:: 2.0
    """
    name = 'RSL'
    aliases = ['rsl']
    filenames = ['*.rsl']
    mimetypes = ['text/rsl']

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'root':[
            (r'\b(Bool|Char|Int|Nat|Real|Text|Unit|abs|all|always|any|as|'
             r'axiom|card|case|channel|chaos|class|devt_relation|dom|elems|'
             r'else|elif|end|exists|extend|false|for|hd|hide|if|in|is|inds|'
             r'initialise|int|inter|isin|len|let|local|ltl_assertion|object|'
             r'of|out|post|pre|read|real|rng|scheme|skip|stop|swap|then|'
             r'thoery|test_case|tl|transition_system|true|type|union|until|'
             r'use|value|variable|while|with|write|~isin|-inflist|-infset|'
             r'-list|-set)\b', Keyword),
            (r'(variable|value)\b', Keyword.Declaration),
            (r'--.*?\n', Comment),
            (r'<:.*?:>', Comment),
            (r'\{!.*?!\}', Comment),
            (r'/\*.*?\*/', Comment),
            (r'^[ \t]*([\w]+)[ \t]*:[^:]', Name.Function),
            (r'(^[ \t]*)([\w]+)([ \t]*\([\w\s,]*\)[ \t]*)(is|as)',
             bygroups(Text, Name.Function, Text, Keyword)),
            (r'\b[A-Z]\w*\b',Keyword.Type),
            (r'(true|false)\b', Keyword.Constant),
            (r'".*"',String),
            (r'\'.\'',String.Char),
            (r'(><|->|-m->|/\\|<=|<<=|<\.|\|\||\|\^\||-~->|-~m->|\\/|>=|>>|'
             r'\.>|\+\+|-\\|<->|=>|:-|~=|\*\*|<<|>>=|\+>|!!|\|=\||#)',
             Operator),
            (r'[0-9]+\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'0x[0-9a-f]+', Number.Hex),
            (r'[0-9]+', Number.Integer),
            (r'.', Text),
       ],
   }

    def analyse_text(text):
        """
        Check for the most common text in the beginning of a RSL file.
        """
        if re.search(r'scheme\s*.*?=\s*class\s*type', text, re.I) is not None:
            return 1.0
        else:
            return 0.01


class PanLexer(RegexLexer):
    """
    Lexer for `pan <http://github.com/quattor/pan/>`_ source files.

    Based on tcsh lexer.

    .. versionadded:: 2.0
    """

    name = 'Pan'
    aliases = ['pan']
    filenames = ['*.pan']

    tokens = {
        'root': [
            include('basic'),
            (r'\(', Keyword, 'paren'),
            (r'{', Keyword, 'curly'),
            include('data'),
        ],
        'basic': [
            (r'\b(if|for|with|else|type|bind|while|valid|final|prefix|unique|'
             r'object|foreach|include|template|function|variable|structure|'
             r'extensible|declaration)\s*\b',
             Keyword),
            (r'\b(file_contents|format|index|length|match|matches|replace|'
             r'splice|split|substr|to_lowercase|to_uppercase|debug|error|'
             r'traceback|deprecated|base64_decode|base64_encode|digest|escape|'
             r'unescape|append|create|first|nlist|key|length|list|merge|next|'
             r'prepend|splice|is_boolean|is_defined|is_double|is_list|is_long|'
             r'is_nlist|is_null|is_number|is_property|is_resource|is_string|'
             r'to_boolean|to_double|to_long|to_string|clone|delete|exists|'
             r'path_exists|if_exists|return|value)\s*\b',
             Name.Builtin),
            (r'#.*', Comment),
            (r'\\[\w\W]', String.Escape),
            (r'(\b\w+)(\s*)(=)', bygroups(Name.Variable, Text, Operator)),
            (r'[\[\]{}()=]+', Operator),
            (r'<<\s*(\'?)\\?(\w+)[\w\W]+?\2', String),
            (r';', Punctuation),
        ],
        'data': [
            (r'(?s)"(\\\\|\\[0-7]+|\\.|[^"\\])*"', String.Double),
            (r"(?s)'(\\\\|\\[0-7]+|\\.|[^'\\])*'", String.Single),
            (r'\s+', Text),
            (r'[^=\s\[\]{}()$"\'`\\;#]+', Text),
            (r'\d+(?= |\Z)', Number),
        ],
        'curly': [
            (r'}', Keyword, '#pop'),
            (r':-', Keyword),
            (r'\w+', Name.Variable),
            (r'[^}:"\'`$]+', Punctuation),
            (r':', Punctuation),
            include('root'),
        ],
        'paren': [
            (r'\)', Keyword, '#pop'),
            include('root'),
        ],
    }


class RedLexer(RegexLexer):
    """
    A `Red-language <http://www.red-lang.org/>`_ lexer.

    .. versionadded:: 2.0
    """
    name = 'Red'
    aliases = ['red', 'red/system']
    filenames = ['*.red', '*.reds']
    mimetypes = ['text/x-red', 'text/x-red-system']

    flags = re.IGNORECASE | re.MULTILINE

    escape_re = r'(?:\^\([0-9a-f]{1,4}\)*)'

    def word_callback(lexer, match):
        word = match.group()

        if re.match(".*:$", word):
            yield match.start(), Generic.Subheading, word
        elif re.match(
            r'(if|unless|either|any|all|while|until|loop|repeat|'
            r'foreach|forall|func|function|does|has|switch|'
            r'case|reduce|compose|get|set|print|prin|equal\?|'
            r'not-equal\?|strict-equal\?|lesser\?|greater\?|lesser-or-equal\?|'
            r'greater-or-equal\?|same\?|not|type\?|stats|'
            r'bind|union|replace|charset|routine)$', word):
            yield match.start(), Name.Builtin, word
        elif re.match(
            r'(make|random|reflect|to|form|mold|absolute|add|divide|multiply|negate|'
            r'power|remainder|round|subtract|even\?|odd\?|and~|complement|or~|xor~|'
            r'append|at|back|change|clear|copy|find|head|head\?|index\?|insert|'
            r'length\?|next|pick|poke|remove|reverse|select|sort|skip|swap|tail|tail\?|'
            r'take|trim|create|close|delete|modify|open|open\?|query|read|rename|'
            r'update|write)$', word):
            yield match.start(), Name.Function, word
        elif re.match(
            r'(yes|on|no|off|true|false|tab|cr|lf|newline|escape|slash|sp|space|null|'
            r'none|crlf|dot|null-byte)$', word):
            yield match.start(), Name.Builtin.Pseudo, word
        elif re.match(
            r'(#system-global|#include|#enum|#define|#either|#if|#import|#export|'
            r'#switch|#default|#get-definition)$', word):
            yield match.start(), Keyword.Namespace, word
        elif re.match(
            r'(system|halt|quit|quit-return|do|load|q|recycle|call|run|ask|parse|'
            r'raise-error|return|exit|break|alias|push|pop|probe|\?\?|spec-of|body-of|'
            r'quote|forever)$', word):
            yield match.start(), Name.Exception, word
        elif re.match(
            r'(action\?|block\?|char\?|datatype\?|file\?|function\?|get-path\?|zero\?|'
            r'get-word\?|integer\?|issue\?|lit-path\?|lit-word\?|logic\?|native\?|'
            r'op\?|paren\?|path\?|refinement\?|set-path\?|set-word\?|string\?|unset\?|'
            r'any-struct\?|none\?|word\?|any-series\?)$', word):
            yield match.start(), Keyword, word
        elif re.match(r'(JNICALL|stdcall|cdecl|infix)$', word):
            yield match.start(), Keyword.Namespace, word
        elif re.match("to-.*", word):
            yield match.start(), Keyword, word
        elif re.match('(\+|-|\*|/|//|\*\*|and|or|xor|=\?|=|==|===|<>|<|>|<=|>=|<<|>>|<<<|>>>|%|-\*\*)$', word):
            yield match.start(), Operator, word
        elif re.match(".*\!$", word):
            yield match.start(), Keyword.Type, word
        elif re.match("'.*", word):
            yield match.start(), Name.Variable.Instance, word # lit-word
        elif re.match("#.*", word):
            yield match.start(), Name.Label, word # issue
        elif re.match("%.*", word):
            yield match.start(), Name.Decorator, word # file
        elif re.match(":.*", word):
            yield match.start(), Generic.Subheading, word # get-word
        else:
            yield match.start(), Name.Variable, word

    tokens = {
        'root': [
            (r'[^R]+', Comment),
            (r'Red/System\s+\[', Generic.Strong, 'script'),
            (r'Red\s+\[', Generic.Strong, 'script'),
            (r'R', Comment)
        ],
        'script': [
            (r'\s+', Text),
            (r'#"', String.Char, 'char'),
            (r'#{[0-9a-f\s]*}', Number.Hex),
            (r'2#{', Number.Hex, 'bin2'),
            (r'64#{[0-9a-z+/=\s]*}', Number.Hex),
            (r'([0-9a-f]+)(h)((\s)|(?=[\[\]{}""\(\)]))',
             bygroups(Number.Hex, Name.Variable, Whitespace)),
            (r'"', String, 'string'),
            (r'{', String, 'string2'),
            (r';#+.*\n', Comment.Special),
            (r';\*+.*\n', Comment.Preproc),
            (r';.*\n', Comment),
            (r'%"', Name.Decorator, 'stringFile'),
            (r'%[^(\^{^")\s\[\]]+', Name.Decorator),
            (r'[+-]?([a-z]{1,3})?\$\d+(\.\d+)?', Number.Float), # money
            (r'[+-]?\d+\:\d+(\:\d+)?(\.\d+)?', String.Other), # time
            (r'\d+[\-\/][0-9a-z]+[\-\/]\d+(\/\d+\:\d+((\:\d+)?'
             r'([\.\d+]?([+-]?\d+:\d+)?)?)?)?', String.Other), # date
            (r'\d+(\.\d+)+\.\d+', Keyword.Constant), # tuple
            (r'\d+[xX]\d+', Keyword.Constant), # pair
            (r'[+-]?\d+(\'\d+)?([\.,]\d*)?[eE][+-]?\d+', Number.Float),
            (r'[+-]?\d+(\'\d+)?[\.,]\d*', Number.Float),
            (r'[+-]?\d+(\'\d+)?', Number),
            (r'[\[\]\(\)]', Generic.Strong),
            (r'[a-z]+[^(\^{"\s:)]*://[^(\^{"\s)]*', Name.Decorator), # url
            (r'mailto:[^(\^{"@\s)]+@[^(\^{"@\s)]+', Name.Decorator), # url
            (r'[^(\^{"@\s)]+@[^(\^{"@\s)]+', Name.Decorator), # email
            (r'comment\s"', Comment, 'commentString1'),
            (r'comment\s{', Comment, 'commentString2'),
            (r'comment\s\[', Comment, 'commentBlock'),
            (r'comment\s[^(\s{\"\[]+', Comment),
            (r'/[^(\^{^")\s/[\]]*', Name.Attribute),
            (r'([^(\^{^")\s/[\]]+)(?=[:({"\s/\[\]])', word_callback),
            (r'<[\w:.-]*>', Name.Tag),
            (r'<[^(<>\s")]+', Name.Tag, 'tag'),
            (r'([^(\^{^")\s]+)', Text),
        ],
        'string': [
            (r'[^(\^")]+', String),
            (escape_re, String.Escape),
            (r'[\(|\)]+', String),
            (r'\^.', String.Escape),
            (r'"', String, '#pop'),
        ],
        'string2': [
            (r'[^(\^{^})]+', String),
            (escape_re, String.Escape),
            (r'[\(|\)]+', String),
            (r'\^.', String.Escape),
            (r'{', String, '#push'),
            (r'}', String, '#pop'),
        ],
        'stringFile': [
            (r'[^(\^")]+', Name.Decorator),
            (escape_re, Name.Decorator),
            (r'\^.', Name.Decorator),
            (r'"', Name.Decorator, '#pop'),
        ],
        'char': [
            (escape_re + '"', String.Char, '#pop'),
            (r'\^."', String.Char, '#pop'),
            (r'."', String.Char, '#pop'),
        ],
        'tag': [
            (escape_re, Name.Tag),
            (r'"', Name.Tag, 'tagString'),
            (r'[^(<>\r\n")]+', Name.Tag),
            (r'>', Name.Tag, '#pop'),
        ],
        'tagString': [
            (r'[^(\^")]+', Name.Tag),
            (escape_re, Name.Tag),
            (r'[\(|\)]+', Name.Tag),
            (r'\^.', Name.Tag),
            (r'"', Name.Tag, '#pop'),
        ],
        'tuple': [
            (r'(\d+\.)+', Keyword.Constant),
            (r'\d+', Keyword.Constant, '#pop'),
        ],
        'bin2': [
            (r'\s+', Number.Hex),
            (r'([0-1]\s*){8}', Number.Hex),
            (r'}', Number.Hex, '#pop'),
        ],
        'commentString1': [
            (r'[^(\^")]+', Comment),
            (escape_re, Comment),
            (r'[\(|\)]+', Comment),
            (r'\^.', Comment),
            (r'"', Comment, '#pop'),
        ],
        'commentString2': [
            (r'[^(\^{^})]+', Comment),
            (escape_re, Comment),
            (r'[\(|\)]+', Comment),
            (r'\^.', Comment),
            (r'{', Comment, '#push'),
            (r'}', Comment, '#pop'),
        ],
        'commentBlock': [
            (r'\[', Comment, '#push'),
            (r'\]', Comment, '#pop'),
            (r'"', Comment, "commentString1"),
            (r'{', Comment, "commentString2"),
            (r'[^(\[\]\"{)]+', Comment),
        ],
    }


class AlloyLexer(RegexLexer):
    """
    For `Alloy <http://alloy.mit.edu>`_ source code.
    """

    name = 'Alloy'
    aliases = ['alloy']
    filenames = ['*.als']
    mimetypes = ['text/x-alloy']

    flags = re.MULTILINE | re.DOTALL

    iden_rex = r'[a-zA-Z_][a-zA-Z0-9_\']*'
    text_tuple = (r'[^\S\n]+', Text)

    tokens = {
        'sig': [
            (r'(extends)\b', Keyword, '#pop'),
            (iden_rex, Name),
            text_tuple,
            (r',', Punctuation),
            (r'\{', Operator, '#pop'),
        ],
        'module': [
            text_tuple,
            (iden_rex, Name, '#pop'),
        ],
        'fun': [
            text_tuple,
            (r'\{', Operator, '#pop'),
            (iden_rex, Name, '#pop'),
        ],
        'root': [
            (r'--.*?$', Comment.Single),
            (r'//.*?$', Comment.Single),
            (r'/\*.*?\*/', Comment.Multiline),
            text_tuple,
            (r'(module|open)(\s+)', bygroups(Keyword.Namespace, Text),
                'module'),
            (r'(sig|enum)(\s+)', bygroups(Keyword.Declaration, Text), 'sig'),
            (r'(iden|univ|none)\b', Keyword.Constant),
            (r'(int|Int)\b', Keyword.Type),
            (r'(this|abstract|extends|set|seq|one|lone|let)\b', Keyword),
            (r'(all|some|no|sum|disj|when|else)\b', Keyword),
            (r'(run|check|for|but|exactly|expect|as)\b', Keyword),
            (r'(and|or|implies|iff|in)\b', Operator.Word),
            (r'(fun|pred|fact|assert)(\s+)', bygroups(Keyword, Text), 'fun'),
            (r'!|#|&&|\+\+|<<|>>|>=|<=>|<=|\.|->', Operator),
            (r'[-+/*%=<>&!^|~\{\}\[\]\(\)\.]', Operator),
            (iden_rex, Name),
            (r'[:,]', Punctuation),
            (r'[0-9]+', Number.Integer),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r'\n', Text),
        ]
    }
