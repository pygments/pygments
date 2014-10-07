# -*- coding: utf-8 -*-
"""
    pygments.lexers.c_like
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for other C-like languages.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, inherit, words
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation

from pygments.lexers.c_cpp import CLexer, CppLexer

__all__ = ['PikeLexer', 'NesCLexer', 'ClayLexer', 'ECLexer', 'ValaLexer',
           'CudaLexer', 'SwigLexer', 'MqlLexer']


class PikeLexer(CppLexer):
    """
    For `Pike <http://pike.lysator.liu.se/>`_ source code.

    .. versionadded:: 2.0
    """
    name = 'Pike'
    aliases = ['pike']
    filenames = ['*.pike', '*.pmod']
    mimetypes = ['text/x-pike']

    tokens = {
        'statements': [
            (words((
                'catch', 'new', 'private', 'protected', 'public', 'gauge',
                'throw', 'throws', 'class', 'interface', 'implement', 'abstract', 'extends', 'from',
                'this', 'super', 'new', 'constant', 'final', 'static', 'import', 'use', 'extern',
                'inline', 'proto', 'break', 'continue', 'if', 'else', 'for',
                'while', 'do', 'switch', 'case', 'as', 'in', 'version', 'return', 'true', 'false', 'null',
                '__VERSION__', '__MAJOR__', '__MINOR__', '__BUILD__', '__REAL_VERSION__',
                '__REAL_MAJOR__', '__REAL_MINOR__', '__REAL_BUILD__', '__DATE__', '__TIME__',
                '__FILE__', '__DIR__', '__LINE__', '__AUTO_BIGNUM__', '__NT__', '__PIKE__',
                '__amigaos__', '_Pragma', 'static_assert', 'defined', 'sscanf'), suffix=r'\b'),
             Keyword),
            (r'(bool|int|long|float|short|double|char|string|object|void|mapping|'
             r'array|multiset|program|function|lambda|mixed|'
             r'[a-z_][a-z0-9_]*_t)\b',
             Keyword.Type),
            (r'(class)(\s+)', bygroups(Keyword, Text), 'classname'),
            (r'[~!%^&*+=|?:<>/-@]', Operator),
            inherit,
        ],
        'classname': [
            (r'[a-zA-Z_]\w*', Name.Class, '#pop'),
            # template specification
            (r'\s*(?=>)', Text, '#pop'),
        ],
    }


class NesCLexer(CLexer):
    """
    For `nesC <https://github.com/tinyos/nesc>`_ source code with preprocessor
    directives.

    .. versionadded:: 2.0
    """
    name = 'nesC'
    aliases = ['nesc']
    filenames = ['*.nc']
    mimetypes = ['text/x-nescsrc']

    tokens = {
        'statements': [
            (words((
                'abstract', 'as', 'async', 'atomic', 'call', 'command', 'component',
                'components', 'configuration', 'event', 'extends', 'generic',
                'implementation', 'includes', 'interface', 'module', 'new', 'norace',
                'post', 'provides', 'signal', 'task', 'uses'), suffix=r'\b'),
             Keyword),
            (words(('nx_struct', 'nx_union', 'nx_int8_t', 'nx_int16_t', 'nx_int32_t',
                    'nx_int64_t', 'nx_uint8_t', 'nx_uint16_t', 'nx_uint32_t',
                    'nx_uint64_t'), suffix=r'\b'),
             Keyword.Type),
            inherit,
        ],
    }


class ClayLexer(RegexLexer):
    """
    For `Clay <http://claylabs.com/clay/>`_ source.

    .. versionadded:: 2.0
    """
    name = 'Clay'
    filenames = ['*.clay']
    aliases = ['clay']
    mimetypes = ['text/x-clay']
    tokens = {
        'root': [
            (r'\s', Text),
            (r'//.*?$', Comment.Singleline),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
            (r'\b(public|private|import|as|record|variant|instance'
             r'|define|overload|default|external|alias'
             r'|rvalue|ref|forward|inline|noinline|forceinline'
             r'|enum|var|and|or|not|if|else|goto|return|while'
             r'|switch|case|break|continue|for|in|true|false|try|catch|throw'
             r'|finally|onerror|staticassert|eval|when|newtype'
             r'|__FILE__|__LINE__|__COLUMN__|__ARG__'
             r')\b', Keyword),
            (r'[~!%^&*+=|:<>/-]', Operator),
            (r'[#(){}\[\],;.]', Punctuation),
            (r'0x[0-9a-fA-F]+[LlUu]*', Number.Hex),
            (r'\d+[LlUu]*', Number.Integer),
            (r'\b(true|false)\b', Name.Builtin),
            (r'(?i)[a-z_?][a-z_?0-9]*', Name),
            (r'"""', String, 'tdqs'),
            (r'"', String, 'dqs'),
        ],
        'strings': [
            (r'(?i)\\(x[0-9a-f]{2}|.)', String.Escape),
            (r'.', String),
        ],
        'nl': [
            (r'\n', String),
        ],
        'dqs': [
            (r'"', String, '#pop'),
            include('strings'),
        ],
        'tdqs': [
            (r'"""', String, '#pop'),
            include('strings'),
            include('nl'),
        ],
    }


class ECLexer(CLexer):
    """
    For eC source code with preprocessor directives.

    .. versionadded:: 1.5
    """
    name = 'eC'
    aliases = ['ec']
    filenames = ['*.ec', '*.eh']
    mimetypes = ['text/x-echdr', 'text/x-ecsrc']

    tokens = {
        'statements': [
            (words((
                'virtual', 'class', 'private', 'public', 'property', 'import',
                'delete', 'new', 'new0', 'renew', 'renew0', 'define', 'get',
                'set', 'remote', 'dllexport', 'dllimport', 'stdcall', 'subclass',
                '__on_register_module', 'namespace', 'using', 'typed_object',
                'any_object', 'incref', 'register', 'watch', 'stopwatching', 'firewatchers',
                'watchable', 'class_designer', 'class_fixed', 'class_no_expansion', 'isset',
                'class_default_property', 'property_category', 'class_data',
                'class_property', 'virtual', 'thisclass', 'dbtable', 'dbindex',
                'database_open', 'dbfield'), suffix=r'\b'), Keyword),
            (words(('uint', 'uint16', 'uint32', 'uint64', 'bool', 'byte',
                    'unichar', 'int64'), suffix=r'\b'),
             Keyword.Type),
            (r'(class)(\s+)', bygroups(Keyword, Text), 'classname'),
            (r'(null|value|this)\b', Name.Builtin),
            inherit,
        ],
        'classname': [
            (r'[a-zA-Z_]\w*', Name.Class, '#pop'),
            # template specification
            (r'\s*(?=>)', Text, '#pop'),
        ],
    }


class ValaLexer(RegexLexer):
    """
    For Vala source code with preprocessor directives.

    .. versionadded:: 1.1
    """
    name = 'Vala'
    aliases = ['vala', 'vapi']
    filenames = ['*.vala', '*.vapi']
    mimetypes = ['text/x-vala']

    tokens = {
        'whitespace': [
            (r'^\s*#if\s+0', Comment.Preproc, 'if0'),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text),  # line continuation
            (r'//(\n|(.|\n)*?[^\\]\n)', Comment.Single),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment.Multiline),
        ],
        'statements': [
            (r'[L@]?"', String, 'string'),
            (r"L?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'",
             String.Char),
            (r'(?s)""".*?"""', String),  # verbatim strings
            (r'(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[lL]?', Number.Float),
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'0x[0-9a-fA-F]+[Ll]?', Number.Hex),
            (r'0[0-7]+[Ll]?', Number.Oct),
            (r'\d+[Ll]?', Number.Integer),
            (r'[~!%^&*+=|?:<>/-]', Operator),
            (r'(\[)(Compact|Immutable|(?:Boolean|Simple)Type)(\])',
             bygroups(Punctuation, Name.Decorator, Punctuation)),
            # TODO: "correctly" parse complex code attributes
            (r'(\[)(CCode|(?:Integer|Floating)Type)',
             bygroups(Punctuation, Name.Decorator)),
            (r'[()\[\],.]', Punctuation),
            (words((
                'as', 'base', 'break', 'case', 'catch', 'construct', 'continue',
                'default', 'delete', 'do', 'else', 'enum', 'finally', 'for',
                'foreach', 'get', 'if', 'in', 'is', 'lock', 'new', 'out', 'params',
                'return', 'set', 'sizeof', 'switch', 'this', 'throw', 'try',
                'typeof', 'while', 'yield'), suffix=r'\b'),
             Keyword),
            (words((
                'abstract', 'const', 'delegate', 'dynamic', 'ensures', 'extern',
                'inline', 'internal', 'override', 'owned', 'private', 'protected',
                'public', 'ref', 'requires', 'signal', 'static', 'throws', 'unowned',
                'var', 'virtual', 'volatile', 'weak', 'yields'), suffix=r'\b'),
             Keyword.Declaration),
            (r'(namespace|using)(\s+)', bygroups(Keyword.Namespace, Text),
             'namespace'),
            (r'(class|errordomain|interface|struct)(\s+)',
             bygroups(Keyword.Declaration, Text), 'class'),
            (r'(\.)([a-zA-Z_]\w*)',
             bygroups(Operator, Name.Attribute)),
            # void is an actual keyword, others are in glib-2.0.vapi
            (words((
                'void', 'bool', 'char', 'double', 'float', 'int', 'int8', 'int16',
                'int32', 'int64', 'long', 'short', 'size_t', 'ssize_t', 'string',
                'time_t', 'uchar', 'uint', 'uint8', 'uint16', 'uint32', 'uint64',
                'ulong', 'unichar', 'ushort'), suffix=r'\b'),
             Keyword.Type),
            (r'(true|false|null)\b', Name.Builtin),
            ('[a-zA-Z_]\w*', Name),
        ],
        'root': [
            include('whitespace'),
            ('', Text, 'statement'),
        ],
        'statement': [
            include('whitespace'),
            include('statements'),
            ('[{}]', Punctuation),
            (';', Punctuation, '#pop'),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})', String.Escape),
            (r'[^\\"\n]+', String),  # all other characters
            (r'\\\n', String),  # line continuation
            (r'\\', String),  # stray backslash
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment.Preproc, '#push'),
            (r'^\s*#el(?:se|if).*\n', Comment.Preproc, '#pop'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment.Preproc, '#pop'),
            (r'.*?\n', Comment),
        ],
        'class': [
            (r'[a-zA-Z_]\w*', Name.Class, '#pop')
        ],
        'namespace': [
            (r'[a-zA-Z_][\w.]*', Name.Namespace, '#pop')
        ],
    }


class CudaLexer(CLexer):
    """
    For NVIDIA `CUDA™ <http://developer.nvidia.com/category/zone/cuda-zone>`_
    source.

    .. versionadded:: 1.6
    """
    name = 'CUDA'
    filenames = ['*.cu', '*.cuh']
    aliases = ['cuda', 'cu']
    mimetypes = ['text/x-cuda']

    function_qualifiers = set(('__device__', '__global__', '__host__',
                               '__noinline__', '__forceinline__'))
    variable_qualifiers = set(('__device__', '__constant__', '__shared__',
                               '__restrict__'))
    vector_types = set(('char1', 'uchar1', 'char2', 'uchar2', 'char3', 'uchar3',
                        'char4', 'uchar4', 'short1', 'ushort1', 'short2', 'ushort2',
                        'short3', 'ushort3', 'short4', 'ushort4', 'int1', 'uint1',
                        'int2', 'uint2', 'int3', 'uint3', 'int4', 'uint4', 'long1',
                        'ulong1', 'long2', 'ulong2', 'long3', 'ulong3', 'long4',
                        'ulong4', 'longlong1', 'ulonglong1', 'longlong2',
                        'ulonglong2', 'float1', 'float2', 'float3', 'float4',
                        'double1', 'double2', 'dim3'))
    variables = set(('gridDim', 'blockIdx', 'blockDim', 'threadIdx', 'warpSize'))
    functions = set(('__threadfence_block', '__threadfence', '__threadfence_system',
                     '__syncthreads', '__syncthreads_count', '__syncthreads_and',
                     '__syncthreads_or'))
    execution_confs = set(('<<<', '>>>'))

    def get_tokens_unprocessed(self, text):
        for index, token, value in CLexer.get_tokens_unprocessed(self, text):
            if token is Name:
                if value in self.variable_qualifiers:
                    token = Keyword.Type
                elif value in self.vector_types:
                    token = Keyword.Type
                elif value in self.variables:
                    token = Name.Builtin
                elif value in self.execution_confs:
                    token = Keyword.Pseudo
                elif value in self.function_qualifiers:
                    token = Keyword.Reserved
                elif value in self.functions:
                    token = Name.Function
            yield index, token, value


class SwigLexer(CppLexer):
    """
    For `SWIG <http://www.swig.org/>`_ source code.

    .. versionadded:: 2.0
    """
    name = 'SWIG'
    aliases = ['swig']
    filenames = ['*.swg', '*.i']
    mimetypes = ['text/swig']
    priority = 0.04  # Lower than C/C++ and Objective C/C++

    tokens = {
        'statements': [
            # SWIG directives
            (r'(%[a-z_][a-z0-9_]*)', Name.Function),
            # Special variables
            ('\$\**\&?\w+', Name),
            # Stringification / additional preprocessor directives
            (r'##*[a-zA-Z_]\w*', Comment.Preproc),
            inherit,
        ],
    }

    # This is a far from complete set of SWIG directives
    swig_directives = set((
        # Most common directives
        '%apply', '%define', '%director', '%enddef', '%exception', '%extend',
        '%feature', '%fragment', '%ignore', '%immutable', '%import', '%include',
        '%inline', '%insert', '%module', '%newobject', '%nspace', '%pragma',
        '%rename', '%shared_ptr', '%template', '%typecheck', '%typemap',
        # Less common directives
        '%arg', '%attribute', '%bang', '%begin', '%callback', '%catches', '%clear',
        '%constant', '%copyctor', '%csconst', '%csconstvalue', '%csenum',
        '%csmethodmodifiers', '%csnothrowexception', '%default', '%defaultctor',
        '%defaultdtor', '%defined', '%delete', '%delobject', '%descriptor',
        '%exceptionclass', '%exceptionvar', '%extend_smart_pointer', '%fragments',
        '%header', '%ifcplusplus', '%ignorewarn', '%implicit', '%implicitconv',
        '%init', '%javaconst', '%javaconstvalue', '%javaenum', '%javaexception',
        '%javamethodmodifiers', '%kwargs', '%luacode', '%mutable', '%naturalvar',
        '%nestedworkaround', '%perlcode', '%pythonabc', '%pythonappend',
        '%pythoncallback', '%pythoncode', '%pythondynamic', '%pythonmaybecall',
        '%pythonnondynamic', '%pythonprepend', '%refobject', '%shadow', '%sizeof',
        '%trackobjects', '%types', '%unrefobject', '%varargs', '%warn',
        '%warnfilter'))

    def analyse_text(text):
        rv = 0
        # Search for SWIG directives, which are conventionally at the beginning of
        # a line. The probability of them being within a line is low, so let another
        # lexer win in this case.
        matches = re.findall(r'^\s*(%[a-z_][a-z0-9_]*)', text, re.M)
        for m in matches:
            if m in SwigLexer.swig_directives:
                rv = 0.98
                break
            else:
                rv = 0.91  # Fraction higher than MatlabLexer
        return rv


class MqlLexer(CppLexer):
    """
    For `MQL4 <http://docs.mql4.com/>`_ and
    `MQL5 <http://www.mql5.com/en/docs>`_ source code.

    .. versionadded:: 2.0
    """
    name = 'MQL'
    aliases = ['mql', 'mq4', 'mq5', 'mql4', 'mql5']
    filenames = ['*.mq4', '*.mq5', '*.mqh']
    mimetypes = ['text/x-mql']

    tokens = {
        'statements': [
            (words((
                'input', '_Digits', '_Point', '_LastError', '_Period', '_RandomSeed',
                '_StopFlag', '_Symbol', '_UninitReason', 'Ask', 'Bars', 'Bid',
                'Close', 'Digits', 'High', 'Low', 'Open', 'Point', 'Time',
                'Volume'), suffix=r'\b'),
             Keyword),
            (words((
                'void', 'char', 'uchar', 'bool', 'short', 'ushort', 'int', 'uint',
                'color', 'long', 'ulong', 'datetime', 'float', 'double',
                'string'), suffix=r'\b'),
             Keyword.Type),
            (words((
                'Alert', 'CheckPointer', 'Comment', 'DebugBreak', 'ExpertRemove',
                'GetPointer', 'GetTickCount', 'MessageBox', 'PeriodSeconds', 'PlaySound',
                'Print', 'PrintFormat', 'ResetLastError', 'ResourceCreate', 'ResourceFree',
                'ResourceReadImage', 'ResourceSave', 'SendFTP', 'SendMail', 'SendNotification',
                'Sleep', 'TerminalClose', 'TesterStatistics', 'ZeroMemory',
                'ArrayBsearch', 'ArrayCopy', 'ArrayCompare', 'ArrayFree', 'ArrayGetAsSeries',
                'ArrayInitialize', 'ArrayFill', 'ArrayIsSeries', 'ArrayIsDynamic',
                'ArrayMaximum', 'ArrayMinimum', 'ArrayRange', 'ArrayResize',
                'ArraySetAsSeries', 'ArraySize', 'ArraySort', 'ArrayCopyRates',
                'ArrayCopySeries', 'ArrayDimension',
                'CharToString', 'DoubleToString', 'EnumToString', 'NormalizeDouble',
                'StringToDouble', 'StringToInteger', 'StringToTime', 'TimeToString',
                'IntegerToString', 'ShortToString', 'ShortArrayToString',
                'StringToShortArray', 'CharArrayToString', 'StringToCharArray',
                'ColorToARGB', 'ColorToString', 'StringToColor', 'StringFormat',
                'CharToStr', 'DoubleToStr', 'StrToDouble', 'StrToInteger', 'StrToTime', 'TimeToStr',
                'MathAbs', 'MathArccos', 'MathArcsin', 'MathArctan', 'MathCeil', 'MathCos', 'MathExp',
                'MathFloor', 'MathLog', 'MathMax', 'MathMin', 'MathMod', 'MathPow', 'MathRand',
                'MathRound', 'MathSin', 'MathSqrt', 'MathSrand', 'MathTan', 'MathIsValidNumber',
                'StringAdd', 'StringBufferLen', 'StringCompare', 'StringConcatenate', 'StringFill',
                'StringFind', 'StringGetCharacter', 'StringInit', 'StringLen', 'StringReplace',
                'StringSetCharacter', 'StringSplit', 'StringSubstr', 'StringToLower', 'StringToUpper',
                'StringTrimLeft', 'StringTrimRight', 'StringGetChar', 'StringSetChar',
                'TimeCurrent', 'TimeTradeServer', 'TimeLocal', 'TimeGMT', 'TimeDaylightSavings',
                'TimeGMTOffset', 'TimeToStruct', 'StructToTime', 'Day', 'DayOfWeek', 'DayOfYear',
                'Hour', 'Minute', 'Month', 'Seconds', 'TimeDay', 'TimeDayOfWeek', 'TimeDayOfYear',
                'TimeHour', 'TimeMinute', 'TimeMonth', 'TimeSeconds', 'TimeYear', 'Year',
                'AccountInfoDouble', 'AccountInfoInteger', 'AccountInfoString', 'AccountBalance',
                'AccountCredit', 'AccountCompany', 'AccountCurrency', 'AccountEquity',
                'AccountFreeMargin', 'AccountFreeMarginCheck', 'AccountFreeMarginMode',
                'AccountLeverage', 'AccountMargin', 'AccountName', 'AccountNumber', 'AccountProfit',
                'AccountServer', 'AccountStopoutLevel', 'AccountStopoutMode',
                'GetLastError', 'IsStopped', 'UninitializeReason', 'MQLInfoInteger', 'MQLInfoString',
                'Symbol', 'Period', 'Digits', 'Point', 'IsConnected', 'IsDemo', 'IsDllsAllowed',
                'IsExpertEnabled', 'IsLibrariesAllowed', 'IsOptimization', 'IsTesting',
                'IsTradeAllowed',
                'IsTradeContextBusy', 'IsVisualMode', 'TerminalCompany', 'TerminalName',
                'TerminalPath',
                'SymbolsTotal', 'SymbolName', 'SymbolSelect', 'SymbolIsSynchronized',
                'SymbolInfoDouble',
                'SymbolInfoInteger', 'SymbolInfoString', 'SymbolInfoTick',
                'SymbolInfoSessionQuote',
                'SymbolInfoSessionTrade', 'MarketInfo',
                'SeriesInfoInteger', 'CopyRates', 'CopyTime', 'CopyOpen',
                'CopyHigh', 'CopyLow', 'CopyClose',
                'CopyTickVolume', 'CopyRealVolume', 'CopySpread', 'iBars', 'iBarShift', 'iClose',
                'iHigh', 'iHighest', 'iLow', 'iLowest', 'iOpen', 'iTime', 'iVolume',
                'HideTestIndicators', 'Period', 'RefreshRates', 'Symbol', 'WindowBarsPerChart',
                'WindowExpertName', 'WindowFind', 'WindowFirstVisibleBar', 'WindowHandle',
                'WindowIsVisible', 'WindowOnDropped', 'WindowPriceMax', 'WindowPriceMin',
                'WindowPriceOnDropped', 'WindowRedraw', 'WindowScreenShot',
                'WindowTimeOnDropped', 'WindowsTotal', 'WindowXOnDropped', 'WindowYOnDropped',
                'OrderClose', 'OrderCloseBy', 'OrderClosePrice', 'OrderCloseTime', 'OrderComment',
                'OrderCommission', 'OrderDelete', 'OrderExpiration', 'OrderLots', 'OrderMagicNumber',
                'OrderModify', 'OrderOpenPrice', 'OrderOpenTime', 'OrderPrint', 'OrderProfit',
                'OrderSelect', 'OrderSend', 'OrdersHistoryTotal', 'OrderStopLoss', 'OrdersTotal',
                'OrderSwap', 'OrderSymbol', 'OrderTakeProfit', 'OrderTicket', 'OrderType',
                'GlobalVariableCheck', 'GlobalVariableTime',
                'GlobalVariableDel', 'GlobalVariableGet', 'GlobalVariableName',
                'GlobalVariableSet', 'GlobalVariablesFlush', 'GlobalVariableTemp',
                'GlobalVariableSetOnCondition', 'GlobalVariablesDeleteAll',
                'GlobalVariablesTotal', 'GlobalVariableCheck', 'GlobalVariableTime',
                'GlobalVariableDel', 'GlobalVariableGet',
                'GlobalVariableName', 'GlobalVariableSet', 'GlobalVariablesFlush',
                'GlobalVariableTemp', 'GlobalVariableSetOnCondition',
                'GlobalVariablesDeleteAll', 'GlobalVariablesTotal',
                'GlobalVariableCheck', 'GlobalVariableTime', 'GlobalVariableDel',
                'GlobalVariableGet', 'GlobalVariableName', 'GlobalVariableSet',
                'GlobalVariablesFlush', 'GlobalVariableTemp',
                'GlobalVariableSetOnCondition', 'GlobalVariablesDeleteAll',
                'GlobalVariablesTotal',
                'FileFindFirst', 'FileFindNext', 'FileFindClose', 'FileOpen', 'FileDelete',
                'FileFlush', 'FileGetInteger', 'FileIsEnding', 'FileIsLineEnding',
                'FileClose', 'FileIsExist', 'FileCopy', 'FileMove', 'FileReadArray',
                'FileReadBool', 'FileReadDatetime', 'FileReadDouble', 'FileReadFloat',
                'FileReadInteger', 'FileReadLong', 'FileReadNumber', 'FileReadString',
                'FileReadStruct', 'FileSeek', 'FileSize', 'FileTell', 'FileWrite',
                'FileWriteArray', 'FileWriteDouble', 'FileWriteFloat', 'FileWriteInteger',
                'FileWriteLong', 'FileWriteString', 'FileWriteStruct', 'FolderCreate',
                'FolderDelete', 'FolderClean', 'FileOpenHistory',
                'IndicatorSetDouble', 'IndicatorSetInteger', 'IndicatorSetString',
                'SetIndexBuffer', 'IndicatorBuffers', 'IndicatorCounted', 'IndicatorDigits',
                'IndicatorShortName', 'SetIndexArrow', 'SetIndexDrawBegin',
                'SetIndexEmptyValue', 'SetIndexLabel', 'SetIndexShift',
                'SetIndexStyle', 'SetLevelStyle', 'SetLevelValue',
                'ObjectCreate', 'ObjectName', 'ObjectDelete', 'ObjectsDeleteAll',
                'ObjectFind', 'ObjectGetTimeByValue', 'ObjectGetValueByTime',
                'ObjectMove', 'ObjectsTotal', 'ObjectGetDouble', 'ObjectGetInteger',
                'ObjectGetString', 'ObjectSetDouble', 'ObjectSetInteger',
                'ObjectSetString', 'TextSetFont', 'TextOut', 'TextGetSize',
                'ObjectDescription', 'ObjectGet', 'ObjectGetFiboDescription',
                'ObjectGetShiftByValue', 'ObjectGetValueByShift', 'ObjectSet',
                'ObjectSetFiboDescription', 'ObjectSetText', 'ObjectType',
                'iAC', 'iAD', 'iADX', 'iAlligator', 'iAO', 'iATR', 'iBearsPower',
                'iBands', 'iBandsOnArray', 'iBullsPower', 'iCCI', 'iCCIOnArray',
                'iCustom', 'iDeMarker', 'iEnvelopes', 'iEnvelopesOnArray',
                'iForce', 'iFractals', 'iGator', 'iIchimoku', 'iBWMFI', 'iMomentum',
                'iMomentumOnArray', 'iMFI', 'iMA', 'iMAOnArray', 'iOsMA', 'iMACD',
                'iOBV', 'iSAR', 'iRSI', 'iRSIOnArray', 'iRVI', 'iStdDev', 'iStdDevOnArray',
                'iStochastic', 'iWPR',
                'EventSetMillisecondTimer', 'EventSetTimer',
                'EventKillTimer', 'EventChartCustom'), suffix=r'\b'),
             Name.Function),
            (words((
                'CHARTEVENT_KEYDOWN', 'CHARTEVENT_MOUSE_MOVE',
                'CHARTEVENT_OBJECT_CREATE',
                'CHARTEVENT_OBJECT_CHANGE', 'CHARTEVENT_OBJECT_DELETE',
                'CHARTEVENT_CLICK',
                'CHARTEVENT_OBJECT_CLICK', 'CHARTEVENT_OBJECT_DRAG',
                'CHARTEVENT_OBJECT_ENDEDIT',
                'CHARTEVENT_CHART_CHANGE', 'CHARTEVENT_CUSTOM',
                'CHARTEVENT_CUSTOM_LAST',
                'PERIOD_CURRENT', 'PERIOD_M1', 'PERIOD_M2', 'PERIOD_M3',
                'PERIOD_M4', 'PERIOD_M5',
                'PERIOD_M6', 'PERIOD_M10', 'PERIOD_M12', 'PERIOD_M15',
                'PERIOD_M20', 'PERIOD_M30',
                'PERIOD_H1', 'PERIOD_H2', 'PERIOD_H3', 'PERIOD_H4',
                'PERIOD_H6', 'PERIOD_H8',
                'PERIOD_H12', 'PERIOD_D1', 'PERIOD_W1', 'PERIOD_MN1',
                'CHART_IS_OBJECT', 'CHART_BRING_TO_TOP',
                'CHART_MOUSE_SCROLL', 'CHART_EVENT_MOUSE_MOVE',
                'CHART_EVENT_OBJECT_CREATE',
                'CHART_EVENT_OBJECT_DELETE', 'CHART_MODE', 'CHART_FOREGROUND',
                'CHART_SHIFT',
                'CHART_AUTOSCROLL', 'CHART_SCALE', 'CHART_SCALEFIX',
                'CHART_SCALEFIX_11',
                'CHART_SCALE_PT_PER_BAR', 'CHART_SHOW_OHLC',
                'CHART_SHOW_BID_LINE',
                'CHART_SHOW_ASK_LINE', 'CHART_SHOW_LAST_LINE',
                'CHART_SHOW_PERIOD_SEP',
                'CHART_SHOW_GRID', 'CHART_SHOW_VOLUMES',
                'CHART_SHOW_OBJECT_DESCR',
                'CHART_VISIBLE_BARS', 'CHART_WINDOWS_TOTAL',
                'CHART_WINDOW_IS_VISIBLE',
                'CHART_WINDOW_HANDLE', 'CHART_WINDOW_YDISTANCE',
                'CHART_FIRST_VISIBLE_BAR',
                'CHART_WIDTH_IN_BARS', 'CHART_WIDTH_IN_PIXELS',
                'CHART_HEIGHT_IN_PIXELS',
                'CHART_COLOR_BACKGROUND', 'CHART_COLOR_FOREGROUND',
                'CHART_COLOR_GRID',
                'CHART_COLOR_VOLUME', 'CHART_COLOR_CHART_UP',
                'CHART_COLOR_CHART_DOWN',
                'CHART_COLOR_CHART_LINE', 'CHART_COLOR_CANDLE_BULL',
                'CHART_COLOR_CANDLE_BEAR',
                'CHART_COLOR_BID', 'CHART_COLOR_ASK', 'CHART_COLOR_LAST',
                'CHART_COLOR_STOP_LEVEL',
                'CHART_SHOW_TRADE_LEVELS', 'CHART_DRAG_TRADE_LEVELS',
                'CHART_SHOW_DATE_SCALE',
                'CHART_SHOW_PRICE_SCALE', 'CHART_SHIFT_SIZE',
                'CHART_FIXED_POSITION',
                'CHART_FIXED_MAX', 'CHART_FIXED_MIN', 'CHART_POINTS_PER_BAR',
                'CHART_PRICE_MIN',
                'CHART_PRICE_MAX', 'CHART_COMMENT', 'CHART_BEGIN',
                'CHART_CURRENT_POS', 'CHART_END',
                'CHART_BARS', 'CHART_CANDLES', 'CHART_LINE', 'CHART_VOLUME_HIDE',
                'CHART_VOLUME_TICK', 'CHART_VOLUME_REAL',
                'OBJ_VLINE', 'OBJ_HLINE', 'OBJ_TREND', 'OBJ_TRENDBYANGLE', 'OBJ_CYCLES',
                'OBJ_CHANNEL', 'OBJ_STDDEVCHANNEL', 'OBJ_REGRESSION', 'OBJ_PITCHFORK',
                'OBJ_GANNLINE', 'OBJ_GANNFAN', 'OBJ_GANNGRID', 'OBJ_FIBO',
                'OBJ_FIBOTIMES', 'OBJ_FIBOFAN', 'OBJ_FIBOARC', 'OBJ_FIBOCHANNEL',
                'OBJ_EXPANSION', 'OBJ_RECTANGLE', 'OBJ_TRIANGLE', 'OBJ_ELLIPSE',
                'OBJ_ARROW_THUMB_UP', 'OBJ_ARROW_THUMB_DOWN',
                'OBJ_ARROW_UP', 'OBJ_ARROW_DOWN',
                'OBJ_ARROW_STOP', 'OBJ_ARROW_CHECK', 'OBJ_ARROW_LEFT_PRICE',
                'OBJ_ARROW_RIGHT_PRICE', 'OBJ_ARROW_BUY', 'OBJ_ARROW_SELL',
                'OBJ_ARROW',
                'OBJ_TEXT', 'OBJ_LABEL', 'OBJ_BUTTON', 'OBJ_BITMAP',
                'OBJ_BITMAP_LABEL',
                'OBJ_EDIT', 'OBJ_EVENT', 'OBJ_RECTANGLE_LABEL',
                'OBJPROP_TIME1', 'OBJPROP_PRICE1', 'OBJPROP_TIME2',
                'OBJPROP_PRICE2', 'OBJPROP_TIME3',
                'OBJPROP_PRICE3', 'OBJPROP_COLOR', 'OBJPROP_STYLE',
                'OBJPROP_WIDTH',
                'OBJPROP_BACK', 'OBJPROP_RAY', 'OBJPROP_ELLIPSE',
                'OBJPROP_SCALE',
                'OBJPROP_ANGLE', 'OBJPROP_ARROWCODE', 'OBJPROP_TIMEFRAMES',
                'OBJPROP_DEVIATION', 'OBJPROP_FONTSIZE', 'OBJPROP_CORNER',
                'OBJPROP_XDISTANCE', 'OBJPROP_YDISTANCE', 'OBJPROP_FIBOLEVELS',
                'OBJPROP_LEVELCOLOR', 'OBJPROP_LEVELSTYLE', 'OBJPROP_LEVELWIDTH',
                'OBJPROP_FIRSTLEVEL', 'OBJPROP_COLOR', 'OBJPROP_STYLE', 'OBJPROP_WIDTH',
                'OBJPROP_BACK', 'OBJPROP_ZORDER', 'OBJPROP_FILL', 'OBJPROP_HIDDEN',
                'OBJPROP_SELECTED', 'OBJPROP_READONLY', 'OBJPROP_TYPE', 'OBJPROP_TIME',
                'OBJPROP_SELECTABLE', 'OBJPROP_CREATETIME', 'OBJPROP_LEVELS',
                'OBJPROP_LEVELCOLOR', 'OBJPROP_LEVELSTYLE', 'OBJPROP_LEVELWIDTH',
                'OBJPROP_ALIGN', 'OBJPROP_FONTSIZE', 'OBJPROP_RAY_RIGHT', 'OBJPROP_RAY',
                'OBJPROP_ELLIPSE', 'OBJPROP_ARROWCODE', 'OBJPROP_TIMEFRAMES', 'OBJPROP_ANCHOR',
                'OBJPROP_XDISTANCE', 'OBJPROP_YDISTANCE', 'OBJPROP_DRAWLINES', 'OBJPROP_STATE',
                'OBJPROP_CHART_ID', 'OBJPROP_XSIZE', 'OBJPROP_YSIZE', 'OBJPROP_XOFFSET',
                'OBJPROP_YOFFSET', 'OBJPROP_PERIOD', 'OBJPROP_DATE_SCALE', 'OBJPROP_PRICE_SCALE',
                'OBJPROP_CHART_SCALE', 'OBJPROP_BGCOLOR', 'OBJPROP_CORNER', 'OBJPROP_BORDER_TYPE',
                'OBJPROP_BORDER_COLOR', 'OBJPROP_PRICE', 'OBJPROP_LEVELVALUE', 'OBJPROP_SCALE',
                'OBJPROP_ANGLE', 'OBJPROP_DEVIATION',
                'OBJPROP_NAME', 'OBJPROP_TEXT', 'OBJPROP_TOOLTIP', 'OBJPROP_LEVELTEXT',
                'OBJPROP_FONT', 'OBJPROP_BMPFILE', 'OBJPROP_SYMBOL',
                'BORDER_FLAT', 'BORDER_RAISED', 'BORDER_SUNKEN', 'ALIGN_LEFT', 'ALIGN_CENTER',
                'ALIGN_RIGHT', 'ANCHOR_LEFT_UPPER', 'ANCHOR_LEFT', 'ANCHOR_LEFT_LOWER',
                'ANCHOR_LOWER', 'ANCHOR_RIGHT_LOWER', 'ANCHOR_RIGHT', 'ANCHOR_RIGHT_UPPER',
                'ANCHOR_UPPER', 'ANCHOR_CENTER', 'ANCHOR_TOP', 'ANCHOR_BOTTOM',
                'CORNER_LEFT_UPPER', 'CORNER_LEFT_LOWER', 'CORNER_RIGHT_LOWER',
                'CORNER_RIGHT_UPPER',
                'OBJ_NO_PERIODS', 'EMPTY', 'OBJ_PERIOD_M1', 'OBJ_PERIOD_M5', 'OBJ_PERIOD_M15',
                'OBJ_PERIOD_M30', 'OBJ_PERIOD_H1', 'OBJ_PERIOD_H4', 'OBJ_PERIOD_D1',
                'OBJ_PERIOD_W1', 'OBJ_PERIOD_MN1', 'OBJ_ALL_PERIODS',
                'GANN_UP_TREND', 'GANN_DOWN_TREND',
                'SYMBOL_THUMBSUP', 'SYMBOL_THUMBSDOWN',
                'SYMBOL_ARROWUP', 'SYMBOL_ARROWDOWN',
                'SYMBOL_STOPSIGN', 'SYMBOL_CHECKSIGN',
                'SYMBOL_LEFTPRICE', 'SYMBOL_RIGHTPRICE',
                'PRICE_CLOSE', 'PRICE_OPEN', 'PRICE_HIGH', 'PRICE_LOW',
                'PRICE_MEDIAN', 'PRICE_TYPICAL', 'PRICE_WEIGHTED',
                'VOLUME_TICK', 'VOLUME_REAL',
                'STO_LOWHIGH', 'STO_CLOSECLOSE',
                'MODE_OPEN', 'MODE_LOW', 'MODE_HIGH', 'MODE_CLOSE', 'MODE_VOLUME', 'MODE_TIME',
                'MODE_SMA', 'MODE_EMA', 'MODE_SMMA', 'MODE_LWMA',
                'MODE_MAIN', 'MODE_SIGNAL', 'MODE_MAIN',
                'MODE_PLUSDI', 'MODE_MINUSDI', 'MODE_UPPER',
                'MODE_LOWER', 'MODE_GATORJAW', 'MODE_GATORTEETH',
                'MODE_GATORLIPS', 'MODE_TENKANSEN',
                'MODE_KIJUNSEN', 'MODE_SENKOUSPANA',
                'MODE_SENKOUSPANB', 'MODE_CHINKOUSPAN',
                'DRAW_LINE', 'DRAW_SECTION', 'DRAW_HISTOGRAM',
                'DRAW_ARROW', 'DRAW_ZIGZAG', 'DRAW_NONE',
                'STYLE_SOLID', 'STYLE_DASH', 'STYLE_DOT',
                'STYLE_DASHDOT', 'STYLE_DASHDOTDOT',
                'DRAW_NONE', 'DRAW_LINE', 'DRAW_SECTION', 'DRAW_HISTOGRAM',
                'DRAW_ARROW', 'DRAW_ZIGZAG', 'DRAW_FILLING',
                'INDICATOR_DATA', 'INDICATOR_COLOR_INDEX',
                'INDICATOR_CALCULATIONS', 'INDICATOR_DIGITS',
                'INDICATOR_HEIGHT', 'INDICATOR_LEVELS',
                'INDICATOR_LEVELCOLOR', 'INDICATOR_LEVELSTYLE',
                'INDICATOR_LEVELWIDTH', 'INDICATOR_MINIMUM',
                'INDICATOR_MAXIMUM', 'INDICATOR_LEVELVALUE',
                'INDICATOR_SHORTNAME', 'INDICATOR_LEVELTEXT',
                'TERMINAL_BUILD', 'TERMINAL_CONNECTED',
                'TERMINAL_DLLS_ALLOWED', 'TERMINAL_TRADE_ALLOWED',
                'TERMINAL_EMAIL_ENABLED',
                'TERMINAL_FTP_ENABLED', 'TERMINAL_MAXBARS',
                'TERMINAL_CODEPAGE', 'TERMINAL_CPU_CORES',
                'TERMINAL_DISK_SPACE', 'TERMINAL_MEMORY_PHYSICAL',
                'TERMINAL_MEMORY_TOTAL',
                'TERMINAL_MEMORY_AVAILABLE', 'TERMINAL_MEMORY_USED',
                'TERMINAL_X64',
                'TERMINAL_OPENCL_SUPPORT', 'TERMINAL_LANGUAGE',
                'TERMINAL_COMPANY', 'TERMINAL_NAME',
                'TERMINAL_PATH', 'TERMINAL_DATA_PATH',
                'TERMINAL_COMMONDATA_PATH',
                'MQL_PROGRAM_TYPE', 'MQL_DLLS_ALLOWED',
                'MQL_TRADE_ALLOWED', 'MQL_DEBUG',
                'MQL_PROFILER', 'MQL_TESTER', 'MQL_OPTIMIZATION',
                'MQL_VISUAL_MODE',
                'MQL_FRAME_MODE', 'MQL_LICENSE_TYPE', 'MQL_PROGRAM_NAME',
                'MQL_PROGRAM_PATH',
                'PROGRAM_SCRIPT', 'PROGRAM_EXPERT',
                'PROGRAM_INDICATOR', 'LICENSE_FREE',
                'LICENSE_DEMO', 'LICENSE_FULL', 'LICENSE_TIME',
                'MODE_LOW', 'MODE_HIGH', 'MODE_TIME', 'MODE_BID',
                'MODE_ASK', 'MODE_POINT',
                'MODE_DIGITS', 'MODE_SPREAD', 'MODE_STOPLEVEL',
                'MODE_LOTSIZE', 'MODE_TICKVALUE',
                'MODE_TICKSIZE', 'MODE_SWAPLONG',
                'MODE_SWAPSHORT', 'MODE_STARTING',
                'MODE_EXPIRATION', 'MODE_TRADEALLOWED',
                'MODE_MINLOT', 'MODE_LOTSTEP', 'MODE_MAXLOT',
                'MODE_SWAPTYPE', 'MODE_PROFITCALCMODE',
                'MODE_MARGINCALCMODE', 'MODE_MARGININIT',
                'MODE_MARGINMAINTENANCE', 'MODE_MARGINHEDGED',
                'MODE_MARGINREQUIRED', 'MODE_FREEZELEVEL',
                'SUNDAY', 'MONDAY', 'TUESDAY', 'WEDNESDAY', 'THURSDAY',
                'FRIDAY', 'SATURDAY',
                'ACCOUNT_LOGIN', 'ACCOUNT_TRADE_MODE',
                'ACCOUNT_LEVERAGE',
                'ACCOUNT_LIMIT_ORDERS', 'ACCOUNT_MARGIN_SO_MODE',
                'ACCOUNT_TRADE_ALLOWED', 'ACCOUNT_TRADE_EXPERT',
                'ACCOUNT_BALANCE',
                'ACCOUNT_CREDIT', 'ACCOUNT_PROFIT', 'ACCOUNT_EQUITY',
                'ACCOUNT_MARGIN',
                'ACCOUNT_FREEMARGIN', 'ACCOUNT_MARGIN_LEVEL',
                'ACCOUNT_MARGIN_SO_CALL',
                'ACCOUNT_MARGIN_SO_SO', 'ACCOUNT_NAME',
                'ACCOUNT_SERVER', 'ACCOUNT_CURRENCY',
                'ACCOUNT_COMPANY', 'ACCOUNT_TRADE_MODE_DEMO',
                'ACCOUNT_TRADE_MODE_CONTEST',
                'ACCOUNT_TRADE_MODE_REAL', 'ACCOUNT_STOPOUT_MODE_PERCENT',
                'ACCOUNT_STOPOUT_MODE_MONEY',
                'STAT_INITIAL_DEPOSIT', 'STAT_WITHDRAWAL', 'STAT_PROFIT',
                'STAT_GROSS_PROFIT',
                'STAT_GROSS_LOSS', 'STAT_MAX_PROFITTRADE',
                'STAT_MAX_LOSSTRADE', 'STAT_CONPROFITMAX',
                'STAT_CONPROFITMAX_TRADES', 'STAT_MAX_CONWINS',
                'STAT_MAX_CONPROFIT_TRADES',
                'STAT_CONLOSSMAX', 'STAT_CONLOSSMAX_TRADES',
                'STAT_MAX_CONLOSSES',
                'STAT_MAX_CONLOSS_TRADES', 'STAT_BALANCEMIN',
                'STAT_BALANCE_DD',
                'STAT_BALANCEDD_PERCENT', 'STAT_BALANCE_DDREL_PERCENT',
                'STAT_BALANCE_DD_RELATIVE', 'STAT_EQUITYMIN',
                'STAT_EQUITY_DD',
                'STAT_EQUITYDD_PERCENT', 'STAT_EQUITY_DDREL_PERCENT',
                'STAT_EQUITY_DD_RELATIVE', 'STAT_EXPECTED_PAYOFF',
                'STAT_PROFIT_FACTOR',
                'STAT_RECOVERY_FACTOR', 'STAT_SHARPE_RATIO',
                'STAT_MIN_MARGINLEVEL',
                'STAT_CUSTOM_ONTESTER', 'STAT_DEALS', 'STAT_TRADES',
                'STAT_PROFIT_TRADES',
                'STAT_LOSS_TRADES', 'STAT_SHORT_TRADES', 'STAT_LONG_TRADES',
                'STAT_PROFIT_SHORTTRADES', 'STAT_PROFIT_LONGTRADES',
                'STAT_PROFITTRADES_AVGCON', 'STAT_LOSSTRADES_AVGCON',
                'SERIES_BARS_COUNT', 'SERIES_FIRSTDATE', 'SERIES_LASTBAR_DATE',
                'SERIES_SERVER_FIRSTDATE', 'SERIES_TERMINAL_FIRSTDATE',
                'SERIES_SYNCHRONIZED',
                'OP_BUY', 'OP_SELL', 'OP_BUYLIMIT', 'OP_SELLLIMIT',
                'OP_BUYSTOP', 'OP_SELLSTOP',
                'TRADE_ACTION_DEAL', 'TRADE_ACTION_PENDING',
                'TRADE_ACTION_SLTP',
                'TRADE_ACTION_MODIFY', 'TRADE_ACTION_REMOVE',
                '__DATE__', '__DATETIME__', '__LINE__', '__FILE__',
                '__PATH__', '__FUNCTION__',
                '__FUNCSIG__', '__MQLBUILD__', '__MQL4BUILD__',
                'M_E', 'M_LOG2E', 'M_LOG10E', 'M_LN2', 'M_LN10',
                'M_PI', 'M_PI_2', 'M_PI_4', 'M_1_PI',
                'M_2_PI', 'M_2_SQRTPI', 'M_SQRT2', 'M_SQRT1_2',
                'CHAR_MIN', 'CHAR_MAX', 'UCHAR_MAX',
                'SHORT_MIN', 'SHORT_MAX', 'USHORT_MAX',
                'INT_MIN', 'INT_MAX', 'UINT_MAX',
                'LONG_MIN', 'LONG_MAX', 'ULONG_MAX',
                'DBL_MIN', 'DBL_MAX', 'DBL_EPSILON', 'DBL_DIG', 'DBL_MANT_DIG',
                'DBL_MAX_10_EXP', 'DBL_MAX_EXP', 'DBL_MIN_10_EXP', 'DBL_MIN_EXP',
                'FLT_MIN', 'FLT_MAX', 'FLT_EPSILON',
                'FLT_DIG', 'FLT_MANT_DIG', 'FLT_MAX_10_EXP',
                'FLT_MAX_EXP', 'FLT_MIN_10_EXP', 'FLT_MIN_EXP', 'REASON_PROGRAM'
                'REASON_REMOVE', 'REASON_RECOMPILE',
                'REASON_CHARTCHANGE', 'REASON_CHARTCLOSE',
                'REASON_PARAMETERS', 'REASON_ACCOUNT',
                'REASON_TEMPLATE', 'REASON_INITFAILED',
                'REASON_CLOSE', 'POINTER_INVALID'
                'POINTER_DYNAMIC', 'POINTER_AUTOMATIC',
                'NULL', 'EMPTY', 'EMPTY_VALUE', 'CLR_NONE', 'WHOLE_ARRAY',
                'CHARTS_MAX', 'clrNONE', 'EMPTY_VALUE', 'INVALID_HANDLE',
                'IS_DEBUG_MODE', 'IS_PROFILE_MODE', 'NULL', 'WHOLE_ARRAY', 'WRONG_VALUE',
                'ERR_NO_ERROR', 'ERR_NO_RESULT', 'ERR_COMMON_ERROR',
                'ERR_INVALID_TRADE_PARAMETERS',
                'ERR_SERVER_BUSY', 'ERR_OLD_VERSION', 'ERR_NO_CONNECTION',
                'ERR_NOT_ENOUGH_RIGHTS',
                'ERR_TOO_FREQUENT_REQUESTS', 'ERR_MALFUNCTIONAL_TRADE',
                'ERR_ACCOUNT_DISABLED',
                'ERR_INVALID_ACCOUNT', 'ERR_TRADE_TIMEOUT',
                'ERR_INVALID_PRICE', 'ERR_INVALID_STOPS',
                'ERR_INVALID_TRADE_VOLUME', 'ERR_MARKET_CLOSED',
                'ERR_TRADE_DISABLED',
                'ERR_NOT_ENOUGH_MONEY', 'ERR_PRICE_CHANGED',
                'ERR_OFF_QUOTES', 'ERR_BROKER_BUSY',
                'ERR_REQUOTE', 'ERR_ORDER_LOCKED',
                'ERR_LONG_POSITIONS_ONLY_ALLOWED', 'ERR_TOO_MANY_REQUESTS',
                'ERR_TRADE_MODIFY_DENIED', 'ERR_TRADE_CONTEXT_BUSY',
                'ERR_TRADE_EXPIRATION_DENIED',
                'ERR_TRADE_TOO_MANY_ORDERS', 'ERR_TRADE_HEDGE_PROHIBITED',
                'ERR_TRADE_PROHIBITED_BY_FIFO',
                'FILE_READ', 'FILE_WRITE', 'FILE_BIN', 'FILE_CSV', 'FILE_TXT',
                'FILE_ANSI', 'FILE_UNICODE',
                'FILE_SHARE_READ', 'FILE_SHARE_WRITE', 'FILE_REWRITE',
                'FILE_COMMON', 'FILE_EXISTS',
                'FILE_CREATE_DATE', 'FILE_MODIFY_DATE',
                'FILE_ACCESS_DATE', 'FILE_SIZE', 'FILE_POSITION',
                'FILE_END', 'FILE_LINE_END', 'FILE_IS_COMMON',
                'FILE_IS_TEXT', 'FILE_IS_BINARY',
                'FILE_IS_CSV', 'FILE_IS_ANSI', 'FILE_IS_READABLE', 'FILE_IS_WRITABLE',
                'SEEK_SET', 'SEEK_CUR', 'SEEK_END', 'CP_ACP',
                'CP_OEMCP', 'CP_MACCP', 'CP_THREAD_ACP',
                'CP_SYMBOL', 'CP_UTF7', 'CP_UTF8', 'IDOK', 'IDCANCEL', 'IDABORT',
                'IDRETRY', 'IDIGNORE', 'IDYES', 'IDNO', 'IDTRYAGAIN', 'IDCONTINUE',
                'MB_OK', 'MB_OKCANCEL', 'MB_ABORTRETRYIGNORE', 'MB_YESNOCANCEL',
                'MB_YESNO', 'MB_RETRYCANCEL',
                'MB_CANCELTRYCONTINUE', 'MB_ICONSTOP', 'MB_ICONERROR',
                'MB_ICONHAND', 'MB_ICONQUESTION',
                'MB_ICONEXCLAMATION', 'MB_ICONWARNING',
                'MB_ICONINFORMATION', 'MB_ICONASTERISK',
                'MB_DEFBUTTON1', 'MB_DEFBUTTON2', 'MB_DEFBUTTON3',
                'MB_DEFBUTTON4'), suffix=r'\b'),
             Name.Constant),
            (words((
                'Black', 'DarkGreen', 'DarkSlateGray', 'Olive',
                'Green', 'Teal', 'Navy', 'Purple',
                'Maroon', 'Indigo', 'MidnightBlue', 'DarkBlue',
                'DarkOliveGreen', 'SaddleBrown',
                'ForestGreen', 'OliveDrab', 'SeaGreen',
                'DarkGoldenrod', 'DarkSlateBlue',
                'Sienna', 'MediumBlue', 'Brown', 'DarkTurquoise',
                'DimGray', 'LightSeaGreen',
                'DarkViolet', 'FireBrick', 'MediumVioletRed',
                'MediumSeaGreen', 'Chocolate',
                'Crimson', 'SteelBlue', 'Goldenrod', 'MediumSpringGreen',
                'LawnGreen', 'CadetBlue',
                'DarkOrchid', 'YellowGreen', 'LimeGreen', 'OrangeRed',
                'DarkOrange', 'Orange',
                'Gold', 'Yellow', 'Chartreuse', 'Lime', 'SpringGreen',
                'Aqua', 'DeepSkyBlue', 'Blue',
                'Magenta', 'Red', 'Gray', 'SlateGray', 'Peru', 'BlueViolet',
                'LightSlateGray', 'DeepPink',
                'MediumTurquoise', 'DodgerBlue', 'Turquoise', 'RoyalBlue',
                'SlateBlue', 'DarkKhaki',
                'IndianRed', 'MediumOrchid', 'GreenYellow',
                'MediumAquamarine', 'DarkSeaGreen',
                'Tomato', 'RosyBrown', 'Orchid', 'MediumPurple',
                'PaleVioletRed', 'Coral', 'CornflowerBlue',
                'DarkGray', 'SandyBrown', 'MediumSlateBlue',
                'Tan', 'DarkSalmon', 'BurlyWood',
                'HotPink', 'Salmon', 'Violet', 'LightCoral', 'SkyBlue',
                'LightSalmon', 'Plum',
                'Khaki', 'LightGreen', 'Aquamarine', 'Silver',
                'LightSkyBlue', 'LightSteelBlue',
                'LightBlue', 'PaleGreen', 'Thistle', 'PowderBlue',
                'PaleGoldenrod', 'PaleTurquoise',
                'LightGray', 'Wheat', 'NavajoWhite', 'Moccasin',
                'LightPink', 'Gainsboro', 'PeachPuff',
                'Pink', 'Bisque', 'LightGoldenrod', 'BlanchedAlmond',
                'LemonChiffon', 'Beige',
                'AntiqueWhite', 'PapayaWhip', 'Cornsilk',
                'LightYellow', 'LightCyan', 'Linen',
                'Lavender', 'MistyRose', 'OldLace', 'WhiteSmoke',
                'Seashell', 'Ivory', 'Honeydew',
                'AliceBlue', 'LavenderBlush', 'MintCream', 'Snow',
                'White'), prefix='(clr)?', suffix=r'\b'),
             Name.Constant),
            inherit,
        ],
    }
