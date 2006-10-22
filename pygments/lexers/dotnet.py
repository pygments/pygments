# -*- coding: utf-8 -*-
"""
    pygments.lexers.dotnet
    ~~~~~~~~~~~~~~~~~~~~~

    .net languages

    :copyright: 2006 by Armin Ronacher.
    :license: GNU LGPL, see LICENSE for more details.
"""
import re

from pygments.lexer import RegexLexer, bygroups, using, this
from pygments.token import \
     Text, Comment, Operator, Keyword, Name, String, Number, Literal

__all__ = ['CSharpLexer', 'BooLexer', 'VbNetLexer']


class CSharpLexer(RegexLexer):
    name = 'C#'
    aliases = ['csharp', 'c#']
    filenames = ['*.cs']

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'root': [
            # method names
            (r'^([ \t]*(?:[a-zA-Z_][a-zA-Z0-9_\.]*\s+)+?)' # return arguments
             r'([a-zA-Z_][a-zA-Z0-9_]*)'                   # method name
             r'(\s*\([^;]*?\))'                            # signature
             r'(?=(?:\s|//.*?\n|/[*].*?[*]/)+\{)',         # lookahead for {
             bygroups(using(this), Name.Function, using(this))),
            (r'^\s*\[.*?\]', Name.Attribute),
            (r'[^\S\n]+', Text),
            (r'\\\n', Text), # line continuation
            (r'//.*?\n', Comment),
            (r'/[*](.|\n)*?[*]/', Comment),
            (r'\n', Text),
            (r'[~!%^&*()+=|\[\]:;,.<>/?-]', Text),
            (r'[{}]', Keyword),
            (r'@"(\\\\|\\"|[^"])*"', String),
            (r'"(\\\\|\\"|[^"\n])*["\n]', String),
            (r"'\\.'|'[^\\]'", String.Char),
            (r"[0-9](\.[0-9]*)?([eE][+-][0-9]+)?"
             r"[flFLdD]?|0[xX][0-9a-fA-F]+[Ll]?", Number),
            (r'#[ \t]*(if|endif|else|elif|define|undef|'
             r'line|error|warning|region|endregion)\b.*?\n', Comment.Preproc),
            (r'(abstract|case|as|base|break|case|catch|'
             r'checked|const|continue|default|delegate|'
             r'do|else|enum|event|explicit|extern|false|finally|'
             r'fixed|for|foreach|goto|if|implicit|in|interface|'
             r'internal|is|lock|nwe|null|operator|'
             r'out|override|params|private|protected|public|readonly|'
             r'ref|return|sealed|sizeof|stackalloc|static|'
             r'switch|this|throw|true|try|typeof|'
             r'unchecked|unsafe|virtual|void|while|'
             r'get|set|new)\b', Keyword),
            (r'(bool|byte|char|decimal|double|float|int|long|object|sbyte|'
             r'short|string|uint|ulong|ushort)\b', Keyword.Type),
            (r'(class|struct)(\s+)', bygroups(Keyword, Text), 'class'),
            (r'(namespace|using)(\s+)', bygroups(Keyword, Text), 'namespace'),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
        ],
        'class': [
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name.Class, '#pop')
        ],
        'namespace': [
            (r'(?=\()', Text, '#pop'), # using (resource)
            (r'[a-zA-Z_][a-zA-Z0-9_.]*', Name.Namespace, '#pop')
        ]
    }


class BooLexer(RegexLexer):
    name = 'Boo'
    aliases = ['boo']
    filenames = ['*.boo']

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'(#|//).*$', Comment),
            (r'/[*]', Comment, 'comment'),
            (r'[]{}:(),.;[]', Text),
            (r'\\\n', Text),
            (r'\\', Text),
            (r'(in|is|and|or|not)\b', Operator.Word),
            (r'/(\\\\|\\/|[^/\s])/', String.Regex),
            (r'@/(\\\\|\\/|[^/])*/', String.Regex),
            (r'=~|!=|==|<<|>>|[-+/*%=<>&^|]', Operator),
            (r'(as|abstract|callable|constructor|destructor|do|import|'
             r'enum|event|final|get|interface|internal|of|override|'
             r'partial|private|protected|public|return|set|static|'
             r'struct|transient|virtual|yield|super|and|break|cast|'
             r'continue|elif|else|ensure|except|for|given|goto|if|in|'
             r'is|isa|not|or|otherwise|pass|raise|ref|try|unless|when|'
             r'while|from|as)\b', Keyword),
            (r'def(?=\s+\(.*?\))', Keyword),
            (r'(def)(\s+)', bygroups(Keyword, Text), 'funcname'),
            (r'(class)(\s+)', bygroups(Keyword, Text), 'classname'),
            (r'(namespace)(\s+)', bygroups(Keyword, Text), 'namespace'),
            (r'(?<!\.)(true|false|null|self|__eval__|__switch__|array|'
             r'assert|checked|enumerate|filter|getter|len|lock|map|'
             r'matrix|max|min|normalArrayIndexing|print|property|range|'
             r'rawArrayIndexing|required|typeof|unchecked|using|'
             r'yieldAll|zip)\b', Name.Builtin),
            ('"""(\\\\|\\"|.*?)"""', String.Double),
            ('"(\\\\|\\"|[^"]*?)"', String.Double),
            ("'(\\\\|\\'|[^']*?)'", String.Single),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
            (r'(\d+\.\d*|\d*\.\d+)([fF][+-]?[0-9]+)?', Number.Float),
            (r'[0-9][0-9\.]*(m|ms|d|h|s)', Number),
            (r'0\d+', Number.Oct),
            (r'0x[a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+', Number.Integer),
        ],
        'comment': [
            ('/[*]', Comment.Multiline, '#push'),
            ('[*]/', Comment.Multiline, '#pop'),
            ('[^/*]', Comment.Multiline),
            ('[*/]', Comment.Multiline)
        ],
        'funcname': [
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name.Function, '#pop')
        ],
        'classname': [
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name.Class, '#pop')
        ],
        'namespace': [
            ('[a-zA-Z_][a-zA-Z0-9_.]*', Name.Namespace, '#pop')
        ]
    }


class VbNetLexer(RegexLexer):
    name = 'VB.net'
    aliases = ['vb.net', 'vbnet']
    filenames = ['*.vb', '*.bas']

    flags = re.MULTILINE | re.IGNORECASE
    tokens = {
        'root': [
            (r'^\s*<.*?>', Name.Attribute),
            (r'\s+', Text),
            (r'\n', Text),
            (r'rem\b.*?\n', Comment),
            (r"'.*?\n", Comment),
            (r'[\(\){}!#,.:]', Text),
            (r'#If\s.*?\sThen|#ElseIf\s.*?\sThen|#End\s+If|#Const|'
             r'#ExternalSource.*?\n|#End\s+ExternalSource|'
             r'#Region.*?\n|#End\s+Region|#ExternalChecksum',
             Comment.Preproc),
            (r'Option\s+(Strict|Explicit|Compare)\s+'
             r'(On|Off|Binary|Text)', Keyword.Declaration),
            (r'(?<!\.)(AddHandler|Alias|'
             r'ByRef|ByVal|Call|Case|Catch|CBool|CByte|CChar|CDate|'
             r'CDec|CDbl|CInt|CLng|CObj|Const|Continue|CSByte|CShort|'
             r'CSng|CStr|CType|CUInt|CULng|CUShort|Declare|'
             r'Default|Delegate|Dim|DirectCast|Do|Each|Else|ElseIf|'
             r'End|EndIf|Enum|Erase|Error|Event|Exit|False|Finally|For|'
             r'Friend|Function|Get|Global|GoSub|GoTo|Handles|If|'
             r'Implements|Imports|Inherits|Interface|'
             r'Let|Lib|Loop|Me|Module|MustInherit|'
             r'MustOverride|MyBase|MyClass|Namespace|Narrowing|New|Next|'
             r'Not|Nothing|NotInheritable|NotOverridable|Of|On|'
             r'Operator|Option|Optional|Overloads|Overridable|'
             r'Overrides|ParamArray|Partial|Private|Property|Protected|'
             r'Public|RaiseEvent|ReadOnly|ReDim|RemoveHandler|Resume|'
             r'Return|Select|Set|Shadows|Shared|Single|'
             r'Static|Step|Stop|Structure|Sub|SyncLock|Then|'
             r'Throw|To|True|Try|TryCast|Wend|'
             r'Using|When|While|Widening|With|WithEvents|'
             r'WriteOnly)\b', Keyword),
            (r'(?<!\.)(Function|Sub|Property)(\s+)',
             bygroups(Keyword, Text), 'funcname'),
            (r'(?<!\.)(Class|Structure|Enum)(\s+)',
             bygroups(Keyword, Text), 'classname'),
            (r'(?<!\.)(Namespace|Imports)(\s+)',
             bygroups(Keyword, Text), 'namespace'),
            (r'(?<!\.)(Boolean|Byte|Char|Date|Decimal|Double|Integer|Long|'
             r'Object|SByte|Short|Single|String|Variant|UInteger|ULong|'
             r'UShort)\b', Keyword.Type),
            (r'(?<!\.)(AddressOf|And|AndAlso|As|GetType|In|Is|IsNot|Like|Mod|'
             r'Or|OrElse|TypeOf|Xor)\b', Operator.Word),
            (r'&=|[*]=|/=|\\=|\^=|\+=|-=|<<=|>>=|<<|>>|:=|'
             r'<=|>=|<>|[-&*/\\^+=<>]',
             Operator),
            ('"', String, 'string'),
            ('[a-zA-Z_][a-zA-Z0-9_]*[%&@!#$]?', Name),
            ('#.*?#', Literal.Date),
            (r'(\d+\.\d*|\d*\.\d+)([fF][+-]?[0-9]+)?', Number.Float),
            (r'\d+([SILDFR]|US|UI|UL)?', Number.Integer),
            (r'&H[0-9a-f]+([SILDFR]|US|UI|UL)?', Number.Integer),
            (r'&O[0-7]+([SILDFR]|US|UI|UL)?', Number.Integer),
            (r'_\n', Text), # Line continuation
        ],
        'string': [
            (r'""', String),
            (r'"C?', String, '#pop'),
            (r'[^"]+', String),
        ],
        'funcname': [
            (r'[a-z_][a-z0-9_]*', Name.Function, '#pop')
        ],
        'classname': [
            (r'[a-z_][a-z0-9_]*', Name.Class, '#pop')
        ],
        'namespace': [
            (r'[a-z_][a-z0-9_.]*', Name.Namespace, '#pop')
        ],
    }
