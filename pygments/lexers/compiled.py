# -*- coding: utf-8 -*-
"""
    pygments.lexers.compiled
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for compiled languages: C/C++, Delphi, Java.

    :copyright: 2006 by Georg Brandl, Armin Ronacher, Christoph Hack.
    :license: GNU LGPL, see LICENSE for more details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups, using, this
from pygments.token import \
     Text, Comment, Operator, Keyword, Name, String, Number


__all__ = ['CLexer', 'CppLexer', 'DelphiLexer', 'JavaLexer']


class CLexer(RegexLexer):
    name = 'C'
    aliases = ['c']
    filenames = ['*.c', '*.h']
    mimetypes = ['text/x-chdr', 'text/x-csrc']

    #: optional Comment or Whitespace
    _ws = r'(?:\s|//.*?\n|/[*].*?[*]/)+'

    tokens = {
        'whitespace': [
            (r'^\s*#if\s+0', Comment.Preproc, 'if0'),
            (r'^\s*#', Comment.Preproc, 'macro'),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'//(\n|(.|\n)*?[^\\]\n)', Comment),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment),
        ],
        'statements': [
            (r'L?"', String, 'string'),
            (r"L?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", String.Char),
            (r'(0x[0-9a-fA-F]|0[0-7]+|(\d+\.\d*|\.\d+)|\d+)'
             r'e[+-]\d+[lL]?', Number.Float),
            (r'0x[0-9a-fA-F]+[Ll]?', Number.Hex),
            (r'0[0-7]+[Ll]?', Number.Oct),
            (r'(\d+\.\d*|\.\d+)', Number.Float),
            (r'\d+', Number.Integer),
            (r'[~!%^&*()+=|\[\]:,.<>/?-]', Text),
            (r'(auto|break|case|const|continue|default|do|else|enum|extern|'
             r'for|goto|if|register|restricted|return|sizeof|static|struct|'
             r'switch|typedef|union|volatile|virtual|while)\b', Keyword),
            (r'(int|long|float|short|double|char|unsigned|signed|void|'
             r'_Complex|_Imaginary|_Bool)\b', Keyword.Type),
            (r'(_{0,2}inline|naked|restrict|thread|typename)\b', Keyword.Reserved),
            (r'__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|'
             r'declspec|finally|int64|try|leave)\b', Keyword.Reserved),
            (r'(true|false|NULL)\b', Keyword.Constant),
            ('[a-zA-Z_][a-zA-Z0-9_]*:', Name.Label),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
        ],
        'root': [
            include('whitespace'),
            # functions
            (r'((?:[a-zA-Z0-9_*\s])+?(?:\s|[*]))'    # return arguments
             r'([a-zA-Z_][a-zA-Z0-9_]*)'             # method name
             r'(\s*\([^;]*?\))'                      # signature
             r'(' + _ws + r')({)',
             bygroups(using(this), Name.Function, using(this), Text, Keyword),
             'function'),
            # function declarations
            (r'((?:[a-zA-Z0-9_*\s])+?(?:\s|[*]))'    # return arguments
             r'([a-zA-Z_][a-zA-Z0-9_]*)'             # method name
             r'(\s*\([^;]*?\))'                      # signature
             r'(' + _ws + r')(;)',
             bygroups(using(this), Name.Function, using(this), Text, Text)),
            ('', Text, 'statement'),
        ],
        'statement' : [
            include('whitespace'),
            include('statements'),
            ('[{}]', Keyword),
            (';', Text, '#pop'),
        ],
        'function': [
            include('whitespace'),
            include('statements'),
            (';', Text),
            ('{', Keyword, '#push'),
            ('}', Keyword, '#pop'),
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
            (r'/[*](.|\n)*?[*]/', Comment),
            (r'//.*?\n', Comment, '#pop'),
            (r'/', Comment.Preproc),
            (r'(?<=\\)\n', Comment.Preproc),
            (r'\n', Comment.Preproc, '#pop'),
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment, '#push'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment, '#pop'),
            (r'.*?\n', Comment),
        ]
    }


class CppLexer(RegexLexer):
    name = 'C++'
    aliases = ['cpp', 'c++']
    filenames = ['*.cpp', '*.hpp', '*.c++', '*.h++']
    mimetypes = ['text/x-c++hdr', 'text/x-c++src']

    tokens = {
        'root': [
            (r'^\s*#if\s+0', Comment.Preproc, 'if0'),
            (r'^\s*#', Comment.Preproc, 'macro'),
            (r'\n', Text),
            (r'\s+', Text),
            (r'\\\n', Text), # line continuation
            (r'//(\n|(.|\n)*?[^\\]\n)', Comment),
            (r'/(\\\n)?[*](.|\n)*?[*](\\\n)?/', Comment),
            (r'[{}]', Keyword),
            (r'L?"', String, 'string'),
            (r"L?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", String.Char),
            (r'(0x[0-9a-fA-F]|0[0-7]+|(\d+\.\d*|\.\d+)|\d+)'
             r'e[+-]\d+[lL]?', Number.Float),
            (r'0x[0-9a-fA-F]+[Ll]?', Number.Hex),
            (r'0[0-7]+[Ll]?', Number.Oct),
            (r'(\d+\.\d*|\.\d+)', Number.Float),
            (r'\d+', Number.Integer),
            (r'[~!%^&*()+=|\[\]:;,.<>/?-]', Text),
            (r'(asm|auto|break|case|catch|const|const_cast|continue|'
             r'default|delete|do|dynamic_cast|else|enum|explicit|export|'
             r'extern|for|friend|goto|if|mutable|namespace|new|operator|'
             r'private|protected|public|register|reinterpret_cast|return|'
             r'sizeof|static|static_cast|struct|switch|template|this|throw|'
             r'throws|try|typedef|typeid|typename|union|using|volatile|'
             r'virtual|while)\b', Keyword),
            (r'(class)(\s+)', bygroups(Keyword, Text), 'classname'),
            (r'(bool|int|long|float|short|double|char|unsigned|signed|'
             r'void|wchar_t)\b', Keyword.Type),
            (r'(_{0,2}inline|naked|thread)\b', Keyword.Reserved),
            (r'__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|'
             r'declspec|finally|int64|try|leave|wchar_t|w64|virtual_inheritance|'
             r'uuidof|unaligned|super|single_inheritance|raise|noop|'
             r'multiple_inheritance|m128i|m128d|m128|m64|interface|'
             r'identifier|forceinline|event|assume)\b', Keyword.Reserved),
            (r'(true|false|NULL)\b', Keyword.Constant),
            ('[a-zA-Z_][a-zA-Z0-9_]*:', Name.Label),
            ('[a-zA-Z_][a-zA-Z0-9_]*', Name),
        ],
        'classname': [
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name.Class, '#pop')
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
            (r'/[*](.|\n)*?[*]/', Comment),
            (r'//.*?\n', Comment, '#pop'),
            (r'/', Comment.Preproc),
            (r'(?<=\\)\n', Comment.Preproc),
            (r'\n', Comment.Preproc, '#pop'),
        ],
        'if0': [
            (r'^\s*#if.*?(?<!\\)\n', Comment, '#push'),
            (r'^\s*#endif.*?(?<!\\)\n', Comment, '#pop'),
            (r'.*?\n', Comment),
        ]
    }


class DelphiLexer(RegexLexer):
    name = 'Delphi'
    aliases = ['delphi', 'pas', 'pascal', 'objectpascal']
    filenames = ['*.pas']
    mimetypes = ['text/x-pascal']

    flags = re.IGNORECASE | re.MULTILINE | re.DOTALL
    tokens = {
        'root': [
            (r'\s+', Text),
            (r'asm\b', Keyword, 'asm'),
            (r'(uses)(\s+)', bygroups(Keyword, Text), 'uses'),
            (r'(procedure|function)(\s+)', bygroups(Keyword, Text), 'funcname'),
            (r'(abstract|and|array|as|assembler|at|begin|case|cdecl|'
             r'class|const|constructor|contains|destructor|dispinterface|'
             r'div|do|downto|else|end|except|false|far|file|finalization|'
             r'finally|for|goto|if|implementation|in|inherited|'
             r'initialization|inline|interface|is|label|mod|near|nil|not|'
             r'object|of|on|or|overload|override|package|packed|pascal|'
             r'private|program|property|protected|public|'
             r'published|raise|record|register|repeat|requires|resourcestring|'
             r'safecall|self|set|shl|shr|stdcall|then|threadvar|to|true|try|'
             r'type|unit|until|uses|var|varargs|virtual|while|with|xor|'
             r'break|assert|dec|inc)\b', Keyword),
            (r'(AnsiString|Boolean|Byte|ByteBool|Cardinal|Char|Comp|'
             r'Currency|Double|Extended|Int64|Integer|LongBool|LongInt|Real|'
             r'Real48|ShortInt|ShortString|Single|SmallInt|String|WideChar|'
             r'WideString|Word|WordBool)\b', Keyword.Type),
            (r'\{.*?\}', Comment),
            (r'\(\*.*?\*\)', Comment),
            (r'//.*?\n', Comment),
            (r"'(''|[^']*)'", String),
            (r'\$[0-9a-fA-F]+', Number),
            (r'\#\$?[0-9]{1,3}', Number),
            (r'[0-9]', Number),
            (r'[@~!%^&*()+=|\[\]:;,.<>/?-]', Text),
            (r'[a-zA-Z_][a-zA-Z0-9_]*:', Name.Label),
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name),
        ],
        'uses': [
            (r'[a-zA-Z_][a-zA-Z0-9_.]*', Name.Namespace),
            (r'\s*,\s*', Text),
            (r';', Text, '#pop')
        ],
        'funcname': [
            (r'[a-zA-Z_][a-zA-Z0-9_.]*', Name.Function, '#pop')
        ],
        'asm': [
            (r'end', Keyword, '#pop'),
            (r'\s+', Text),
            (r'\{.*?\}', Comment),
            (r'\(\*.*?\*\)', Comment),
            (r'//.*?\n', Comment),
            (r'(AAA|AAD|AAM|AAS|ADC|ADD|AND|ARPL|BOUND|BSF|BSR|BSWAP|BT|'
             r'BTC|BTR|BTS|CALL|CBW|CDQ|CLC|CLD|CLI|CLTS|CMC|CMP|CMPSB|'
             r'CMPSD|CMPSW|CMPXCHG|CMPXCHG486|CMPXCHG8B|CPUID|CWD|CWDE|'
             r'DAA|DAS|DEC|DIV|EMMS|ENTER|HLT|IBTS|ICEBP|IDIV|IMUL|IN|INC|'
             r'INSB|INSD|INSW|INT|INT01|INT03|INT1|INT3|INTO|INVD|INVLPG|'
             r'IRET|IRETD|IRETW|JCXZ|JECXZ|JMP|LAHF|LAR|LCALL|LDS|LEA|LEAVE|'
             r'LES|LFS|LGDT|LGS|LIDT|LJMP|LLDT|LMSW|LOADALL|LOADALL286|LOCK|'
             r'LODSB|LODSD|LODSW|LOOP|LOOPE|LOOPNE|LOOPNZ|LOOPZ|LSL|LSS|LTR|'
             r'MOV|MOVD|MOVQ|MOVSB|MOVSD|MOVSW|MOVSX|MOVZX|MUL|NEG|NOP|NOT|'
             r'OR|OUT|OUTSB|OUTSD|OUTSW|POP|POPA|POPAD|POPAW|POPF|POPFD|'
             r'POPFW|PUSH|PUSHA|PUSHAD|PUSHAW|PUSHF|PUSHFD|PUSHFW|RCL|RCR|'
             r'RDMSR|RDPMC|RDSHR|RDTSC|REP|REPE|REPNE|REPNZ|REPZ|RET|RETF|'
             r'RETN|ROL|ROR|RSDC|RSLDT|RSM|SAHF|SAL|SALC|SAR|SBB|SCASB|SCASD|'
             r'SCASW|SGDT|SHL|SHLD|SHR|SHRD|SIDT|SLDT|SMI|SMINT|SMINTOLD|'
             r'SMSW|STC|STD|STI|STOSB|STOSD|STOSW|STR|SUB|SVDC|SVLDT|SVTS|'
             r'SYSCALL|SYSENTER|SYSEXIT|SYSRET|TEST|UD1|UD2|UMOV|VERR|VERW|'
             r'WAIT|WBINVD|WRMSR|WRSHR|XADD|XBTS|XCHG|XLAT|XLATB|XOR|cmova|'
             r'cmovae|cmovb|cmovbe|cmovc|cmovcxz|cmove|cmovg|cmovge|cmovl|'
             r'cmovle|cmovna|cmovnae|cmovnb|cmovnbe|cmovnc|cmovne|cmovng|'
             r'cmovnge|cmovnl|cmovnle|cmovno|cmovnp|cmovns|cmovnz|cmovo|'
             r'cmovp|cmovpe|cmovpo|cmovs|cmovz|ja|jae|jb|jbe|jc|jcxz|je|jg|'
             r'jge|jl|jle|jna|jnae|jnb|jnbe|jnc|jne|jng|jnge|jnl|jnle|jno|'
             r'jnp|jns|jnz|jo|jp|jpe|jpo|js|jz|seta|setae|setb|setbe|setc|'
             r'setcxz|sete|setg|setge|setl|setle|setna|setnae|setnb|setnbe|'
             r'setnc|setne|setng|setnge|setnl|setnle|setno|setnp|setns|setnz|'
             r'seto|setp|setpe|setpo|sets|setz)\b', Keyword),
            ('[a-zA-Z_@][a-zA-Z0-9_]*', Name),
            (r'\$[0-9]+', Number),
            (r"'(''|[^']+)'", String),
            (r'.', Text)
        ]
    }


class JavaLexer(RegexLexer):
    name = 'Java'
    aliases = ['java']
    filenames = ['*.java']
    mimetypes = ['text/x-java']

    flags = re.MULTILINE | re.DOTALL

    #: optional Comment or Whitespace
    _ws = r'(?:\s|//.*?\n|/[*].*?[*]/)+'

    tokens = {
        'root': [
            # method names
            (r'^(\s*(?:[a-zA-Z_][a-zA-Z0-9_\.]*\s+)+?)'  # return arguments
             r'([a-zA-Z_][a-zA-Z0-9_]*)'                 # method name
             r'(\s*\([^;]*?\))'                          # signature
             r'(?=' + _ws +                              # exception declaration
             r'(?:throws\s+(?:[a-zA-Z_][a-zA-Z0-9_]*,?\s*)+)?' +
             _ws + r'\{)',
             bygroups(using(this), Name.Function, using(this))),
            (r'[^\S\n]+', Text),
            (r'//.*?\n', Comment),
            (r'/\*.*?\*/', Comment),
            (r'@[a-zA-Z_][a-zA-Z0-9_\.]*', Name.Decorator),
            (r'(abstract|assert|break|case|catch|'
             r'const|continue|default|do|else|enum|extends|final|'
             r'finally|for|if|goto|implements|import|instanceof|'
             r'interface|native|new|package|private|protected|public|'
             r'return|static|strictfp|super|switch|synchronized|this|'
             r'throw|throws|transient|try|volatile|while)\b', Keyword),
            (r'(boolean|byte|char|double|float|int|long|short|void)\b',
             Keyword.Type),
            (r'(true|false|null)\b', Keyword.Constant),
            (r'(class)(\s+)', bygroups(Keyword, Text), 'class'),
            (r'"(\\\\|\\"|[^"])*"', String),
            (r"'\\.'|'[^\\]'|'\\u[0-9a-f]{4}'", String.Char),
            (r'[a-zA-Z_][a-zA-Z0-9_]*:', Name.Label),
            (r'[a-zA-Z_\$][a-zA-Z0-9_]*', Name),
            (r'[~\^\*!%&\[\]\(\)\{\}<>\|+=:;,./?-]', Operator),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number),
            (r'[0-9]+L?', Number),
            (r'0x[0-9a-f]+', Number),
            (r'\n', Text)
        ],
        'class': [
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name.Class, '#pop')
        ]
    }
