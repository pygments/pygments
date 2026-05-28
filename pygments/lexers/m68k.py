# M68K Assembly Lexer for Pygments
# Copyright (C) 2025  ggelado
#
# Licensed under the EUPL, Version 1.2 or – as soon they will be
# approved by the European Commission – subsequent versions of the
# EUPL (the "Licence").
#
# You may not use this work except in compliance with the Licence.
# You may obtain a copy of the Licence at:
#
#     https://eupl.eu/
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the Licence is distributed on an "AS IS"
# basis, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
# or implied. See the Licence for the specific language governing
# permissions and limitations under the Licence.
from pygments.lexer import RegexLexer
from pygments.token import *
import re

class M68KLexer(RegexLexer):
    name = 'Assembly (m68k)'
    aliases = ['m68k']
    filenames = ['*.s', '*.S', '*.i', '*.I']
    mimetypes = ['text/x-m68k']

    tokens = {
        'root': [
            (r'(;|\B\*).*$' , Comment.Single),
            (r'"[^"\n]*"', String),
            (r"'\S'", String.Char),

            (r'^[a-zA-Z_][a-zA-Z0-9_]*:', Name.Function),
            (r'^(\.|_)[a-zA-Z_][a-zA-Z0-9_]*:?', Name.Namespace),

            (r'-?\b[0-9]+\b', Number.Integer),
            (r'-?\$[0-9a-fA-F]+\b', Number.Hex),

            (r'(?i)\b([ad][0-7]|sp|pc|sr)\b', Name.Builtin),
            (r'(?i)\b(usp|dfc|sfc|vbr|cacr|caar|msp|isp)\b', Name.Variable),
            (r'(?i)\b(crp|srp|tc|tt[01])\b', Name.Variable),

            (r'(?i)\b(fp[0-7])\b', Name.Builtin),

            (r'#-?\$?[0-9a-fA-F]+', Number),

            (r'(?i)\b(reset|rte|stop|pflusha|pflushan|frestore|fsave|move16|pmove|movec|moves|cinva|cpusha|cpush[lp]|cinv[lp]|RTS)\b', Operator.Word),

            (r'(?i)\b(bra|bcc|bne|beq|blt|ble|bgt|bge|bhi|bhs|blo|bls|bvs|bvc|bsr|jmp|jsr|dbra|dbf|dbt|dbeq|dbne|dblt|dbgt|dble|dbge)\b', Operator.Word),

            (r'(?i)\b(fabs|fadd|fdiv|fsub|fmul|fmove|fsqrt|fint|fneg|fcmp|ftst|fcos|fsin|ftan|flogn|fetox|fmod|frem|fscale|fsincos|fmovecr|fnop|ftrap)\b', Operator.Word),

            (r'(?i)\b(f(b|s)?(f|eq|ne|gt|lt|ge|le|t|un|gl|gle|ngle|nle|ngt|nge|nlt|ogl|or|seq|sf|st|sne|sogt|soge|sole|sult|suge|sugt))\b', Operator.Word),

            (r'(?i)\b([as]bcd|add[aiqx]?|adda|and[i]?|andi|or[i]?|ori|sub[aiqx]?|suba|cmp[il]?|cmpa|clr|not|tst|ext|swap|pea|link|unlk|trap|trapv|nop|lea|move[a]?|movem|movep|moveq|div[su]|muls?|mulu?|nbcd|exg|chk2?|cas2?|illegal|pack|unpk|move\.[blw]|move)\b', Operator.Word),

            (r'(?i)\b(even|cnop|macro|endm|rept|endr|section|text|data|bss|end|dc[bswl]?|dcb[bswl]?|set|equ|fequ|reg|xref|xdef|include|incbin|opt|machine|fpu|comment)\b', Keyword.Pseudo),

            (r'\b([a-zA-Z_][a-zA-Z0-9_]*)\b', Name),

            (r'\s+', Text),
            (r'[%+-.,:()#]', Punctuation),
        ],
    }
