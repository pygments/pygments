#author: ggelado
# LICENSE: GPL
# there is no warranty for this free software
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
            (r'(;|\B\*).*$' , Comment.Single),  # Comentarios
            (r'"[^"\n]*"', String),  # Cadenas
            (r"'\S'", String.Char),  # Caracteres

            # Labels
            (r'^[a-zA-Z_][a-zA-Z0-9_]*:', Name.Function),  # Funciones (labels)
            (r'^(\.|_)[a-zA-Z_][a-zA-Z0-9_]*:?', Name.Namespace),  # Espacios de nombres

            # Números
            (r'-?\b[0-9]+\b', Number.Integer),  # Números enteros
            (r'-?\$[0-9a-fA-F]+\b', Number.Hex),  # Números hexadecimales

            # Registros
            (r'(?i)\b([ad][0-7]|sp|pc|sr)\b', Name.Builtin),  # Registros (como A0, D0, PC, etc)
            (r'(?i)\b(usp|dfc|sfc|vbr|cacr|caar|msp|isp)\b', Name.Variable),  # Registros de control
            (r'(?i)\b(crp|srp|tc|tt[01])\b', Name.Variable),  # Registros adicionales

            # Registros de punto flotante
            (r'(?i)\b(fp[0-7])\b', Name.Builtin),

            # Inmediatos
            (r'#-?\$?[0-9a-fA-F]+', Number),  # Valores inmediatos

            # Instrucciones privilegiadas
            (r'(?i)\b(reset|rte|stop|pflusha|pflushan|frestore|fsave|move16|pmove|movec|moves|cinva|cpusha|cpush[lp]|cinv[lp]|RTS)\b', Operator.Word),

            # Instrucciones de salto (branch instructions)
            (r'(?i)\b(bra|bcc|bne|beq|blt|ble|bgt|bge|bhi|bhs|blo|bls|bvs|bvc|bsr|jmp|jsr|dbra|dbf|dbt|dbeq|dbne|dblt|dbgt|dble|dbge)\b', Operator.Word),

            # Condicionales de punto flotante
            (r'(?i)\b(fabs|fadd|fdiv|fsub|fmul|fmove|fsqrt|fint|fneg|fcmp|ftst|fcos|fsin|ftan|flogn|fetox|fmod|frem|fscale|fsincos|fmovecr|fnop|ftrap)\b', Operator.Word),

            # Condicionales de salto para FP
            (r'(?i)\b(f(b|s)?(f|eq|ne|gt|lt|ge|le|t|un|gl|gle|ngle|nle|ngt|nge|nlt|ogl|or|seq|sf|st|sne|sogt|soge|sole|sult|suge|sugt))\b', Operator.Word),

            # Instrucciones generales (con variantes)
            (r'(?i)\b([as]bcd|add[aiqx]?|adda|and[i]?|andi|or[i]?|ori|sub[aiqx]?|suba|cmp[il]?|cmpa|clr|not|tst|ext|swap|pea|link|unlk|trap|trapv|nop|lea|move[a]?|movem|movep|moveq|div[su]|muls?|mulu?|nbcd|exg|chk2?|cas2?|illegal|pack|unpk|move\.[blw]|move)\b', Operator.Word),

            # Directivas de ensamblador
            (r'(?i)\b(even|cnop|macro|endm|rept|endr|section|text|data|bss|end|dc[bswl]?|dcb[bswl]?|set|equ|fequ|reg|xref|xdef|include|incbin|opt|machine|fpu|comment)\b', Keyword.Pseudo),

            # Símbolos / identificadores
            (r'\b([a-zA-Z_][a-zA-Z0-9_]*)\b', Name),

            # Espacios en blanco y puntuación
            (r'\s+', Text),
            (r'[%+-.,:()#]', Punctuation),
        ],
    }
