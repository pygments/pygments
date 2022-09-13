"""
    pygments.lexers.mips
    ~~~~~~~~~~~~~~~~~~~~
    Lexers for MIPS assembly.
    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer
from pygments.token import Whitespace, Comment, String, Keyword, Name, Text

__all__ = ["MIPSLexer"]


class MIPSLexer(RegexLexer):
    """
    A MIPS Assembly Lexer.

    Based on the Emacs major mode by hlissner:
    https://github.com/hlissner/emacs-mips-mode
    """

    name = 'MIPS'
    aliases = ['mips']
    filenames = ['*.asm', '*.mips']

    # Note: because these lists create regexes that are checked in order, any
    # element that is a substring of another must come after that other element.
    keywords = [
        # Arithmetic insturctions
        "subu", "subi", "sub", "addu", "addiu", "addi", "add",
        # Multiplication/division
        "multu", "mult", "mulu", "mul", "maddu", "madd", "msubu", "msub", "divu", "div",
        # Bitwise operations
        "nor", "xor", "andi", "and", "ori", "or", "xori", "clo", "clz",
        # Shifts
        "sllv", "sll", "srlv", "srl", "srav", "sra",
        # Comparisons
        "sltiu", "sltu", "slti", "slt",
        # Move data
        "mfhi", "mthi", "mflo", "mtlo", "movn", "movz", "movf", "movt",
        # Jump
        "jalr", "jal", "jr", "j",
        # branch
        "bc1f", "bc1t", "beq", "bgez", "bgezal", "bgtz", "blez", "bltzal", "bltz", "bne",
        # Load
        "lui", "lbu", "lb", "lhu", "lh", "lwcl", "lwl", "lwr", "lw",
        # Store
        "sb", "sh", "swl", "swr", "sw", # coproc: swc1 sdc1
        # Concurrent load/store
        "ll", "sc",
        # Trap handling
        "teqi", "teq", "tneqi", "tne", "tgeiu", "tgeu", "tgei", "tge", "tltiu", "tltu", "tlti", "tlt",
        # Exception / Interrupt
        "eret", "break", "bop", "syscall",
        #--- Floats -----------------------------------------------------
        # Arithmetic
        "add.s", "add.d", "sub.s", "sub.d", "mul.s", "mul.d", "div.s", "div.d", "neg.d",
        "neg.s",
        # Comparison
        "c.e.d", "c.e.s", "c.le.d", "c.le.s", "c.lt.s", "c.lt.d", # "c.gt.s", "c.gt.d",
        "madd.s", "madd.d", "msub.s", "msub.d",
        # Move Floats
        "mov.d", "move.s", "movf.d", "movf.s", "movt.d", "movt.s", "movn.d", "movn.s",
        "movnzd", "movz.s", "movz.d",
        # Conversion
        "cvt.d.s", "cvt.d.w", "cvt.s.d", "cvt.s.w", "cvt.w.d", "cvt.w.s", "trunc.w.d",
        "trunc.w.s",
        # Math
        "abs.s", "abs.d", "sqrt.s", "sqrt.d", "ceil.w.d", "ceil.w.s", "floor.w.d",
        "floor.w.s", "round.w.d", "round.w.s",
    ]

    pseudoinstructions = [
        # Arithmetic & logical
        "remu", "rem", "mulou", "mulo", "abs", "negu", "neg", "not", "rol", "ror",
        # branches
        "beqz", "bgeu", "bgtu", "bge", "bgt", "bleu", "ble", "bltu", "blt", "bnez", "b",
        # loads
        "la", "li", "ld", "ulhu", "ulh", "ulw",
        # Store
        "sd", "ush", "usw",
        # move
        "move", # coproc: "mfc1.d"
        # comparisons
        "sgtu", "sgt", "sgeu", "sge", "sleu", "sle", "sne", "seq",
        #--- Floats -----------------------------------------------------
        # load-store
        "l.d", "l.s", "s.d", "s.s",
    ]

    directives = [
        ".align", ".asciiz", ".ascii", ".byte", ".data", ".double", ".extern", ".float",
        ".globl", ".half", ".kdata", ".ktext", ".space", ".text", ".word",
    ]

    deprecated = [
        "beql", "bnel", "bgtzl", "bgezl", "bltzl", "blezl", "bltzall", "bgezall",
    ]

    @staticmethod
    def regexp_opt(l):
        """
        Creates a regexp that matches any string in list l by joining them
        via a pipe (|). Replaces . with \\. to avoid matching other
        characters.

        >>> regexp_opt(['abc', 'd.f', 'ghi'])
        'abc|d\\.f|ghi'
        """

        return "|".join([r"{}".format(s.replace('.', r'\.')) for s in l])

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'#.*?$', Comment),
            (r'"', String, 'string'),
            (r'-?[0-9]+?', Keyword.Constant),
            ("[a-zA-Z_0-9]*:", Name.Function),
            (regexp_opt(keywords), Keyword),
            (regexp_opt(pseudoinstructions), Name.Variable),
            (regexp_opt(deprecated), Keyword), # need warning face
            (r'[slm][ftwd]c[0-9](?:[.]d)?', Keyword),
            (r'\$(f?[0-2][0-9]|f?3[01]|[ft]?[0-9]|[vk][01]|a[0-3]|s[0-7]|[gsf]p|ra|at|zero)', Keyword.Type),
            (regexp_opt(directives), Name.Entity), # Preprocessor?
            (r':|,|;|{|}|=>|@|\$|=', Name.Builtin),
            (r'\w+', Text),
            (r'.', Text),
        ],
        'string': [
            (r'"', String, '#pop'),
            (r'[^\\"]+', String),
        ],
    }
