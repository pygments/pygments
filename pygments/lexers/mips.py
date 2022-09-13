"""
    pygments.lexers.mips
    ~~~~~~~~~~~~~~~~~~~~
    Lexers for MIPS assembly.
    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words
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
    keywords1 = [
        # branch: These need to be tested before pseudoinstructions to avoid
        # substring problem
        "bc1f", "bc1t", "bgezal", "bgez", "bgtz", "blez", "bltzal", "bltz",
    ]

    keywords2 = [
        # Arithmetic insturctions
        "subu", "subi", "sub", "addu", "addiu", "addi", "add",
        # Multiplication/division
        "multu", "mult", "mulu", "mul", "maddu", "madd", "msubu", "msub", "divu", "div",
        # Bitwise operations
        "andi", "and", "nor", "xori", "ori", "xor", "or", "clo", "clz",
        # Shifts
        "sllv", "sll", "srlv", "srl", "srav", "sra",
        # Comparisons
        "sltiu", "sltu", "slti", "slt",
        # branching
        "bne", "beq",
        # Move data
        "mfhi", "mthi", "mflo", "mtlo", "movn", "movz", "movf", "movt",
        # Jump
        "jalr", "jal", "jr", "j",
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
        "beqz", "bgeu", "bge", "bgtu", "bgt", "bleu", "ble", "bltu", "blt", "bnez", "b",
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

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'#.*', Comment),
            (r'"', String, 'string'),
            (r'-?[0-9]+?', Keyword.Constant),
            (r'\w*:', Name.Function),
            (words(deprecated, suffix=r'\b'), Keyword.Pseudo), # need warning face
            (words(pseudoinstructions, suffix=r'\b'), Name.Variable),
            (words(keywords, suffix=r'\b'), Keyword),
            (r'[slm][ftwd]c[0-9]([.]d)?', Keyword),
            (r'\$(f?[0-2][0-9]|f?3[01]|[ft]?[0-9]|[vk][01]|a[0-3]|s[0-7]|[gsf]p|ra|at|zero)', Keyword.Type),
            (words(directives, suffix=r'\b'), Name.Entity), # Preprocessor?
            (r':|,|;|\{|\}|=>|@|\$|=', Name.Builtin),
            (r'\w+', Text),
            (r'.', Text),
        ],
        'string': [
            (r'\\.', String.Escape),
            (r'"', String, '#pop'),
            (r'[^\\"]+', String),
        ],
    }
