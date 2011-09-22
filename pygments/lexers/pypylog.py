# -*- coding: utf-8 -*-
"""
    pygments.lexers.pypylog
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for pypy log files.

    :copyright: Copyright 2006-2011 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import Text, Keyword, Number, Comment, Punctuation, Name, \
    String


__all__ = ["PyPyLogLexer"]


class PyPyLogLexer(RegexLexer):
    """
    Lexer for PyPy log files.

    *New in Pygments 1.5.*
    """
    name = "PyPy Log"
    aliases = ["pypylog", "pypy"]
    filenames = ["*.pypylog"]
    mimetypes = ['application/x-pypylog']

    tokens = {
        "root": [
            (r"\[\w+\] {jit-log-.*?$", Keyword, "jit-log"),
            (r"\[\w+\] {jit-backend-counts$", Keyword, "jit-backend-counts"),
            include("extra-stuff"),
        ],
        "jit-log": [
            (r"\[\w+\] jit-log-.*?}$", Keyword, "#pop"),

            (r"[ifp]\d+", Name),
            (r"ptr\d+", Name),
            (r"(\()([\w_]+(?:\.[\w_]+)?)(\))",
             bygroups(Punctuation, Name.Builtin, Punctuation)),
            (r"[\[\]=,()]", Punctuation),
            (r"(\d+\.\d+|inf|-inf)", Number.Float),
            (r"-?\d+", Number.Integer),
            (r"'.*'", String),
            (r"(None|descr|ConstClass|ConstPtr)", Name),
            (r"<.*?>", Name.Builtin),
            (r"(debug_merge_point|jump|finish)", Name.Class),
            (r"(int_add_ovf|int_add|int_sub_ovf|int_sub|int_mul_ovf|int_mul|"
             r"int_floordiv|int_mod|int_lshift|int_rshift|int_and|int_or|"
             r"int_xor|int_eq|int_ne|int_ge|int_gt|int_le|int_lt|int_is_zero|"
             r"int_is_true|"
             r"uint_floordiv|uint_ge|uint_lt|"
             r"float_add|float_sub|float_mul|float_truediv|"
             r"float_eq|float_ne|float_ge|float_gt|float_le|float_lt|float_abs|"
             r"ptr_eq|"
             r"cast_int_to_float|cast_float_to_int|cast_opaque_ptr|"
             r"force_token|quasiimmut_field|same_as|virtual_ref_finish|virtual_ref|"
             r"call_may_force|call_assembler|call_loopinvariant|call_release_gil|call_pure|call|"
             r"new_with_vtable|new_array|newstr|newunicode|new|"
             r"arraylen_gc|"
             r"getarrayitem_gc_pure|getarrayitem_gc|setarrayitem_gc|"
             r"getarrayitem_raw|setarrayitem_raw|getfield_gc_pure|getfield_gc|"
             r"getfield_raw|setfield_gc|setfield_raw|"
             r"strgetitem|strsetitem|strlen|copystrcontent|"
             r"unicodegetitem|unicodesetitem|unicodelen|"
             r"guard_true|guard_false|guard_value|guard_isnull|"
             r"guard_nonnull_class|guard_nonnull|guard_class|guard_no_overflow|"
             r"guard_not_forced|guard_no_exception|guard_not_invalidated)",
             Name.Builtin),
            include("extra-stuff"),
        ],
        "jit-backend-counts": [
            (r"\[\w+\] jit-backend-counts}$", Keyword, "#pop"),
            (r"[:]", Punctuation),
            (r"\d+", Number),
            include("extra-stuff"),
        ],
        "extra-stuff": [
            (r"[\n\s]+", Text),
            (r"#.*?$", Comment),
        ],
    }
