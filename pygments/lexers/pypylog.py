from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import (Text, Keyword, Number, Comment, Punctuation, Name,
    String, Literal)


__all__ = [
    "PyPyLogLexer",
]

class PyPyLogLexer(RegexLexer):
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
            (r"(\()([\w_]+)(\))", bygroups(Punctuation, Name.Builtin, Punctuation)),
            (r"[\[\]=,()]", Punctuation),
            (r"(\d+\.\d+|inf|-inf)", Number.Float),
            (r"-?\d+", Number.Integer),
            (r"'.*'", String),
            (r"(None|descr|ConstClass|ConstPtr)", Name),
            (r"<.*?>", Name.Builtin),
            (r"(debug_merge_point|jump|finish)", Name.Class),
            (r"(int_add_ovf|int_add|int_sub_ovf|int_sub|int_mul_ovf|int_mul|int_or|"
             r"int_ge|int_gt|int_lt|int_is_zero|int_is_true|"
             r"uint_floordiv|"
             r"float_add|float_sub|float_mul|float_truediv|"
             r"float_eq|float_ne|float_gt|"
             r"ptr_eq|"
             r"force_token|"
             r"call_may_force|call_assembler|call|"
             r"new_with_vtable|new_array|"
             r"arraylen_gc|"
             r"getarrayitem_gc|setarrayitem_gc|getarrayitem_raw|setarrayitem_raw|"
             r"getfield_gc_pure|getfield_gc|getfield_raw|setfield_gc|setfield_raw|"
             r"guard_true|guard_false|guard_value|guard_isnull|guard_nonnull_class|guard_class|guard_no_overflow|guard_not_forced|guard_no_exception)", Name.Builtin),
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
