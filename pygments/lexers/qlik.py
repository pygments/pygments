"""
    pygments.lexers.qlik
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for the qlik scripting language

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups
from pygments.token import *

__all__ = ["QlikLexer"]


class QlikLexer(RegexLexer):
    """
    Lexer for qlik code, including .qvs files
    """

    name = "Qlik"
    aliases = ["qlik", "qlikview", "qliksense", "qlikscript"]
    filenames = ["*.qvs", "*.qvw"]

    flags = re.IGNORECASE

    tokens = {
        "comment": [
            (r"[^*/]", Comment.Multiline),
            (r"/\*", Comment.Multiline, "#push"),
            (r"\*/", Comment.Multiline, "#pop"),
            (r"[*/]", Comment.Multiline),
        ],
        "numerics": [
            (r"\b[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?\b", Number.Float),
            (r"\b[0-9]+\b", Number.Integer),
        ],
        "variables": [
            (r"(\$\()(\w+)(\))", bygroups(Keyword, Name.Variable, Keyword)),
        ],
        "root": [
            (r"\s+", Text),
            (r"/\*", Comment.Multiline, "comment"),
            (r"//.*\n", Comment.Singleline),
            (
                r"\b(and|or|is|not)\b",
                Operator.Word,
            ),
            (
                r"(for|in|while|do|break|return|continue|switch|case|default|"
                r"if|else|endif|then|end|errormode|each|next)\b",
                Keyword,
            ),
            (r"(set|let|sub)\b", Keyword.Declaration),
            (r"(num|date|dual|date#|text)\(", Name.Builtin),
            (r"[a-z]\w*:", Keyword.Declaration),
            (r"(?<![.$])(true|false|null)\b", Keyword.Constant),
            (
                r"(\bas|resident|mapping|distinct|load|join|(left|right|outer|full|"
                r"inner) join|from|trace|execute|odbc|connect|add|"
                r"alias|binary|buffer|bundle|concatenate|directory|"
                r"intervalmatch|trace|unqualify|qualify|include|sql\sselect|"
                r"inline|autogenerate|group\sby|order\sby|asc|desc|store|into|"
                r"drop|table|field)\b",
                Operator.Word,
            ),
            (r"(noconcatenate|concatenate)\b", Operator.Word),
            (
                r"(ceil|floor|round|rangesum|rangeavg|rangestdev|len|trim|"
                r"subfield|left|right|replace|exists|fieldindex|fieldvalue|"
                r"peek|previous|next|lookup|recno|rowno|iterno|autonumberhash128|"
                r"autonumberhash256|fieldvaluecount|sum|max|maxstring|min|"
                r"avg|count|second|minute|hour|day|week|month|year|weekyear|"
                r"weekday|now|today|localtime|makedate|makeweekdate|maketime|"
                r"yeartodate|setdateyear|setdateyearmonth|yearstart|yearend|"
                r"inyear|inyeartodate|inquarter|inquartertodate|addmonths|"
                r"monthstart|monthend|inmonth|inmonthtodate|inmonths|"
                r"inmonthstodate|inweek|inweektodate|inlunarweek|"
                r"inlunarweektodate|timezone|gmt|utc|daylightsaving|filesize|"
                r"documentname|filetime|isnull|crosstable|applymap|if|"
                r"filename)\(",
                Operator.Word,
            ),
            include("variables"),
            (r'"(\s|\w)+"', Keyword),
            (
                r"(\[.+)(\$\()(\w+)(\))(.+])",
                bygroups(String, Keyword, Name.Variable, Keyword, String),
            ),
            (r"\[[^\]]+\]", Keyword),
            include("numerics"),
            (
                r"('.+)(\$\()(\w+)(\))(.+')",
                bygroups(String, Keyword, Name.Variable, Keyword, String),
            ),
            (r"<>|[\-\<\>\+\*\%\&\|\^\/=\)\(]", Operator),
            (r"'.+'", String),
            (r"\b\w+\b", Text),
            (r"(\,|\;|\.|\(|\)|\\|\/)", Punctuation),
        ],
    }
