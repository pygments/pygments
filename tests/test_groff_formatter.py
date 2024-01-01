"""
    Pygments Groff formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments import highlight
from pygments.lexer import RegexLexer
from pygments.lexers import PythonLexer
from pygments.style import Style
from pygments.token import Token
from pygments.formatters import GroffFormatter


# FIXME: more thorough tests

def test_basic():
    code = """
for it in that:
    do(it)

import this
"""
    expected = r""".defcolor 000080 rgb #000080
.defcolor 0000FF rgb #0000FF
.defcolor 0044DD rgb #0044DD
.defcolor 008000 rgb #008000
.defcolor 008400 rgb #008400
.defcolor 19177C rgb #19177C
.defcolor 3D7B7B rgb #3D7B7B
.defcolor 666666 rgb #666666
.defcolor 687822 rgb #687822
.defcolor 717171 rgb #717171
.defcolor 767600 rgb #767600
.defcolor 800080 rgb #800080
.defcolor 880000 rgb #880000
.defcolor 9C6500 rgb #9C6500
.defcolor A00000 rgb #A00000
.defcolor A45A77 rgb #A45A77
.defcolor AA22FF rgb #AA22FF
.defcolor AA5D1F rgb #AA5D1F
.defcolor B00040 rgb #B00040
.defcolor BA2121 rgb #BA2121
.defcolor CB3F38 rgb #CB3F38
.defcolor E40000 rgb #E40000
.defcolor bbbbbb rgb #bbbbbb
.nf
\f[CR]
\m[008000]\f[CB]for\f[CR]\m[] it \m[AA22FF]\f[CB]in\f[CR]\m[] that:
    do(it)

\m[008000]\f[CB]import\f[CR]\m[] \m[0000FF]\f[CB]this\f[CR]\m[]

.fi"""
    assert highlight(code, PythonLexer(), GroffFormatter()) == expected


class ToyLexer(RegexLexer):
    tokens = {
        "root": [
            ("a", Token.Name),
            ("b", Token.Name.Custom),
        ],
    }

class ToyStyle(Style):
    styles = {
        Token.Name: "bold",
    }


def test_inheritance_custom_tokens():
    expected = r""".nf
\f[CR]
\f[CB]a\f[CR]\f[CB]b\f[CR]

.fi"""
    assert highlight("ab", ToyLexer(), GroffFormatter(style=ToyStyle)) == expected
