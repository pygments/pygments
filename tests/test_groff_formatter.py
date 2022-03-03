"""
    Pygments Groff formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments import highlight
from pygments.lexer import RegexLexer
from pygments.style import Style
from pygments.token import Token
from pygments.formatters import GroffFormatter


# FIXME: this tests a bug fix, but the basic functionality
# is not tested thoroughly yet.

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


expected = r""".nf
\f[CR]
\f[CB]a\f[CR]\f[CB]b\f[CR]

.fi"""

def test_inheritance_custom_tokens():
    assert highlight("ab", ToyLexer(), GroffFormatter(style=ToyStyle)) == expected
