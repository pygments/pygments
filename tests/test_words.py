"""
    Pygments tests for words()
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, words
from pygments.token import Token


class MyLexer(RegexLexer):
    tokens = {
        "root": [
            (
                words(
                    [
                        "a-word",
                        "another-word",
                        # Test proper escaping of a few things that can occur
                        # in regular expressions. They are all matched literally.
                        "[",
                        "]",
                        "^",
                        "\\",
                        "(",
                        ")",
                        "(?:",
                        "-",
                        "|",
                        r"\w",
                    ]
                ),
                Token.Name,
            ),
            (words(["space-allowed-before-this"], prefix=" ?"), Token.Name),
            (words(["space-allowed-after-this"], suffix=" ?"), Token.Name),
            (
                words(["space-required-before-and-after-this"], prefix=" ", suffix=" "),
                Token.Name,
            ),
            # prefix and suffix can be regexes.
            (words(["one-whitespace-allowed-before-this"], prefix=r"\s?"), Token.Name),
            (words(["all-whitespace-allowed-after-this"], suffix=r"\s*"), Token.Name),
            (
                words(
                    ["all-whitespace-allowed-one-required-after-this"], suffix=r"\s+"
                ),
                Token.Name,
            ),
            (r"\n", Token.Text),
        ],
    }


def test_basic():
    s = "a-word this-is-not-in-the-list another-word"
    assert list(MyLexer().get_tokens(s)) == [
        (Token.Name, "a-word"),
        (Token.Error, " "),
        (Token.Error, "t"),
        (Token.Error, "h"),
        (Token.Error, "i"),
        (Token.Error, "s"),
        (Token.Name, "-"),
        (Token.Error, "i"),
        (Token.Error, "s"),
        (Token.Name, "-"),
        (Token.Error, "n"),
        (Token.Error, "o"),
        (Token.Error, "t"),
        (Token.Name, "-"),
        (Token.Error, "i"),
        (Token.Error, "n"),
        (Token.Name, "-"),
        (Token.Error, "t"),
        (Token.Error, "h"),
        (Token.Error, "e"),
        (Token.Name, "-"),
        (Token.Error, "l"),
        (Token.Error, "i"),
        (Token.Error, "s"),
        (Token.Error, "t"),
        (Token.Error, " "),
        (Token.Name, "another-word"),
        (Token.Text, "\n"),
    ]


def test_special_characters():
    s = """
[
]
^
\\
(
)
(?:
-
|
\\w
"""
    assert list(MyLexer().get_tokens(s)) == [
        (Token.Name, "["),
        (Token.Text, "\n"),
        (Token.Name, "]"),
        (Token.Text, "\n"),
        (Token.Name, "^"),
        (Token.Text, "\n"),
        (Token.Name, "\\"),
        (Token.Text, "\n"),
        (Token.Name, "("),
        (Token.Text, "\n"),
        (Token.Name, ")"),
        (Token.Text, "\n"),
        (Token.Name, "(?:"),
        (Token.Text, "\n"),
        (Token.Name, "-"),
        (Token.Text, "\n"),
        (Token.Name, "|"),
        (Token.Text, "\n"),
        (Token.Name, "\\w"),
        (Token.Text, "\n"),
    ]


def test_affixes():
    s = """
space-allowed-after-this |
space-allowed-before-this
space-allowed-after-this
 space-required-before-and-after-this |
space-required-before-and-after-this |
 space-required-before-and-after-this<= no space after
"""
    assert list(MyLexer().get_tokens(s)) == [
        (Token.Name, "space-allowed-after-this "),
        (Token.Name, "|"),
        (Token.Text, "\n"),
        (Token.Name, "space-allowed-before-this"),
        (Token.Text, "\n"),
        (Token.Name, "space-allowed-after-this"),
        (Token.Text, "\n"),
        (Token.Name, " space-required-before-and-after-this "),
        (Token.Name, "|"),
        (Token.Text, "\n"),
        (Token.Error, "s"),
        (Token.Error, "p"),
        (Token.Error, "a"),
        (Token.Error, "c"),
        (Token.Error, "e"),
        (Token.Name, "-"),
        (Token.Error, "r"),
        (Token.Error, "e"),
        (Token.Error, "q"),
        (Token.Error, "u"),
        (Token.Error, "i"),
        (Token.Error, "r"),
        (Token.Error, "e"),
        (Token.Error, "d"),
        (Token.Name, "-"),
        (Token.Error, "b"),
        (Token.Error, "e"),
        (Token.Error, "f"),
        (Token.Error, "o"),
        (Token.Error, "r"),
        (Token.Error, "e"),
        (Token.Name, "-"),
        (Token.Error, "a"),
        (Token.Error, "n"),
        (Token.Error, "d"),
        (Token.Name, "-"),
        (Token.Error, "a"),
        (Token.Error, "f"),
        (Token.Error, "t"),
        (Token.Error, "e"),
        (Token.Error, "r"),
        (Token.Name, "-"),
        (Token.Error, "t"),
        (Token.Error, "h"),
        (Token.Error, "i"),
        (Token.Error, "s"),
        (Token.Error, " "),
        (Token.Name, "|"),
        (Token.Text, "\n"),
        (Token.Error, " "),
        (Token.Error, "s"),
        (Token.Error, "p"),
        (Token.Error, "a"),
        (Token.Error, "c"),
        (Token.Error, "e"),
        (Token.Name, "-"),
        (Token.Error, "r"),
        (Token.Error, "e"),
        (Token.Error, "q"),
        (Token.Error, "u"),
        (Token.Error, "i"),
        (Token.Error, "r"),
        (Token.Error, "e"),
        (Token.Error, "d"),
        (Token.Name, "-"),
        (Token.Error, "b"),
        (Token.Error, "e"),
        (Token.Error, "f"),
        (Token.Error, "o"),
        (Token.Error, "r"),
        (Token.Error, "e"),
        (Token.Name, "-"),
        (Token.Error, "a"),
        (Token.Error, "n"),
        (Token.Error, "d"),
        (Token.Name, "-"),
        (Token.Error, "a"),
        (Token.Error, "f"),
        (Token.Error, "t"),
        (Token.Error, "e"),
        (Token.Error, "r"),
        (Token.Name, "-"),
        (Token.Error, "t"),
        (Token.Error, "h"),
        (Token.Error, "i"),
        (Token.Error, "s"),
        (Token.Error, "<"),
        (Token.Error, "="),
        (Token.Error, " "),
        (Token.Error, "n"),
        (Token.Error, "o"),
        (Token.Error, " "),
        (Token.Error, "s"),
        (Token.Error, "p"),
        (Token.Error, "a"),
        (Token.Error, "c"),
        (Token.Error, "e"),
        (Token.Error, " "),
        (Token.Error, "a"),
        (Token.Error, "f"),
        (Token.Error, "t"),
        (Token.Error, "e"),
        (Token.Error, "r"),
        (Token.Text, "\n"),
    ]


def test_affixes_regexes():
    s = """
 one-whitespace-allowed-before-this
NOT-WHITESPACEone-whitespace-allowed-before-this
all-whitespace-allowed-after-this \n \t
all-whitespace-allowed-after-thisNOT-WHITESPACE
all-whitespace-allowed-one-required-after-thisNOT-WHITESPACE"""
    assert list(MyLexer().get_tokens(s)) == [
        (Token.Name, " one-whitespace-allowed-before-this"),
        (Token.Text, "\n"),
        (Token.Error, "N"),
        (Token.Error, "O"),
        (Token.Error, "T"),
        (Token.Name, "-"),
        (Token.Error, "W"),
        (Token.Error, "H"),
        (Token.Error, "I"),
        (Token.Error, "T"),
        (Token.Error, "E"),
        (Token.Error, "S"),
        (Token.Error, "P"),
        (Token.Error, "A"),
        (Token.Error, "C"),
        (Token.Error, "E"),
        (Token.Name, "one-whitespace-allowed-before-this"),
        (Token.Text, "\n"),
        (Token.Name, "all-whitespace-allowed-after-this \n \t\n"),
        (Token.Name, "all-whitespace-allowed-after-this"),
        (Token.Error, "N"),
        (Token.Error, "O"),
        (Token.Error, "T"),
        (Token.Name, "-"),
        (Token.Error, "W"),
        (Token.Error, "H"),
        (Token.Error, "I"),
        (Token.Error, "T"),
        (Token.Error, "E"),
        (Token.Error, "S"),
        (Token.Error, "P"),
        (Token.Error, "A"),
        (Token.Error, "C"),
        (Token.Error, "E"),
        (Token.Text, "\n"),
        (Token.Error, "a"),
        (Token.Error, "l"),
        (Token.Error, "l"),
        (Token.Name, "-"),
        (Token.Error, "w"),
        (Token.Error, "h"),
        (Token.Error, "i"),
        (Token.Error, "t"),
        (Token.Error, "e"),
        (Token.Error, "s"),
        (Token.Error, "p"),
        (Token.Error, "a"),
        (Token.Error, "c"),
        (Token.Error, "e"),
        (Token.Name, "-"),
        (Token.Error, "a"),
        (Token.Error, "l"),
        (Token.Error, "l"),
        (Token.Error, "o"),
        (Token.Error, "w"),
        (Token.Error, "e"),
        (Token.Error, "d"),
        (Token.Name, "-"),
        (Token.Error, "o"),
        (Token.Error, "n"),
        (Token.Error, "e"),
        (Token.Name, "-"),
        (Token.Error, "r"),
        (Token.Error, "e"),
        (Token.Error, "q"),
        (Token.Error, "u"),
        (Token.Error, "i"),
        (Token.Error, "r"),
        (Token.Error, "e"),
        (Token.Error, "d"),
        (Token.Name, "-"),
        (Token.Error, "a"),
        (Token.Error, "f"),
        (Token.Error, "t"),
        (Token.Error, "e"),
        (Token.Error, "r"),
        (Token.Name, "-"),
        (Token.Error, "t"),
        (Token.Error, "h"),
        (Token.Error, "i"),
        (Token.Error, "s"),
        (Token.Error, "N"),
        (Token.Error, "O"),
        (Token.Error, "T"),
        (Token.Name, "-"),
        (Token.Error, "W"),
        (Token.Error, "H"),
        (Token.Error, "I"),
        (Token.Error, "T"),
        (Token.Error, "E"),
        (Token.Error, "S"),
        (Token.Error, "P"),
        (Token.Error, "A"),
        (Token.Error, "C"),
        (Token.Error, "E"),
        (Token.Text, "\n"),
    ]


class MySecondLexer(RegexLexer):
    tokens = {
        "root": [
            (words(["[", "x"]), Token.Name),
        ],
    }


def test_bracket_escape():
    # This used to emit a FutureWarning.
    assert list(MySecondLexer().get_tokens("x")) == [
        (Token.Name, "x"),
        (Token.Text.Whitespace, "\n"),
    ]
