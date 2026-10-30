"""
    Tests for YAML plain scalar token categorization (issue #2149).
"""

import pytest

from pygments import highlight
from pygments.formatters import HtmlFormatter, TerminalFormatter
from pygments.lexers.data import YamlLexer
from pygments.styles import get_style_by_name
from pygments.token import Comment, Name, Punctuation, String, Whitespace


@pytest.fixture(scope="module")
def lexer():
    return YamlLexer()


def test_yaml_block_plain_scalar_tokens(lexer):
    code = (
        "key: plain value\n"
        "multi: hello world\n"
        "list:\n"
        "  - item one\n"
        "  - item two\n"
        "nested:\n"
        "  child: nested plain value\n"
        "multiline: first line\n"
        "  second line\n"
    )
    tokens = list(lexer.get_tokens(code))

    # Reconstruct text perfectly
    assert "".join(val for _, val in tokens) == code

    # Check key: plain value
    assert tokens[0] == (Name.Tag, "key")
    assert tokens[1] == (Punctuation, ":")
    assert tokens[2] == (Whitespace, " ")
    assert tokens[3] == (String, "plain")
    assert tokens[4] == (String, " ")
    assert tokens[5] == (String, "value")
    assert tokens[6] == (Whitespace, "\n")

    # Check list items
    list_item_tokens = [t for t in tokens if t[1] in ("item", "one", "two")]
    assert list_item_tokens == [
        (String, "item"), (String, "one"),
        (String, "item"), (String, "two"),
    ]

    # Check multiline plain scalar
    multiline_tokens = [t for t in tokens if t[1] in ("first", "second", "line")]
    assert multiline_tokens == [
        (String, "first"), (String, "line"),
        (String, "second"), (String, "line"),
    ]


def test_yaml_controls_preserved(lexer):
    code = (
        "# Top comment\n"
        "mapping:\n"
        "  quoted_double: \"double quoted\"\n"
        "  quoted_single: 'single quoted'\n"
        "  flow_sequence: [plain_flow, another_flow]\n"
        "  literal_block: |\n"
        "    literal text\n"
    )
    tokens = list(lexer.get_tokens(code))

    # Perfect reconstruction
    assert "".join(val for _, val in tokens) == code

    # Comments are Comment
    comments = [val for tok, val in tokens if tok in Comment]
    assert "# Top comment" in comments

    # Keys are Name.Tag
    tag_names = [val for tok, val in tokens if tok == Name.Tag]
    assert "mapping" in tag_names
    assert "quoted_double" in tag_names
    assert "quoted_single" in tag_names
    assert "flow_sequence" in tag_names
    assert "literal_block" in tag_names

    # Flow sequence elements remain Name.Variable (not block String)
    flow_vars = [val for tok, val in tokens if tok == Name.Variable]
    assert "plain_flow" in flow_vars
    assert "another_flow" in flow_vars

    # Quoted strings contain String tokens
    quoted_tokens = [t for t in tokens if t[1] in ("double", "single", "quoted")]
    assert quoted_tokens == [
        (String, "double"), (String, "quoted"),
        (String, "single"), (String, "quoted"),
    ]
    assert (Name.Constant, "literal text") in tokens


def test_yaml_html_and_terminal_styling(lexer):
    code = "title: My Article Title\n"
    tokens = list(lexer.get_tokens(code))

    # The scalar tokens should be String
    scalar_tokens = [tok for tok, val in tokens if val in ("My", "Article", "Title")]
    assert scalar_tokens == [String, String, String]

    # In default style, String has a color
    default_style = get_style_by_name("default")
    assert default_style.style_for_token(String)["color"] is not None

    # Formatted HTML applies standard string CSS class (s), not unstyled l-Scalar-Plain
    html = highlight(code, lexer, HtmlFormatter(style="default"))
    assert "l-Scalar-Plain" not in html
    assert '<span class="s">My Article Title</span>' in html

    # Terminal formatter applies ansi color to the string tokens
    term = highlight(code, lexer, TerminalFormatter(style="default"))
    assert "\x1b[33mMy\x1b[39;49;00m" in term
    assert "\x1b[33mArticle\x1b[39;49;00m" in term
