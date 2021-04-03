import bz2
import gzip

from pygments import highlight
from pygments.formatters import HtmlFormatter, RawTokenFormatter
from pygments.lexers import PythonLexer, RawTokenLexer


def test_raw_token():
    code = "2 + α"
    raw = highlight(code, PythonLexer(), RawTokenFormatter())
    html = highlight(code, PythonLexer(), HtmlFormatter())

    assert highlight(raw, RawTokenLexer(), RawTokenFormatter()) == raw
    assert highlight(raw, RawTokenLexer(), HtmlFormatter()) == html
    assert highlight(raw.decode(), RawTokenLexer(), HtmlFormatter()) == html

    raw_gz = highlight(code, PythonLexer(), RawTokenFormatter(compress="gz"))
    assert gzip.decompress(raw_gz) == raw
    assert highlight(raw_gz, RawTokenLexer(compress="gz"), RawTokenFormatter()) == raw
    assert (
        highlight(
            raw_gz.decode("latin1"), RawTokenLexer(compress="gz"), RawTokenFormatter()
        )
        == raw
    )

    raw_bz2 = highlight(code, PythonLexer(), RawTokenFormatter(compress="bz2"))
    assert bz2.decompress(raw_bz2) == raw
    assert highlight(raw_bz2, RawTokenLexer(compress="bz2"), RawTokenFormatter()) == raw
    assert (
        highlight(
            raw_bz2.decode("latin1"), RawTokenLexer(compress="bz2"), RawTokenFormatter()
        )
        == raw
    )


def test_invalid_raw_token():
    # These should not throw exceptions.
    assert (
        highlight("Tolkien", RawTokenLexer(), RawTokenFormatter())
        == b"Token.Error\t'Tolkien\\n'\n"
    )
    assert (
        highlight("Tolkien\t'x'", RawTokenLexer(), RawTokenFormatter())
        == b"Token\t'x'\n"
    )
    assert (
        highlight("Token.Text\t42", RawTokenLexer(), RawTokenFormatter())
        == b"Token.Error\t'Token.Text\\t42\\n'\n"
    )
    assert (
        highlight("Token.Text\t'", RawTokenLexer(), RawTokenFormatter())
        == b'Token.Error\t"Token.Text\\t\'\\n"\n'
    )
    assert (
        highlight("Token.Text\t'α'", RawTokenLexer(), RawTokenFormatter())
        == b"Token.Text\t'\\u03b1'\n"
    )
    assert (
        highlight("Token.Text\tu'α'", RawTokenLexer(), RawTokenFormatter())
        == b"Token.Text\t'\\u03b1'\n"
    )
    assert (
        highlight(b"Token.Text\t'\xff'", RawTokenLexer(), RawTokenFormatter())
        == b"Token.Text\t'\\xff'\n"
    )
