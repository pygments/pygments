import pytest
from pygments.lexers import get_lexer_by_name
from pygments.token import Generic


def test_pytest_header_highlighting():
    lexer = get_lexer_by_name("pytest")
    tokens = list(lexer.get_tokens("==== FAILURES ===="))

    assert any(toktype is Generic.Heading for toktype, _ in tokens)


def test_pytest_captured_output_header():
    lexer = get_lexer_by_name("pytest")
    tokens = list(lexer.get_tokens("--- Captured stdout ---"))

    assert any(toktype is Generic.Subheading for toktype, _ in tokens)


def test_pytest_assertion_error_line():
    lexer = get_lexer_by_name("pytest")
    tokens = list(lexer.get_tokens("E   AssertionError: test message"))

    assert any(toktype is Generic.Error for toktype, _ in tokens)


def test_pytest_subtest_line():
    lexer = get_lexer_by_name("pytest")
    line = "FAILED [100%] tests/test_file.py::test_example[subtest-x]"
    tokens = list(lexer.get_tokens(line))

    assert any(toktype is Generic.Strong for toktype, _ in tokens)


def test_pytest_filename_traceback():
    lexer = get_lexer_by_name("pytest")
    tokens = list(lexer.get_tokens("tests/test_file.py:14: AssertionError"))

    assert any(toktype is Generic.Traceback for toktype, _ in tokens)