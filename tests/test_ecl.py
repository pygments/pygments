import pytest

from pygments.lexers.ecl import ECLLexer

@pytest.fixture(scope="module")
def lexer():
    yield ECLLexer()

def test_ecl_analyze_text(lexer):
    text = r"""
            STRING  ABC -> size32_t lenAbc, const char * abc;
            """
    res = lexer.analyse_text(text)
    assert res == 0.01