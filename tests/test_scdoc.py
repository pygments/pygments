"""Scdoc guessing requires its mandatory document header."""

import pytest

from pygments.lexers import guess_lexer
from pygments.lexers.scdoc import ScdocLexer


@pytest.mark.parametrize('text', [
    '',
    'some_underlined_text',
    '*bold* _underlined_',
    'tool(0)\n_underlined_',
    'tool(8)\n*bold*',
    'tool(1) trailing text\n_underlined_',
    '\ntool(1)\n_underlined_',
])
def test_scdoc_no_confidence_without_preamble(text):
    assert ScdocLexer.analyse_text(text) == 0


@pytest.mark.parametrize('header', [
    'tool(1)',
    'tool(7)',
    'tool(1) "left footer"',
    'tool(1) "left footer" "center header"',
])
@pytest.mark.parametrize('body, score', [
    ('', 0.5),
    ('\n*bold*', 0.51),
    ('\n_underlined_', 0.51),
    ('\n*bold* _underlined_', 0.52),
])
def test_scdoc_valid_preamble_confidence(header, body, score):
    assert ScdocLexer.analyse_text(header + body) == pytest.approx(score)
    assert isinstance(guess_lexer(header + body), ScdocLexer)


def test_python_underscore_is_not_scdoc():
    text = 'def my_func(param1, param2):\n  print(f"{param1} {param2}")'
    assert ScdocLexer.analyse_text(text) == 0
    assert not isinstance(guess_lexer(text), ScdocLexer)
