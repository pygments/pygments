# -*- coding: utf-8 -*-
"""
    Typograhic Number Theory tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.tnt import TNTLexer
from pygments.token import Text, Comment, Operator, Keyword, Name, Number, \
     Punctuation, Error

@pytest.fixture(autouse=True)
def lexer():
    yield TNTLexer()

# whitespace

@pytest.mark.parametrize('text', ('  a', ' \t0', '\n\n 3'))
def test_whitespace_positive_matches(lexer, text):
    """Test fragments that should be tokenized as whitespace text."""
    assert lexer.whitespace(0, text) == len(text) - 1
    assert lexer.whitespace(0, text, True) == len(text) - 1
    assert lexer.cur[-1] == (0, Text, text[:-1])

@pytest.mark.parametrize('text', ('0 a=b premise', 'b=a symmetry'))
def test_whitespace_negative_matches(lexer, text):
    """Test statements that do not start with whitespace text."""
    assert lexer.whitespace(0, text) == 0
    with pytest.raises(AssertionError):
        lexer.whitespace(0, text, True)
    assert not lexer.cur

# terms that can go on either side of an = sign

@pytest.mark.parametrize('text', ('a ', "a' ", 'b ', "c' "))
def test_variable_positive_matches(lexer, text):
    """Test fragments that should be tokenized as variables."""
    assert lexer.variable(0, text) == len(text) - 1
    assert lexer.cur[-1] == (0, Name.Variable, text[:-1])

@pytest.mark.parametrize('text', ("' ", 'f ', "f' "))
def test_variable_negative_matches(lexer, text):
    """Test fragments that should **not** be tokenized as variables."""
    with pytest.raises(AssertionError):
        lexer.variable(0, text)
    assert not lexer.cur

@pytest.mark.parametrize('text', ('0', 'S0', 'SSSSS0'))
def test_numeral_positive_matches(lexer, text):
    """Test fragments that should be tokenized as (unary) numerals."""
    assert lexer.term(0, text) == len(text)
    assert lexer.cur[-1] == (len(text) - 1, Number.Integer, text[-1])
    if text != '0':
        assert lexer.cur[-2] == (0, Number.Integer, text[:-1])

@pytest.mark.parametrize('text', (
    '(a+b)', '(b.a)', '(c+d)'
))
def test_multiterm_positive_matches(lexer, text):
    """Test fragments that should be tokenized as a compound term."""
    assert lexer.term(0, text) == len(text)
    assert [t[1] for t in lexer.cur] == [
        Punctuation, Name.Variable, Operator,
        Name.Variable, Punctuation
    ]

@pytest.mark.parametrize('text', ('1', '=', 'A'))
def test_term_negative_matches(lexer, text):
    """Test fragments that should not be tokenized as terms at all."""
    with pytest.raises(AssertionError):
        lexer.term(0, text)
    assert not lexer.cur

# full statements, minus rule

@pytest.mark.parametrize('text', ('~a=b ', '~~~~a=b '))
def test_negator_positive_matches(lexer, text):
    """Test statements that start with a negation."""
    assert lexer.formula(0, text) == len(text) - 1
    assert lexer.cur[0] == (0, Operator, text[:-4])

@pytest.mark.parametrize('text', ('Aa:a=b ', 'Eb:a=b '))
def test_quantifier_positive_matches(lexer, text):
    """Test statements that start with a quantifier."""
    assert lexer.formula(0, text) == len(text) - 1
    assert lexer.cur[0][1] == Keyword.Declaration
    assert lexer.cur[1][1] == Name.Variable
    assert lexer.cur[2] == (2, Punctuation, ':')

@pytest.mark.parametrize('text', ('Aaa=b', 'Eba=b'))
def test_quantifier_negative_matches(lexer, text):
    """Test quantifiers that are only partially valid."""
    with pytest.raises(AssertionError):
        lexer.formula(0, text)
    # leftovers should still be valid
    assert lexer.cur[0][1] == Keyword.Declaration
    assert lexer.cur[1][1] == Name.Variable

@pytest.mark.parametrize('text', ('<a=b&b=a>', '<a=b|b=a>', '<a=b]b=a>'))
def test_compound_positive_matches(lexer, text):
    """Test statements that consist of multiple formulas compounded."""
    assert lexer.formula(0, text) == len(text)
    assert lexer.cur[0] == (0, Punctuation, '<')
    assert lexer.cur[4][1] == Operator
    assert lexer.cur[-1] == (len(text)-1, Punctuation, '>')

@pytest.mark.parametrize('text', ('<a=b/b=a>', '<a=b&b=a '))
def test_compound_negative_matches(lexer, text):
    """Test statements that look like compounds but are invalid."""
    with pytest.raises(AssertionError):
        lexer.formula(0, text)
    assert lexer.cur[0] == (0, Punctuation, '<')

@pytest.mark.parametrize('text', ('a=b ', 'a=0 ', '0=b '))
def test_formula_postive_matches(lexer, text):
    """Test the normal singular formula."""
    assert lexer.formula(0, text) == len(text) - 1
    assert lexer.cur[0][2] == text[0]
    assert lexer.cur[1] == (1, Operator, '=')
    assert lexer.cur[2][2] == text[2]

@pytest.mark.parametrize('text', ('a/b', '0+0 '))
def test_formula_negative_matches(lexer, text):
    """Test anything but an equals sign."""
    with pytest.raises(AssertionError):
        lexer.formula(0, text)

# rules themselves

@pytest.mark.parametrize('text', (
    'fantasy rule', 'carry over line 5', 'premise', 'joining',
    'double-tilde', 'switcheroo', 'De Morgan', 'specification'
))
def test_rule_positive_matches(lexer, text):
    """Test some valid rules of TNT."""
    assert lexer.rule(0, text) == len(text)
    assert lexer.cur[0][:2] == (0, Keyword)
    if text[-1].isdigit():
        assert lexer.cur[1][1] == Number.Integer

@pytest.mark.parametrize('text', (
    'fantasy', 'carry over', 'premse', 'unjoining',
    'triple-tilde', 'switcheru', 'De-Morgan', 'despecification'
))
def test_rule_negative_matches(lexer, text):
    """Test some invalid rules of TNT."""
    with pytest.raises(AssertionError):
        lexer.rule(0, text)

# referrals

@pytest.mark.parametrize('text', ('(lines 1, 2, and 4)', '(line 3,5,6)', '(lines 1, 6 and 0)'))
def test_lineno_positive_matches(lexer, text):
    """Test line referrals."""
    assert lexer.lineno(0, text) == len(text)
    assert lexer.cur[0] == (0, Punctuation, '(')
    assert lexer.cur[1][:2] == (1, Text)
    assert lexer.cur[2][1] == Number.Integer
    assert lexer.cur[3] == (len(text)-1, Punctuation, ')')

@pytest.mark.parametrize('text', (
    '(lines one, two, and four)1 ', # to avoid IndexError
    '(lines 1 2 and 3)', '(lines 1 2 3)'
))
def test_lineno_negative_matches(lexer, text):
    """Test invalid line referrals."""
    with pytest.raises(AssertionError):
        lexer.lineno(0, text)

# worst-case: error text

@pytest.mark.parametrize('text', ('asdf', 'fdsa\nasdf', 'asdf\n  '))
def test_error_till_line_end(lexer, text):
    try:
        nl = text.index('\n')
    except ValueError:
        nl = len(text)
    try:
        end = text.find(text.split(None, 2)[1])
    except IndexError: # split failed
        end = len(text)
    assert lexer.error_till_line_end(0, text) == end
    assert lexer.cur[0] == (0, Error, text[:nl])

# full statement, including rule (because this can't be tested any other way)

@pytest.mark.parametrize('text', ('[ push', '] pop'))
def test_fantasy_positive_matches(lexer, text):
    """Test statements that should be tokenized as push/pop statements."""
    assert lexer.get_tokens_unprocessed(text)[0] == (0, Keyword, text[0])

# full text is already done by examplefiles, but here's some exceptions

@pytest.mark.parametrize('text', (
    '0', 'a=b', 'premise',
    '0 a=b premise', '1 b=a symmetry (line 0)'
))
def test_no_crashing(lexer, text):
    """Test incomplete text fragments that shouldn't crash the whole lexer."""
    assert lexer.get_tokens(text)