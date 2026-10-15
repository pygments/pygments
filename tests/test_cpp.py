"""C++ method recognition after constructor bodies."""

import pytest

from pygments.lexers.c_cpp import CLexer, CppLexer
from pygments.lexers.c_like import PikeLexer
from pygments.token import Error, Name, Punctuation


@pytest.mark.parametrize('constructor', [
    'point() {}',
    'explicit point(double x = 0, double y = 0) : x(x), y(y) {}',
    'point() { x = 1; }',
    'point() { if (true) { x = 1; } }',
    '~point() {}',
])
def test_method_after_constructor_body(constructor):
    source = ('struct point { double x, y; ' + constructor +
              ' int dim() const { return y == 0 ? x > 0 : y > 0; } };')
    tokens = list(CppLexer().get_tokens(source))
    assert (Name.Function, 'dim') in tokens
    assert ''.join(value for _, value in tokens) == source + '\n'
    assert not any(kind in Error for kind, _ in tokens)


@pytest.mark.parametrize('source', [
    'void f() { point value{}; value.dim(); }',
    'void f() { auto value = []() { return 1; }; value(); }',
    'struct point { int x; }; point value{};',
])
def test_expression_names_remain_names(source):
    tokens = list(CppLexer().get_tokens(source))
    assert (Name, 'value') in tokens
    assert (Name.Function, 'value') not in tokens
    assert ''.join(value for _, value in tokens) == source + '\n'


def test_c_function_recognition():
    tokens = list(CLexer().get_tokens('struct point { int x; }; int dim(void) { return 1; }'))
    assert (Name.Function, 'dim') in tokens


@pytest.mark.parametrize('label', ['"string"', 'STRING_TYPE'])
def test_pike_case_after_constructor(label):
    source = ('create() {} mixed cast(string to) { switch(to) { case ' +
              label + ': return 1; } }')
    tokens = list(PikeLexer().get_tokens(source))
    assert (Name.Function, 'cast') in tokens
    assert (Punctuation, ':') in tokens
    if label == 'STRING_TYPE':
        assert (Name.Constant, label) in tokens


@pytest.mark.parametrize('source', [
    'struct point { int x; }; int after() { point value{1}; return value.x; }',
    'struct point { point() {} }; int after() { auto value = [] { return 1; }; return value(); }',
    'struct point { point() { if (true) {} } }; int after() { return 1; }',
    '#define MAKE(x) { x; }\nstruct point { point() {} }; int after() { MAKE(1) return 1; }',
])
def test_function_after_class_body(source):
    tokens = list(CppLexer().get_tokens(source))
    assert (Name.Function, 'after') in tokens
    assert ''.join(value for _, value in tokens) == source + '\n'
    assert not any(kind in Error for kind, _ in tokens)
