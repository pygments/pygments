"""
    C++ function highlighting tests.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import CppLexer
from pygments.token import Name


@pytest.mark.parametrize('return_type', [
    'std::vector<int>',
    'std::map<int, std::string>',
    'std::vector<std::vector<int>>',
    'const std::vector<int>&',
    'std::vector<int>*',
])
@pytest.mark.parametrize('ending', [';', ' { return {}; }'])
def test_template_return_function(return_type, ending):
    code = f'{return_type} make_values(){ending}'
    tokens = list(CppLexer().get_tokens(code))
    assert (Name.Function, 'make_values') in tokens
    assert ''.join(value for _, value in tokens) == code + '\n'


@pytest.mark.parametrize('code', [
    'auto result = a < b > get_value();',
    'a < b > get_value();',
    'get_value<int>();',
    'bool test() { return a < b > get_value(); }',
    'void test() { result = a < b > get_value(); }',
    'void test() { if (a < b) get_value(); }',
])
def test_comparison_call_is_not_a_function_definition(code):
    tokens = list(CppLexer().get_tokens(code))
    assert (Name.Function, 'get_value') not in tokens
    if 'test()' in code:
        assert (Name.Function, 'test') in tokens
