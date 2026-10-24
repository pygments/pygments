"""C# method-declaration token regression tests.

    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import CSharpLexer
from pygments.token import Name


@pytest.mark.parametrize('level', ['none', 'basic', 'full'])
@pytest.mark.parametrize('return_type', [
    'GenericResult',
    'GenericResult<bool>',
    'GenericResult<MeLiBu.IMeLiBuFrame>',
    'Dictionary<string, List<int>>',
    'Task<Result<T>>[]',
    'System.Collections.Generic.List<int>',
])
def test_generic_return_type_method_name(level, return_type):
    source = f'public static {return_type} Connect(IPAddress ip);\n'
    tokens = list(CSharpLexer(unicodelevel=level).get_tokens(source))
    assert ''.join(value for _, value in tokens) == source
    assert [value for token, value in tokens if token is Name.Function] == ['Connect']
