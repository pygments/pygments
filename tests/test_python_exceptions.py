import pytest

from pygments.lexers.python import PythonLexer
from pygments.token import Name


@pytest.mark.parametrize('name', [
    'BaseExceptionGroup', 'ExceptionGroup', 'PythonFinalizationError',
])
def test_recent_builtin_exceptions(name):
    assert list(PythonLexer().get_tokens_unprocessed(name)) == [
        (0, Name.Exception, name),
    ]


@pytest.mark.parametrize('name', [
    'ExceptionGroupFactory', 'MyExceptionGroup', 'PythonFinalizationErrorExtra',
])
def test_exception_name_boundaries(name):
    assert list(PythonLexer().get_tokens_unprocessed(name)) == [(0, Name, name)]


@pytest.mark.parametrize('name', ['ExceptionGroup', 'PythonFinalizationError'])
def test_qualified_names_are_not_builtin_exceptions(name):
    tokens = list(PythonLexer().get_tokens_unprocessed('custom.' + name))
    assert tokens[-1][1:] == (Name, name)
