"""
    Python lexer tests
    ~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import PythonLexer
from pygments.token import Keyword, Name, Operator, Text


@pytest.mark.parametrize(('prefix', 'alias_token'), [
    ('import operator', Name.Namespace),
    ('from math import operator', Name),
])
@pytest.mark.parametrize('before_as', [' ', '\t', ' \\\n    '])
@pytest.mark.parametrize('after_as', [' ', '\t', ' \\\n    '])
def test_import_alias(prefix, alias_token, before_as, after_as):
    source = f'{prefix}{before_as}as{after_as}op\n'
    tokens = list(PythonLexer().get_tokens(source))
    assert (Keyword, 'as') in tokens
    assert (alias_token, 'op') in tokens
    assert ''.join(value for _, value in tokens) == source


@pytest.mark.parametrize(('source', 'expected'), [
    ('import os \\\n    , sys as system\n', [
        (Keyword.Namespace, 'import'), (Name.Namespace, 'os'),
        (Operator, ','), (Name.Namespace, 'sys'), (Keyword, 'as'),
        (Name.Namespace, 'system'),
    ]),
    ('import package.\\\n    module as mod\n', [
        (Keyword.Namespace, 'import'), (Name.Namespace, 'package'),
        (Name.Namespace, '.'), (Name.Namespace, 'module'),
        (Keyword, 'as'), (Name.Namespace, 'mod'),
    ]),
    ('from package.\\\n    module import name\n', [
        (Keyword.Namespace, 'from'), (Name.Namespace, 'package'),
        (Name.Namespace, '.'), (Name.Namespace, 'module'),
        (Keyword.Namespace, 'import'), (Name, 'name'),
    ]),
    ('from package \\\n    import name\n', [
        (Keyword.Namespace, 'from'), (Name.Namespace, 'package'),
        (Keyword.Namespace, 'import'), (Name, 'name'),
    ]),
])
def test_import_continuation(source, expected):
    tokens = list(PythonLexer().get_tokens(source))
    assert [(token, value) for token, value in tokens if token not in Text] == expected
    assert ''.join(value for _, value in tokens) == source


@pytest.mark.parametrize('statement', [
    'import os', 'import os as', 'import os,', 'from os',
    'from os import path as',
])
@pytest.mark.parametrize('newline', ['\n', ' \n', '\t\n'])
def test_import_ends_at_newline(statement, newline):
    source = f'{statement}{newline}value = 1\n'
    tokens = list(PythonLexer().get_tokens(source))
    assert (Name, 'value') in tokens
    assert (Name.Namespace, 'value') not in tokens
    assert ''.join(value for _, value in tokens) == source


@pytest.mark.parametrize('keyword', ['as', 'import'])
@pytest.mark.parametrize('suffix', ['\u0301', '_module', 'ing'])
@pytest.mark.parametrize('package', ['', 'package.'])
@pytest.mark.parametrize('from_import', [False, True])
def test_import_keyword_prefix_in_module_name(keyword, suffix, package, from_import):
    module = keyword + suffix
    expected_module = [(Name.Namespace, module)]
    if package:
        expected_module[:0] = [(Name.Namespace, 'package'), (Name.Namespace, '.')]
    if from_import:
        source = f'from {package}{module} import value\n'
        expected = [(Keyword.Namespace, 'from'), *expected_module,
                    (Keyword.Namespace, 'import'), (Name, 'value')]
    else:
        source = f'import {package}{module}\n'
        expected = [(Keyword.Namespace, 'import'), *expected_module]
    compile(source, '<test>', 'exec')
    tokens = list(PythonLexer().get_tokens(source))
    assert [(token, value) for token, value in tokens if token not in Text] == expected
    assert ''.join(value for _, value in tokens) == source


@pytest.mark.parametrize(('source', 'expected'), [
    ('import os\f as alias\n', [
        (Keyword.Namespace, 'import'), (Name.Namespace, 'os'),
        (Keyword, 'as'), (Name.Namespace, 'alias'),
    ]),
    ('from os\f import path\n', [
        (Keyword.Namespace, 'from'), (Name.Namespace, 'os'),
        (Keyword.Namespace, 'import'), (Name, 'path'),
    ]),
])
def test_import_formfeed_whitespace(source, expected):
    compile(source, '<test>', 'exec')
    tokens = list(PythonLexer().get_tokens(source))
    assert [(token, value) for token, value in tokens if token not in Text] == expected
    assert ''.join(value for _, value in tokens) == source
