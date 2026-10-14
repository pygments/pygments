"""
    Go module lexer tests
    ~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import get_lexer_by_name, get_lexer_for_filename


@pytest.mark.parametrize('alias', ['gomod', 'go-mod'])
def test_gomod_alias(alias):
    assert get_lexer_by_name(alias).name == 'Go Module'


@pytest.mark.parametrize('filename', ['go.mod', '/project/go.mod'])
@pytest.mark.parametrize('code', [None, 'module example.com/project\n\ngo 1.26.0\n'])
def test_gomod_filename(filename, code):
    assert get_lexer_for_filename(filename, code).name == 'Go Module'
