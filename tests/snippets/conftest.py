"""
    Generated lexer tests
    ~~~~~~~~~~~~~~~~~~~~~

    Checks that lexers output the expected tokens for each sample
    under lexers/*/test_*.txt.

    After making a change, rather than updating the samples manually,
    run `pytest --update-goldens tests/lexers`.

    To add a new sample, create a new file matching this pattern.
    The directory must match the alias of the lexer to be used.
    Populate only the input, then just `--update-goldens`.

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pathlib
import pytest

from tests.conftest import LexerInlineTestItem


def pytest_collect_file(parent, path):
    if path.ext == '.txt':
        return LexerTestFile.from_parent(parent, path=pathlib.Path(path))


class LexerTestFile(pytest.File):
    def collect(self):
        yield LexerInlineTestItem.from_parent(self, name='')
