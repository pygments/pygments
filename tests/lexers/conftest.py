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

    :copyright: Copyright 2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
from pathlib import Path

import pytest

import pygments.lexers


def pytest_collect_file(parent, path):
    if path.ext == '.txt':
        return LexerTestFile.from_parent(parent, fspath=path)


class LexerTestFile(pytest.File):
    def collect(self):
        yield LexerTestItem.from_parent(self, name='test')


class LexerTestItem(pytest.Item):
    def __init__(self, name, parent):
        super().__init__(name, parent)
        self.lexer = Path(str(self.fspath)).parent.name

        content = self.fspath.read_text('utf-8')
        content, _, self.expected = content.partition('\n---tokens---\n')
        if content.startswith('---input---\n'):
            content = '\n' + content
        self.comment, _, self.input = content.rpartition('\n---input---\n')
        if not self.input.endswith('\n'):
            self.input += '\n'
        self.comment = self.comment.strip()

    @classmethod
    def _prettyprint_tokens(cls, tokens):
        for tok, val in tokens:
            yield '{!r:<13} {}'.format(val, str(tok)[6:])
            if val and val == '\n' * len(val):
                yield ''

    def runtest(self):
        lexer = pygments.lexers.get_lexer_by_name(self.lexer)
        tokens = lexer.get_tokens(self.input)
        self.actual = '\n'.join(self._prettyprint_tokens(tokens)).rstrip('\n') + '\n'
        if not self.config.getoption('--update-goldens'):
            assert self.actual == self.expected

    def _test_file_rel_path(self):
        return Path(self.fspath).relative_to(Path(__file__).parent.parent.parent)

    def repr_failure(self, excinfo):
        if isinstance(excinfo.value, AssertionError):
            message = (
                'The tokens produced by the "{}" lexer differ from the '
                'expected ones in the file "{}".\n'
                'Run `pytest tests/lexers --update-goldens` to update it.'
            ).format(self.lexer, self._test_file_rel_path())
            diff = str(excinfo.value).split('\n', 1)[-1]
            return message + '\n\n' + diff

    def reportinfo(self):
        return self.fspath, None, str(self._test_file_rel_path())


def pytest_runtest_teardown(item, nextitem):
    if item.config.getoption('--update-goldens') and isinstance(item, LexerTestItem):
        with item.fspath.open('w', encoding='utf-8') as f:
            f.write(item.comment)
            if item.comment:
                f.write('\n\n')
            f.write('---input---\n')
            f.write(item.input)
            f.write('\n---tokens---\n')
            f.write(item.actual)
