"""
    Generated lexer tests
    ~~~~~~~~~~~~~~~~~~~~~

    Checks that lexers output the expected tokens for each sample
    under lexers/*/test_*.yaml.

    After making a change, rather than updating the samples manually,
    run `pytest --update-goldens tests/lexers`.

    To add a new sample, create a new file matching this pattern.
    The directory must match the alias of the lexer to be used.
    Populate only the input, then just `--update-goldens`.

    :copyright: Copyright 2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
from pathlib import Path
import textwrap

import pytest

import pygments.lexers


def pytest_collect_file(parent, path):
    if path.ext == '.yaml':
        return LexerTestFile.from_parent(parent, fspath=path)


class LexerTestFile(pytest.File):
    def collect(self):
        lexer = Path(str(self.fspath)).parent.name
        yield LexerTestItem.from_parent(self, name='test', file=self.fspath, lexer=lexer)


class LexerTestItem(pytest.Item):
    def __init__(self, name, parent, file, lexer):
        super().__init__(name, parent)
        self.file = file
        self.lexer = lexer

        input_lines = []
        input_newline = ''
        expected_lines = []
        comment_lines = []
        current = None

        with file.open(encoding='utf-8') as f:
            for line in f:
                line = line.rstrip('\n')
                assert (
                    line in ('input: |', 'input: |-', 'tokens: |') or line.startswith('#') or
                    current is not None and (line.strip() == '' or line.startswith('  '))
                )
                if line.startswith('input'):
                    current = input_lines
                    if '-' not in line:
                        input_newline = '\n'
                elif line.startswith('tokens'):
                    current = expected_lines
                elif line.startswith('#'):
                    comment_lines.append(line)
                else:
                    current.append(line[2:])

        self.input = '\n'.join(input_lines).rstrip('\n') + input_newline
        self.expected = ''.join(line + '\n' for line in expected_lines)
        self.comment = ''.join(line + '\n' for line in comment_lines)

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


def pytest_runtest_teardown(item, nextitem):
    if item.config.getoption('--update-goldens') and isinstance(item, LexerTestItem):
        with item.file.open('w', encoding='utf-8') as f:
            f.write(item.comment)
            if item.input.endswith('\n'):
                f.write('input: |\n')
                f.write(textwrap.indent(item.input, '  '))
            else:
                f.write('input: |-\n')
                f.write(textwrap.indent(item.input, '  ') + '\n')
            f.write('tokens: |\n')
            f.write(textwrap.indent(item.actual, '  '))
