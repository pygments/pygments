# -*- coding: utf-8 -*-
"""
    Generated lexer tests
    ~~~~~~~~~~~~~~~~~~~~~

    Checks that lexers output the expected tokens for each sample
    under lexers/*/test_*.yaml.

    After making a change, rather than updating the samples manually,
    run `pytest --update-goldens tests/test_lexers.py`.

    To add a new sample, create a new file matching this pattern.
    The directory must match the alias of the lexer to be used.
    Populate only the `input: |` key, then just `--update-goldens`.

    :copyright: Copyright 2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import sys

import pytest

import pygments.lexers


def _prettyprint_tokens(tokens):
    for tok, val in tokens:
        yield '{!r:<13} {}'.format(val, str(tok)[6:])
        if val and val == '\n' * len(val):
            yield ''


@pytest.mark.skipif(sys.version_info < (3, 6), reason="requires python3.6 or higher")
@pytest.mark.golden_test("lexers/*/*.yaml")
def test_lexers(golden):
    lexer = pygments.lexers.get_lexer_by_name(golden.path.parent.name)
    tokens = lexer.get_tokens(golden["input"])
    assert '\n'.join(_prettyprint_tokens(tokens)) == golden.out["tokens"]
