"""
    Tests for pygments.lexers.lisp
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import time

import pytest

from pygments.lexers.lisp import CommonLispLexer, EmacsLispLexer


@pytest.mark.parametrize('lexer_cls', [CommonLispLexer, EmacsLispLexer])
def test_symbol_redos_resistance(lexer_cls):
    """Regression test for catastrophic backtracking in the symbol regex.

    ``nonmacro``'s "+-/" character range unintentionally lets "." through,
    which also matched via ``constituent``'s own explicit "[#.:]" - two ways
    to attribute each "." in a run to the same repeated group. When the
    enclosing "*symbol*" special-variable rule fails to find its closing
    "*", the engine retried every such split before giving up, which was
    quadratic in the run's length (2000 dots previously did not return in
    any reasonable time; with the fix this is well under a second).
    """
    attack = '*' + '.' * 2000 + '\n'
    t0 = time.time()
    list(lexer_cls().get_tokens(attack))
    elapsed = time.time() - t0
    assert elapsed < 5.0, f'Catastrophic backtracking: took {elapsed:.1f}s (should be < 5s)'


def test_symbol_starting_with_dot_still_lexes():
    """A symbol may legitimately start with "." (e.g. ``:#.foo``, exercised by
    tests/examplefiles/common-lisp/type.lisp) - the ReDoS fix must not remove
    "." from the character class that allows this, only the redundant second
    path that let a run of "." be re-partitioned.
    """
    tokens = list(CommonLispLexer().get_tokens(':#.foo'))
    assert not any('Error' in str(tok) for tok, _ in tokens)
