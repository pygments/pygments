"""
    Lisp lexer tests
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import time

import pytest

from pygments import lex
from pygments.lexers.lisp import CommonLispLexer, EmacsLispLexer


@pytest.mark.parametrize('lexer_cls', [CommonLispLexer, EmacsLispLexer])
def test_symbol_redos_resistance(lexer_cls):
    """Regression test: catastrophic backtracking on a run of dots.

    nonmacro's character class contained the fragment `+-/`, which is
    interpreted as a range from '+' to '/' and unintentionally matches
    '.' too. Since '.' was also matched by constituent's own explicit
    `[#.:]` alternative, the same character could be consumed by either
    branch of the repeated `(?:constituent)*` group, and a long run of
    dots with no closing terminator made the regex engine try every
    combination of which branch matched each dot before giving up.
    """
    attack = '*' + '.' * 200 + '\n'
    t0 = time.time()
    list(lex(attack, lexer_cls()))
    elapsed = time.time() - t0
    assert elapsed < 2.0, (
        f'Catastrophic backtracking in {lexer_cls.__name__}: '
        f'took {elapsed:.1f}s (should be < 2s)'
    )
