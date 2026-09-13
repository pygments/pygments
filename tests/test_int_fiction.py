"""
    Interactive fiction lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import time

from pygments import lex
from pygments.lexers.int_fiction import Tads3Lexer


def test_tads3_comment_redos_resistance():
    r"""Regression test: catastrophic backtracking on an unterminated comment.

    Tads3Lexer._comment_single matched each character of a comment with
    either of two alternatives, [^\\\n] and \\+[\w\W]. The second
    alternative can also match a backslash, so a run of N backslashes
    after // could be split between the two branches in 2**(N-1) ways.
    With no newline to end the comment the regex engine tried every
    split, making an input of '//' followed by a long run of backslashes
    take exponential time to highlight. The alternatives are now
    disjoint (\\[\w\W] consumes exactly one backslash and one more
    character), so matching is linear.
    """
    attack = '//' + '\\' * 40
    t0 = time.time()
    list(lex(attack, Tads3Lexer()))
    elapsed = time.time() - t0
    assert elapsed < 2.0, (
        f'Catastrophic backtracking in Tads3Lexer: '
        f'took {elapsed:.1f}s (should be < 2s)'
    )
