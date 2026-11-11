"""
    Haxe lexer tests
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexers.haxe import HaxeLexer


def test_preproc_stack_does_not_leak_between_runs():
    """
    Lexer instances are cached and reused; preproc_stack used to be a mutable
    class attribute shared by every instance, so a run with an unbalanced
    #if (no #end) corrupted the next run's #else handling.
    """
    lexer = HaxeLexer()
    # Unbalanced #if inside a class body saves ['root', 'class-body'].
    list(lexer.get_tokens("class A {\n#if FOO\n}\n"))
    # A later run's #else must not restore the previous run's stack.
    reused = list(lexer.get_tokens("#else\nvar x = 1;\n"))

    # True baseline: start from a clean class-level stack (the fix makes each
    # run clear it; without the fix both instances would share the pollution
    # and compare equal vacuously).
    HaxeLexer.preproc_stack = []
    fresh = HaxeLexer()
    expected = list(fresh.get_tokens("#else\nvar x = 1;\n"))

    assert reused == expected
