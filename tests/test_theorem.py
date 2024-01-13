"""
Tests of the theorem lexers
"""

import textwrap

from pygments.lexers.lean import Lean3Lexer, Lean4Lexer

def test_lean3_import():
    s = textwrap.dedent("""\
        -- this is Lean 3
        import data.nat.basic

        def foo : nat := 1
        """)
    assert Lean3Lexer.analyse_text(s) > 0
    assert Lean4Lexer.analyse_text(s) == 0

def test_lean4_import():
    s = textwrap.dedent("""\
        -- this is Lean 4
        import Mathlib.Data.Nat.Basic

        def foo : Nat := 1
        """)
    assert Lean3Lexer.analyse_text(s) == 0
    assert Lean4Lexer.analyse_text(s) > 0
