import pytest

from pygments.lexers.theorem import CoqLexer

@pytest.fixture(scope="module")
def lexer():
    yield CoqLexer()

def test_coq_analyze_text(lexer):
    text = r"""
            Theorem demorgan : forall (P Q : Prop),
              ~(P \/ Q) -> ~P /\ ~Q.
            Proof.
              tauto.
            Qed.
            """
    res = lexer.analyse_text(text)
    assert res == 1.0
