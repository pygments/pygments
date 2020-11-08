import pytest

from pygments.lexers.templates import JavascriptDjangoLexer, MasonLexer
from pygments.token import Comment


@pytest.fixture(scope="module")
def lexer():
    yield JavascriptDjangoLexer()

@pytest.fixture(scope='module')
def lexerMason():
    yield MasonLexer()

def test_do_not_mistake_JSDoc_for_django_comment(lexer):
    """
    Test to make sure the lexer doesn't mistake
    {* ... *} to be a django comment
    """
    text = """/**
               * @param {*} cool
               */
              func = function(cool) {
              };

              /**
               * @param {*} stuff
               */
              fun = function(stuff) {
              };"""
    tokens = lexer.get_tokens(text)
    assert not any(t[0] == Comment for t in tokens)

def test_mason_unnamed_block(lexerMason):
    text = """
            <%class>
            has 'foo';
            has 'bar' => (required => 1);
            has 'baz' => (isa => 'Int', default => 17);
            </%class>
            """
    res = lexerMason.analyse_text(text)
    assert res == 1.0
    