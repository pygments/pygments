import pytest

from pygments.lexers.templates import JavascriptDjangoLexer
from pygments.token import Comment


@pytest.fixture(scope="module")
def lexer():
    yield JavascriptDjangoLexer()


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
