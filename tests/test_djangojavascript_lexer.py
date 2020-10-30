import pytest

from pygments.lexers.templates import JavascriptDjangoLexer
from pygments.token import Comment


@pytest.fixture(scope="module")
def lexer():
    yield JavascriptDjangoLexer()


def test_do_not_mistake_JSDoc_for_django_comment(lexer):
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
    tokens = list(lexer.get_tokens(text))
    assert (
        (
            Comment,
            """{*} cool
               */
              func = function(cool) {
              };
               
              /**
               * @param {*}""",
        )
        not in tokens
    )
