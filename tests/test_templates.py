import pytest

from pygments.lexers.templates import JavascriptDjangoLexer, MasonLexer, VelocityLexer
from pygments.token import Comment


@pytest.fixture(scope="module")
def lexer():
    yield JavascriptDjangoLexer()

@pytest.fixture(scope='module')
def lexerMason():
    yield MasonLexer()

@pytest.fixture(scope='module')
def lexerVelocity():
    yield VelocityLexer()

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

def test_velocity_macro(lexerVelocity):
    text = """
            #macro(getBookListLink, $readingTrackerResult)
              $readingTrackerResult.getBookListLink()
            #end
            """
    res = lexerVelocity.analyse_text(text)
    assert res == 0.26

def test_velocity_foreach(lexerVelocity):
    text = """
            <ul>
            #foreach( $product in $allProducts )
              <li>$product</li>
            #end
            </ul>
            """
    res = lexerVelocity.analyse_text(text)
    assert res == 0.16

def test_velocity_if(lexerVelocity):
    text = """
            #if( $display )
              <strong>Velocity!</strong>
            #end
            """
    res = lexerVelocity.analyse_text(text)
    assert res == 0.16

def test_velocity_reference(lexerVelocity):
    text = """
            Hello $name!  Welcome to Velocity!
            """
    res = lexerVelocity.analyse_text(text)
    assert res == 0.01
