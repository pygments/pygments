import pytest

from pygments.lexers.templates import JavascriptDjangoLexer, MasonLexer
from pygments.token import Comment, Token


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

def test_mason_handles_tags_correctly(lexerMason):
    fragment = "<%class>\nhas 'foo';\nhas 'bar' => (required => 1);\nhas 'baz' => (isa => 'Int', default => 17);\n</%class>\n"
    tokens = [
        (Token.Name.Tag, '<%class>'),
        (Token.Text, '\n'),
        (Token.Name, ''),
        (Token.Name, 'has'),
        (Token.Text, ' '),
        (Token.Literal.String, "'foo'"),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
        (Token.Name, ''),
        (Token.Name, 'has'),
        (Token.Text, ' '),
        (Token.Literal.String, "'bar'"),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Operator, '>'),
        (Token.Text, ' '),
        (Token.Punctuation, '('),
        (Token.Name, ''),
        (Token.Name, 'required'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Operator, '>'),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '1'),
        (Token.Punctuation, ')'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
        (Token.Name, ''),
        (Token.Name, 'has'),
        (Token.Text, ' '),
        (Token.Literal.String, "'baz'"),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Operator, '>'),
        (Token.Text, ' '),
        (Token.Punctuation, '('),
        (Token.Name, ''),
        (Token.Name, 'isa'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Operator, '>'),
        (Token.Text, ' '),
        (Token.Literal.String, "'Int'"),
        (Token.Punctuation, ','),
        (Token.Text, ' '),
        (Token.Name, ''),
        (Token.Name, 'default'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Operator, '>'),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '17'),
        (Token.Punctuation, ')'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
        (Token.Name.Tag, '</%class>'),
        (Token.Text, '\n'),
    ]
    assert list(lexerMason.get_tokens(fragment)) == tokens