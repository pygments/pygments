# -*- coding: utf-8 -*-
"""
    HTML Lexer Tests
    ~~~~~~~~~~~~~~~~

    :copyright: Copyright 2020-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import time

import pytest

from pygments.lexers.html import HtmlLexer
from pygments.token import Token

@pytest.fixture(scope='module')
def lexer_html():
    yield HtmlLexer()

def test_simple_html(lexer_html):
    """ extremely basic happy-path case

    more tests are in test_examplefiles """

    fragment = "<html>\n\t<body>\n\t\thello world\n\t</body>\n</html>"
    tokens = list(lexer_html.get_tokens(fragment))
    assert all(x[1] != Token.Error for x in tokens)

def test_happy_javascript_fragment(lexer_html):
    """ valid, even long Javascript fragments should still get parsed ok """

    fragment = "<script type=\"text/javascript\">"+"alert(\"hi\");"*2000+"</script>"
    start_time = time.time()
    tokens = list(lexer_html.get_tokens(fragment))
    assert all(x[1] != Token.Error for x in tokens)
    assert time.time() - start_time < 5, 'The HTML lexer might have an expensive happy-path script case'

def test_happy_css_fragment(lexer_html):
    """ valid, even long CSS fragments should still get parsed ok """

    fragment = "<style>"+".ui-helper-hidden{display:none}"*2000+"</style>"
    start_time = time.time()
    tokens = list(lexer_html.get_tokens(fragment))
    assert all(x[1] != Token.Error for x in tokens)
    assert time.time() - start_time < 5, 'The HTML lexer might have an expensive happy-path style case'

def test_long_unclosed_javascript_fragment(lexer_html):
    """ unclosed, long Javascript fragments should parse quickly """

    reps = 2000
    fragment = "<script type=\"text/javascript\">"+"alert(\"hi\");"*reps
    start_time = time.time()
    tokens = list(lexer_html.get_tokens(fragment))
    assert time.time() - start_time < 5, 'The HTML lexer might have an expensive error script case'
    tokens_intro = [
        (Token.Punctuation, '<'),
        (Token.Name.Tag, 'script'),
        (Token.Text, ' '),
        (Token.Name.Attribute, 'type'),
        (Token.Operator, '='),
        (Token.Literal.String, '"text/javascript"'),
        (Token.Punctuation, '>'),
    ]
    tokens_body = [
        (Token.Name.Other, 'alert'),
        (Token.Punctuation, '('),
        (Token.Literal.String.Double, '"hi"'),
        (Token.Punctuation, ')'),
        (Token.Punctuation, ';'),
    ]

    # make sure we get the right opening tokens
    assert tokens[:len(tokens_intro)] == tokens_intro
    # and make sure we get the right body tokens even though the script is
    # unclosed
    assert tokens[len(tokens_intro):-1] == tokens_body * reps
    # and of course, the newline we get for free from get_tokens
    assert tokens[-1] == (Token.Text, "\n")

def test_long_unclosed_css_fragment(lexer_html):
    """ unclosed, long CSS fragments should parse quickly """

    reps = 2000
    fragment = "<style>"+".ui-helper-hidden{display:none}"*reps
    start_time = time.time()
    tokens = list(lexer_html.get_tokens(fragment))
    assert time.time() - start_time < 5, 'The HTML lexer might have an expensive error style case'

    tokens_intro = [
        (Token.Punctuation, '<'),
        (Token.Name.Tag, 'style'),
        (Token.Punctuation, '>'),
    ]
    tokens_body = [
        (Token.Punctuation, '.'),
        (Token.Name.Class, 'ui-helper-hidden'),
        (Token.Punctuation, '{'),
        (Token.Keyword, 'display'),
        (Token.Punctuation, ':'),
        (Token.Keyword.Constant, 'none'),
        (Token.Punctuation, '}'),
    ]

    # make sure we get the right opening tokens
    assert tokens[:len(tokens_intro)] == tokens_intro
    # and make sure we get the right body tokens even though the style block is
    # unclosed
    assert tokens[len(tokens_intro):-1] == tokens_body * reps
    # and of course, the newline we get for free from get_tokens
    assert tokens[-1] == (Token.Text, "\n")

def test_unclosed_fragment_with_newline_recovery(lexer_html):
    """ unclosed Javascript fragments should recover on the next line """

    fragment = "<script type=\"text/javascript\">"+"alert(\"hi\");"*20+"\n<div>hi</div>"
    tokens = list(lexer_html.get_tokens(fragment))
    recovery_tokens = [
            (Token.Punctuation, '<'),
            (Token.Name.Tag, 'div'),
            (Token.Punctuation, '>'),
            (Token.Text, 'hi'),
            (Token.Punctuation, '<'),
            (Token.Punctuation, '/'),
            (Token.Name.Tag, 'div'),
            (Token.Punctuation, '>'),
            (Token.Text, '\n')]
    assert tokens[-1*len(recovery_tokens):] == recovery_tokens

