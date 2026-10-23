"""
    JavaScript lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.javascript import JavascriptLexer, TypeScriptLexer
from pygments.lexers.jsx import JsxLexer, TsxLexer
from pygments.token import Comment, Error, Name, String


@pytest.mark.parametrize('lexer_cls', [JavascriptLexer, TypeScriptLexer, JsxLexer, TsxLexer])
@pytest.mark.parametrize('source, name', [
    ('function processData(items) { console.log(items); }', 'processData'),
    ('const f = function named() {};', 'named'),
    ('async function fetchData() {}', 'fetchData'),
    ('function* generate() { yield /value/g; }', 'generate'),
    ('const f = async function * generate() {};', 'generate'),
    ('function /* name */ named() {}', 'named'),
    ('function // name\n named() {}', 'named'),
    ('function\n* /* name */\n generate() {}', 'generate'),
    ('function named /* parameters */ () {}', 'named'),
    ('function named // parameters\n () {}', 'named'),
    ('function $name_1() {}', '$name_1'),
    ('function café() {}', 'café'),
    ('function 变量() {}', '变量'),
    ('function a\u0301\u200c() {}', 'a\u0301\u200c'),
    (r'function \u0066oo() {}', r'\u0066oo'),
    ('object./* property */method(); /* gap */ function named() {}', 'named'),
])
def test_function_names(lexer_cls, source, name):
    tokens = list(lexer_cls(ensurenl=False).get_tokens(source))
    assert [value for token, value in tokens if token is Name.Function] == [name]
    assert not any(token is Error for token, _ in tokens)
    assert ''.join(value for _, value in tokens) == source


@pytest.mark.parametrize('lexer_cls', [JavascriptLexer, TypeScriptLexer, JsxLexer, TsxLexer])
@pytest.mark.parametrize('source', [
    'const f = function(items) { return items; }; after();',
    'const f = function /* anonymous */ (items) { return items; }; after();',
    'const f = function* (items) { yield items; }; after();',
    'const f = async function /* anonymous */ * (items) { yield items; }; after();',
    'const text = "function named() {}"; after();',
    '// function named() {}\nafter();',
    '/* function named() {} */ after();',
    'const text = `function named() {}`; after();',
    'const pattern = /function named/g; after();',
    'object.function(items); after();',
    'object.function * scale();',
    'object. /* property */ function * scale();',
    'object?. // property\n function * scale();',
    'const object = {function: value}; after();',
    'const object = {function(items) { return items; }}; after();',
    'function$factory();',
    'class C { function\nvalue = 1; }',
    'class C { function /* field\n separator */ value = 1; }',
    'class C { function\nvalue; }',
])
def test_not_function_names(lexer_cls, source):
    tokens = list(lexer_cls(ensurenl=False).get_tokens(source))
    assert not any(token in (Name.Function, Error) for token, _ in tokens)
    assert ''.join(value for _, value in tokens) == source


@pytest.mark.parametrize('lexer_cls', [JavascriptLexer, TypeScriptLexer, JsxLexer, TsxLexer])
def test_function_regex_and_following_code(lexer_cls):
    source = 'const f = function* () { yield /value/g; }; const after = /next/;'
    tokens = list(lexer_cls().get_tokens(source))
    assert [value for token, value in tokens if token is String.Regex] == ['/value/g', '/next/']
    assert (Name.Other, 'after') in tokens


def test_function_in_property_comment():
    tokens = list(JavascriptLexer().get_tokens('object. // function\n method();'))
    assert (Comment.Single, '// function') in tokens
    assert not any(token is Error for token, _ in tokens)


@pytest.mark.parametrize('lexer_cls', [TypeScriptLexer, TsxLexer])
def test_typescript_generic_function(lexer_cls):
    tokens = list(lexer_cls().get_tokens('function identity<T>(value: T): T { return value; }'))
    assert (Name.Function, 'identity') in tokens
    assert (Name.Other, 'value') in tokens
