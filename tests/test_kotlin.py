"""Kotlin named argument highlighting."""

import pytest

from pygments.lexers.jvm import KotlinLexer
from pygments.token import Comment, Name, String


@pytest.mark.parametrize('source,names', [
    ('execute(context = Context(), str = "string")', ['context', 'str']),
    ('reformat("text", normalizeCase = false)', ['normalizeCase']),
    ('execute(\n    context = Context(),\n    str = "string"\n)', ['context', 'str']),
    ('outer(inner(value = 1), other = 2)', ['value', 'other']),
    ('execute(`argument name` = 1)', ['`argument name`']),
    ('@Annotation(value = "text") class Example', ['value']),
    ('execute(action = { value -> println(value) })', ['action']),
])
def test_named_arguments(source, names):
    tokens = list(KotlinLexer().get_tokens(source))
    for name in names:
        assert (Name.Variable, name) in tokens


@pytest.mark.parametrize('source,expected', [
    ('execute(value == other)', [(Name, 'value'), (Name, 'other')]),
    ('fun execute(value: Int = 1) {}', [(Name, 'value')]),
    ('class Example(val value: Int = 1)', [(Name.Variable, 'value')]),
    ('execute({ value -> value = other })', [(Name, 'value'), (Name, 'other')]),
    ('val (value, other) = pair', [(Name.Variable, 'value'), (Name.Variable, 'other')]),
])
def test_other_identifiers_are_not_named_arguments(source, expected):
    tokens = list(KotlinLexer().get_tokens(source))
    for token in expected:
        assert token in tokens


def test_strings_and_comments():
    tokens = list(KotlinLexer().get_tokens('"(value = 1)" // (other = 2)\n'))
    assert not any(kind is Name.Variable for kind, _ in tokens)
    assert any(kind in String for kind, _ in tokens)
    assert any(kind in Comment for kind, _ in tokens)
