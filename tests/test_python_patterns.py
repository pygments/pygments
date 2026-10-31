import ast

import pytest

from pygments.lexers.python import PythonLexer
from pygments.token import Comment, Keyword, Name, String


@pytest.mark.parametrize('source, expected', [
    ('match value:\n    case "_": pass\n', [String]),
    ("match value:\n    case '_': pass\n", [String]),
    ('match value:\n    case r"_": pass\n', [String]),
    ('match value:\n    case [_, "_"]: pass\n', [Keyword, String]),
    ('match value:\n    case {"_": _}: pass\n', [String, Keyword]),
    ('match value:\n    case _ if _ is not None: pass\n', [Keyword, Name]),
    ('match _:\n    case _: pass\n', [Name, Keyword]),
    ('match value:\n    case [\n        _, "_"\n    ]: pass\n', [Keyword, String]),
    ('match value:\n    case _:  # _\n        pass\n', [Keyword, Comment]),
])
def test_pattern_underscore_context(source, expected):
    ast.parse(source)
    tokens = list(PythonLexer().get_tokens_unprocessed(source))
    positions = [index for index, char in enumerate(source) if char == '_']
    for position, token_type in zip(positions, expected, strict=True):
        token = next(token for index, token, value in tokens
                     if index <= position < index + len(value))
        assert token in token_type


@pytest.mark.parametrize('name', ['_capture', 'capture_', 'capture_name'])
def test_pattern_capture_identifiers(name):
    source = f'match value:\n    case {name}: pass\n'
    ast.parse(source)
    tokens = list(PythonLexer().get_tokens_unprocessed(source))
    assert any(token is Name and value == name for _, token, value in tokens)


def test_pattern_as_keyword():
    source = 'match value:\n    case "text" as capture: pass\n'
    ast.parse(source)
    assert any(token is Keyword and value == 'as'
               for _, token, value in PythonLexer().get_tokens_unprocessed(source))
