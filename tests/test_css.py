"""
    Pygments CSS lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.css import ScssLexer
from pygments.token import Comment, String


@pytest.mark.parametrize('source, comment', [
    ('// root comment\na {}\n', '// root comment\n'),
    ('a { color: red;\n  // content comment\n  width: 1px; }\n',
     '// content comment'),
    ('a { color: red; // inline comment\n  width: 1px; }\n',
     '// inline comment'),
    ('a, // selector comment\nb { color: red; }\n', '// selector comment'),
    ('a {} // after block\nb {}\n', '// after block\n'),
    ('a { color: red; } // final comment', '// final comment'),
])
def test_scss_single_line_comments(source, comment):
    tokens = list(ScssLexer(stripnl=False, ensurenl=False).get_tokens(source))
    assert ''.join(value for _, value in tokens) == source
    assert [value for token, value in tokens if token in Comment.Single] == [comment]
    if 'width' in source:
        assert any(value == 'width' and token not in Comment for token, value in tokens)


@pytest.mark.parametrize('url', [
    'url(http://example.com/a.png)',
    'URL(http://example.com/a.png)',
    'url(//example.com/a.png)',
    'url(https://example.com/a//b)',
    'url(data:image/png;base64,a//b)',
    'url("https://example.com/a)b//c")',
    "url('https://example.com/a)b//c')",
    'url( "https://example.com/a)b//c" )',
    'url(\nhttps://example.com/a)',
    'url(\n"https://example.com/a)b//c"\n)',
    r'url(https://example.com/a\)b//c)',
    'url(https://example.com/#{$name}//image.png)',
])
@pytest.mark.parametrize('separator', [': ', ':'])
def test_scss_url_slashes_are_not_comments(url, separator):
    source = f'a {{ background{separator}{url}; // after URL\n  width: 1px; }}\n'
    tokens = list(ScssLexer().get_tokens(source))
    assert ''.join(value for _, value in tokens) == source
    assert [value for token, value in tokens if token in Comment.Single] == ['// after URL']
    assert any(token in String and '//' in value for token, value in tokens)
    assert any(value == 'width' and token not in Comment for token, value in tokens)


@pytest.mark.parametrize('value', [
    '"// literal"',
    "'// literal'",
    r'"escaped \" // literal"',
    r'"escaped \\ // literal"',
    '"#{$name}// literal"',
])
def test_scss_string_slashes_are_not_comments(value):
    source = f'a {{ content: {value}; // after string\n  width: 1px; }}\n'
    tokens = list(ScssLexer().get_tokens(source))
    assert ''.join(text for _, text in tokens) == source
    assert [text for token, text in tokens if token in Comment.Single] == ['// after string']
    assert any(token in String and '// literal' in text for token, text in tokens)


def test_scss_slashes_in_block_comments():
    source = 'a { color: red; /* // block comment\n */ // line comment\n  width: 1px; }\n'
    tokens = list(ScssLexer().get_tokens(source))
    assert ''.join(value for _, value in tokens) == source
    assert [value for token, value in tokens if token in Comment.Single] == ['// line comment']
    assert any(token in Comment.Multiline and '// block comment' in value
               for token, value in tokens)
    assert any(value == 'width' and token not in Comment for token, value in tokens)
