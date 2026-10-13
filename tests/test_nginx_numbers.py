"""
    Tests for complete numeric arguments in Nginx configuration.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.configs import NginxConfLexer
from pygments.token import Comment, Keyword, Name, Number, Punctuation, String, Whitespace


@pytest.mark.parametrize('directive, argument', [
    ('listen', '127.0.0.1'),
    ('allow', '192.0.0.1'),
    ('deny', '10.1.2.3/24'),
    ('server_name', '123.example.com'),
    ('root', '123/path'),
])
def test_numeric_prefix_does_not_split_argument(directive, argument):
    tokens = [(token, value) for token, value in
              NginxConfLexer().get_tokens(f'{directive} {argument};\n')
              if token is not Whitespace]
    assert tokens == [(Keyword, directive), (String, argument), (Punctuation, ';')]


@pytest.mark.parametrize('argument', ['0', '80', '1024', '16k', '8m'])
@pytest.mark.parametrize('ending', [';', ' ;', '# comment\n;'])
def test_complete_numeric_arguments(argument, ending):
    tokens = list(NginxConfLexer().get_tokens(f'buffer {argument}{ending}\n'))
    assert (Number.Integer, argument) in tokens
    if '#' in ending:
        assert any(token is Comment.Single for token, _ in tokens)


def test_address_with_port_preserves_existing_tokens():
    tokens = [(token, value) for token, value in
              NginxConfLexer().get_tokens('listen 127.0.0.1:8080;\n')
              if token is not Whitespace]
    assert tokens == [
        (Keyword, 'listen'), (Name, '127.0.0.1'), (Punctuation, ':'),
        (Number.Integer, '8080'), (Punctuation, ';'),
    ]
