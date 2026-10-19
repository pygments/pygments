"""
    Pygments installer lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.installers import DebianSourcesLexer
from pygments.token import Comment, Error, Keyword, String


@pytest.mark.parametrize('signed_by', (
    '/usr/share/keyrings/debian-archive-keyring.gpg',
    '6D33866EDD8FFA41C0143AEDDCC9EFBF77E11517',
    '/usr/share/keyrings/debian-archive-keyring.gpg '
    '6D33866EDD8FFA41C0143AEDDCC9EFBF77E11517',
    '/usr/share/keyrings/debian-archive-keyring.gpg\n '
    '6D33866EDD8FFA41C0143AEDDCC9EFBF77E11517',
    '/usr/share/keyrings/debian-archive-keyring.gpg\n# Package-managed keyring',
    '/usr/share/keyrings/debian-archive-keyring.gpg\n# Select this key\n '
    '6D33866EDD8FFA41C0143AEDDCC9EFBF77E11517',
))
def test_debian_sources_signed_by_keyring(signed_by):
    text = (
        'Types: deb\n'
        'URIs: https://deb.debian.org/debian\n'
        f'Signed-By: {signed_by}\n'
        'Suites: bookworm\n'
        'Components: main\n'
    )
    tokens = list(DebianSourcesLexer().get_tokens(text))

    assert not any(token is Error for token, _ in tokens)
    for line in signed_by.splitlines():
        token = Comment if line.startswith('#') else String
        assert (token, line.strip()) in tokens
    assert (Keyword, 'Suites') in tokens
    assert (String, 'bookworm') in tokens
    assert (Keyword, 'Components') in tokens
    assert (String, 'main') in tokens
