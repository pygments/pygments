"""
    Tests for line comments at the end of embedded .NET and Rust fragments.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.dotnet import CSharpLexer, FSharpLexer, VbNetLexer
from pygments.lexers.rust import RustLexer
from pygments.token import Comment, String


@pytest.mark.parametrize('lexer, text, token', [
    (CSharpLexer, '// comment', Comment.Single),
    (VbNetLexer, "' comment", Comment),
    (VbNetLexer, 'REM comment', Comment),
    (VbNetLexer, 'rem comment', Comment),
    (FSharpLexer, '// comment', Comment.Single),
    (FSharpLexer, '/// documentation', String.Doc),
    (RustLexer, '// comment', Comment.Single),
    (RustLexer, '//! documentation', String.Doc),
    (RustLexer, '/// documentation', String.Doc),
    (RustLexer, '///', String.Doc),
])
def test_line_comment_without_final_newline(lexer, text, token):
    tokens = list(lexer(ensurenl=False).get_tokens(text))
    assert tokens == [(token, text)]
