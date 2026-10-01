"""
    Pygments JavaScript lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.formatters.latex import LatexEmbeddedLexer
from pygments.lexers.javascript import NodeConsoleLexer
from pygments.token import Token


NODE_SESSIONS = [
    '42\n',
    '42\nundefined\n',
    '> a\n',
    '> a\n1\n',
    'Welcome\n> const a = 1\nundefined\n',
    '> a\n> b\n',
    '> a\n1\n> b\n2\n',
    '> a\n1\n2\n> b\n3\n4\n',
    '> if (true) {\n... console.log("x");\n... }\nx\nundefined\n',
    '> if (true) {\n... if (true) {\n...... 1\n...... }\n... }\n1\n',
    '... value\nanswer\n',
    '> a\n\n> b\n2\n',
    '> function f() {\n...\n... }\nundefined\n',
    '> \nundefined\n',
    '  > not a prompt\n> a\n1\n',
    '.break\n> a\n1\n',
    '> const π = "😀"\nundefined\n> π\n\'😀\'\n',
    '😀 ready\n> a\n1\n',
    '> a\n1\n> a + 1\n',
    '> a\n1\n> function f() {\n... return 1;\n... }\n',
]


@pytest.mark.parametrize('source', NODE_SESSIONS)
@pytest.mark.parametrize('newline', ['\n', '\r\n'])
def test_node_console_token_offsets(source, newline):
    # gh-2666: every batch must use positions in the complete session.
    source = source.replace('\n', newline)
    position = 0
    for index, _token, value in NodeConsoleLexer().get_tokens_unprocessed(source):
        assert index == position
        assert source[index:index + len(value)] == value
        position += len(value)
    assert position == len(source)


@pytest.mark.parametrize('protected', [
    '"@keep@"',
    "'@keep@'",
    '`@keep@`',
    '// @keep@',
    '/* @keep@ */',
])
def test_node_console_embedded_escapes(protected):
    source = f'> {protected}\nundefined\n[ @\\textit{{1}}@, 2 ]\n'
    lexer = LatexEmbeddedLexer('@', '@', NodeConsoleLexer())
    tokens = list(lexer.get_tokens(source))

    assert [(token, value) for token, value in tokens if token is Token.Escape] == [
        (Token.Escape, r'\textit{1}'),
    ]
    assert ''.join(value for _token, value in tokens) == (
        f'> {protected}\nundefined\n[ \\textit{{1}}, 2 ]\n'
    )


@pytest.mark.parametrize(('options', 'source', 'expected'), [
    ({}, '> a\n1', '> a\n1\n'),
    ({'stripnl': False}, '\n> a\n1\n\n', '\n> a\n1\n\n'),
    ({'stripnl': True}, '\n> a\n1\n\n', '> a\n1\n'),
    ({'stripall': True}, ' \n> a\n1\n \n', '> a\n1\n'),
    ({'ensurenl': False, 'stripnl': False}, '> a\n1\n', '> a\n1\n'),
    ({'tabsize': 4}, '>\tvalue\n\toutput\n', '>   value\n    output\n'),
    ({}, '> a\r\n1\r\n', '> a\n1\n'),
])
def test_node_console_input_preprocessing(options, source, expected):
    inner = NodeConsoleLexer(**options)
    wrapped = LatexEmbeddedLexer('@', '@', inner)

    assert ''.join(value for _token, value in inner.get_tokens(source)) == expected
    assert ''.join(value for _token, value in wrapped.get_tokens(source)) == expected
