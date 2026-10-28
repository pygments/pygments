"""
    Tests for the VHDL lexer.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.hdl import VhdlLexer
from pygments.token import Comment, Keyword, Name, String, Whitespace


@pytest.mark.parametrize('directive', [
    '`if VHDL_VERSION >= "2019" then',
    '`elsif TOOL_TYPE = "SIMULATION" then',
    '`else',
    '`end if',
    '`end',
    '`warning "unsupported tool"',
    '`error "unsupported version"',
    '`protect begin_protected',
    '`vendor_extension "argument"',
])
def test_tool_directive(directive):
    tokens = list(VhdlLexer().get_tokens(directive + '\nsignal ready : bit;\n'))
    assert tokens[0] == (Comment.Preproc, directive)
    assert tokens[1] == (Whitespace, '\n')
    assert tokens[2] == (Keyword, 'signal')
    assert (Name, 'ready') in tokens


@pytest.mark.parametrize(('source', 'token'), [
    ('-- `error "disabled"', Comment.Single),
    ('/* `error "disabled" */', Comment.Multiline),
    ('"`warning"', String),
])
def test_grave_accent_in_comment_or_string(source, token):
    assert list(VhdlLexer().get_tokens(source))[0] == (token, source)


def test_tool_directive_between_end_and_block_name():
    tokens = list(VhdlLexer().get_tokens('end\n`warning "ending"\nentity;\n'))
    assert (Comment.Preproc, '`warning "ending"') in tokens
    assert (Keyword, 'entity') in tokens


def test_indented_tool_directive_at_end_of_file():
    tokens = list(VhdlLexer(ensurenl=False).get_tokens('  `else'))
    assert tokens == [(Whitespace, '  '), (Comment.Preproc, '`else')]
