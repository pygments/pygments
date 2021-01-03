# -*- coding: utf-8 -*-
"""
    Basic CLexer Test
    ~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import textwrap

import pytest

from pygments.token import Text, Number, Token
from pygments.lexers import CLexer


@pytest.fixture(scope='module')
def lexer():
    yield CLexer()


def test_numbers(lexer):
    code = '42 23.42 23. .42 023 0xdeadbeef 23e+42 42e-23'
    wanted = []
    for item in zip([Number.Integer, Number.Float, Number.Float,
                     Number.Float, Number.Oct, Number.Hex,
                     Number.Float, Number.Float], code.split()):
        wanted.append(item)
        wanted.append((Text, ' '))
    wanted = wanted[:-1] + [(Text, '\n')]
    assert list(lexer.get_tokens(code)) == wanted


def test_switch(lexer):
    fragment = '''\
    int main()
    {
        switch (0)
        {
            case 0:
            default:
                ;
        }
    }
    '''
    tokens = [
        (Token.Keyword.Type, 'int'),
        (Token.Text, ' '),
        (Token.Name.Function, 'main'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Punctuation, '{'),
        (Token.Text, '\n'),
        (Token.Text, '    '),
        (Token.Keyword, 'switch'),
        (Token.Text, ' '),
        (Token.Punctuation, '('),
        (Token.Literal.Number.Integer, '0'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Text, '    '),
        (Token.Punctuation, '{'),
        (Token.Text, '\n'),
        (Token.Text, '        '),
        (Token.Keyword, 'case'),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '0'),
        (Token.Operator, ':'),
        (Token.Text, '\n'),
        (Token.Text, '        '),
        (Token.Keyword, 'default'),
        (Token.Operator, ':'),
        (Token.Text, '\n'),
        (Token.Text, '            '),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
        (Token.Text, '    '),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(textwrap.dedent(fragment))) == tokens


def test_switch_space_before_colon(lexer):
    fragment = '''\
    int main()
    {
        switch (0)
        {
            case 0 :
            default :
                ;
        }
    }
    '''
    tokens = [
        (Token.Keyword.Type, 'int'),
        (Token.Text, ' '),
        (Token.Name.Function, 'main'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Punctuation, '{'),
        (Token.Text, '\n'),
        (Token.Text, '    '),
        (Token.Keyword, 'switch'),
        (Token.Text, ' '),
        (Token.Punctuation, '('),
        (Token.Literal.Number.Integer, '0'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Text, '    '),
        (Token.Punctuation, '{'),
        (Token.Text, '\n'),
        (Token.Text, '        '),
        (Token.Keyword, 'case'),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '0'),
        (Token.Text, ' '),
        (Token.Operator, ':'),
        (Token.Text, '\n'),
        (Token.Text, '        '),
        (Token.Keyword, 'default'),
        (Token.Text, ' '),
        (Token.Operator, ':'),
        (Token.Text, '\n'),
        (Token.Text, '            '),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
        (Token.Text, '    '),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(textwrap.dedent(fragment))) == tokens


def test_label(lexer):
    fragment = '''\
    int main()
    {
    foo:
      goto foo;
    }
    '''
    tokens = [
        (Token.Keyword.Type, 'int'),
        (Token.Text, ' '),
        (Token.Name.Function, 'main'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Punctuation, '{'),
        (Token.Text, '\n'),
        (Token.Name.Label, 'foo'),
        (Token.Punctuation, ':'),
        (Token.Text, '\n'),
        (Token.Text, '  '),
        (Token.Keyword, 'goto'),
        (Token.Text, ' '),
        (Token.Name, 'foo'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(textwrap.dedent(fragment))) == tokens


def test_label_space_before_colon(lexer):
    fragment = '''\
    int main()
    {
    foo :
      goto foo;
    }
    '''
    tokens = [
        (Token.Keyword.Type, 'int'),
        (Token.Text, ' '),
        (Token.Name.Function, 'main'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Punctuation, '{'),
        (Token.Text, '\n'),
        (Token.Name.Label, 'foo'),
        (Token.Text, ' '),
        (Token.Punctuation, ':'),
        (Token.Text, '\n'),
        (Token.Text, '  '),
        (Token.Keyword, 'goto'),
        (Token.Text, ' '),
        (Token.Name, 'foo'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(textwrap.dedent(fragment))) == tokens


def test_label_followed_by_statement(lexer):
    fragment = '''\
    int main()
    {
    foo:return 0;
      goto foo;
    }
    '''
    tokens = [
        (Token.Keyword.Type, 'int'),
        (Token.Text, ' '),
        (Token.Name.Function, 'main'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
        (Token.Punctuation, '{'),
        (Token.Text, '\n'),
        (Token.Name.Label, 'foo'),
        (Token.Punctuation, ':'),
        (Token.Keyword, 'return'),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '0'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
        (Token.Text, '  '),
        (Token.Keyword, 'goto'),
        (Token.Text, ' '),
        (Token.Name, 'foo'),
        (Token.Punctuation, ';'),
        (Token.Text, '\n'),
        (Token.Punctuation, '}'),
        (Token.Text, '\n'),
    ]
    assert list(lexer.get_tokens(textwrap.dedent(fragment))) == tokens


def test_preproc_file(lexer):
    fragment = '#include <foo>\n'
    tokens = [
        (Token.Comment.Preproc, '#'),
        (Token.Comment.Preproc, 'include'),
        (Token.Text, ' '),
        (Token.Comment.PreprocFile, '<foo>'),
        (Token.Comment.Preproc, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_preproc_file2(lexer):
    fragment = '#include "foo.h"\n'
    tokens = [
        (Token.Comment.Preproc, '#'),
        (Token.Comment.Preproc, 'include'),
        (Token.Text, ' '),
        (Token.Comment.PreprocFile, '"foo.h"'),
        (Token.Comment.Preproc, '\n'),
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
