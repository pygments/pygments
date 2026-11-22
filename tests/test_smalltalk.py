"""
    Pygments Smalltalk lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import SmalltalkLexer
from pygments.token import Error, Name


@pytest.mark.parametrize('prefix', ['', '| listing |\nlisting := #(1 2 3).\n'])
@pytest.mark.parametrize('separator', [
    '\n', '\n   ', '\n\n', '\n"a comment"\n',
    '\n"a ""quoted"" comment"\n',
])
def test_multiline_keyword_message(prefix, separator):
    # gh-911: a standalone receiver is not a unary method header if its
    # next significant token is a keyword selector.
    source = (prefix + 'listing' + separator + 'select: #foo\n'
              "    thenCollect: #bar\n    andsomething: 'blabla'")
    tokens = list(SmalltalkLexer().get_tokens_unprocessed(source))
    assert not any(ttype in Error for _, ttype, _ in tokens)
    assert ''.join(value for _, _, value in tokens) == source
    assert [(offset, value) for offset, ttype, value in tokens
            if ttype is Name.Function] == [
                (source.index('select:'), 'select:'),
                (source.index('thenCollect:'), 'thenCollect:'),
                (source.index('andsomething:'), 'andsomething:'),
            ]
    receiver_offset = len(prefix)
    assert (receiver_offset, Name.Variable, 'listing') in tokens


@pytest.mark.parametrize('source, header', [
    ('doIt\n    ^ self size', 'doIt'),
    ('doIt\n    item := 1.\n    ^ item', 'doIt'),
    ('doIt\n    "a comment"\n    ^ self size', 'doIt'),
    ('doIt\n    | item |\n    item := 1.\n    ^ item', 'doIt'),
    ('at: index\n    ^ items at: index', 'at:'),
    ('+ other\n    ^ self value + other', '+'),
])
def test_method_headers_preserved(source, header):
    tokens = list(SmalltalkLexer().get_tokens_unprocessed(source))
    assert not any(ttype in Error for _, ttype, _ in tokens)
    assert tokens[0] == (0, Name.Function, header)
    assert ''.join(value for _, _, value in tokens) == source
