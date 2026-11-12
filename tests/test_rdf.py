"""
    RDF lexer tests
    ~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.rdf import ShExCLexer, SparqlLexer, TurtleLexer
from pygments.token import Error, String


@pytest.mark.parametrize(
    'lexer_class, template',
    [
        (SparqlLexer, 'SELECT ?s WHERE {{ ?s <urn:p> {first} . ?s <urn:q> {second} . }}\n'),
        (TurtleLexer, '<urn:s> <urn:p> {first} .\n<urn:s> <urn:q> {second} .\n'),
        (ShExCLexer, '<urn:Shape> {{ <urn:p> [ {first} {second} ] }}\n'),
    ],
)
@pytest.mark.parametrize('quote', ['"""', "'''"])
@pytest.mark.parametrize(
    'body, suffix',
    [
        ('first literal', ''),
        ('first line\nsecond line', ''),
        ('one {q} and two {q}{q} quote characters', ''),
        (r'escaped \{q} character', ''),
        (r'escaped \{q}{q}{q} sequence', ''),
        (r'escaped \\ backslash', ''),
        ('first literal', '@en-US'),
        ('first literal', '^^<urn:type>'),
    ],
)
def test_long_string_terminators(lexer_class, template, quote, body, suffix):
    # Regression for https://github.com/pygments/pygments/issues/1108.
    first = quote + body.format(q=quote[0]) + quote
    second = quote + 'second literal' + quote
    code = template.format(first=first + suffix, second=second)
    tokens = list(lexer_class().get_tokens_unprocessed(code))
    assert ''.join(value for _, _, value in tokens) == code
    assert not any(token in Error for _, token, _ in tokens)

    spans = [
        (code.index(literal), code.index(literal) + len(literal), literal)
        for literal in (first, second)
    ]
    for start, end, literal in spans:
        parts = [(token, value) for index, token, value in tokens if start <= index < end]
        assert all(token in String for token, _ in parts)
        assert ''.join(value for _, value in parts) == literal
    for index, token, value in tokens:
        if token in String:
            assert any(start <= index and index + len(value) <= end for start, end, _ in spans)
