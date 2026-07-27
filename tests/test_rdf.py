"""Tests for RDF-family lexers."""

import pytest

from pygments.lexers.rdf import SparqlLexer, TurtleLexer, ShExCLexer
from pygments.token import Error, Keyword, Literal, Name, Operator, Punctuation, Text


RDF_LEXERS = [SparqlLexer, TurtleLexer, ShExCLexer]


def _instrument_prefixed_name_rule(lexer):
    rules = list(lexer._tokens['root'])
    prefixed_name = (r'(' + lexer.PN_PREFIX + r')?(\:)(' +
                     lexer.PN_LOCAL + r')?')
    patterns = [rule[0] for rule in lexer.tokens['root']]
    assert patterns.count(prefixed_name) == 1
    index = patterns.index(prefixed_name)
    original, action, state = rules[index]
    counter = {'candidate_units': 0}

    def counted(text, pos):
        counter['candidate_units'] += len(text) - pos
        return original(text, pos)

    rules[index] = counted, action, state
    lexer._tokens = dict(lexer._tokens)
    lexer._tokens['root'] = rules
    return counter


@pytest.mark.parametrize('lexer_cls', RDF_LEXERS)
def test_colonless_prefix_work_is_linear(lexer_cls):
    size = 128
    lexer = lexer_cls(ensurenl=False)
    counter = _instrument_prefixed_name_rule(lexer)
    tokens = list(lexer.get_tokens_unprocessed('a' * size))

    expected = [(index, Error, 'a') for index in range(size)]
    if lexer_cls is not TurtleLexer:
        expected[-1] = (size - 1, Keyword, 'a')
    assert tokens == expected
    assert counter['candidate_units'] <= 2 * size


@pytest.mark.parametrize(('lexer_cls', 'text', 'expected'), [
    (SparqlLexer, 'select', [(0, Keyword, 'select')]),
    (SparqlLexer, 'str', [(0, Name.Function, 'str')]),
    (SparqlLexer, 'true', [(0, Keyword.Constant, 'true')]),
    (SparqlLexer, 'xgroup by', [
        (0, Error, 'x'),
        (1, Keyword, 'group by'),
    ]),
    (SparqlLexer, 'ex:local', [
        (0, Name.Namespace, 'ex'),
        (2, Punctuation, ':'),
        (3, Name.Tag, 'local'),
    ]),
    (TurtleLexer, 'true', [(0, Literal, 'true')]),
    (TurtleLexer, ' a ', [(0, Text, ' '), (1, Keyword.Type, 'a'),
                          (2, Text, ' ')]),
    (TurtleLexer, 'ex:local', [
        (0, Name.Namespace, 'ex'),
        (2, Punctuation, ':'),
        (3, Name.Tag, 'local'),
    ]),
    (ShExCLexer, 'base', [(0, Keyword, 'base')]),
    (ShExCLexer, 'and', [(0, Operator.Word, 'and')]),
    (ShExCLexer, 'ex:local', [
        (0, Name.Namespace, 'ex'),
        (2, Punctuation, ':'),
        (3, Name.Tag, 'local'),
    ]),
])
def test_valid_tokens_are_not_stolen(lexer_cls, text, expected):
    lexer = lexer_cls(ensurenl=False)
    assert list(lexer.get_tokens_unprocessed(text)) == expected
