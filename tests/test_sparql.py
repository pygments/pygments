"""
    Tests for the SPARQL lexer.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import SparqlLexer
from pygments.token import Error
from pygments.token import Name
from pygments.token import Operator


def test_property_path_query():
    code = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
SELECT ?entity ?alias WHERE{
  ?entity wdt:P734?/(skos:altLabel|rdfs:label) ?alias .
}
"""
    tokens = list(SparqlLexer().get_tokens(code))
    assert not any(token is Error for token, _ in tokens)
    assert [value for token, value in tokens if token is Operator] == ['?', '/', '|']
    assert [value for token, value in tokens if token is Name.Variable] == [
        '?entity', '?alias', '?entity', '?alias',
    ]


@pytest.mark.parametrize('code, operators, variables', [
    (':p?', ['?'], []),
    (':p|:q', ['|'], []),
    ('?subject :p? ?object .', ['?'], ['?subject', '?object']),
    ('FILTER (?left || $right)', ['||'], ['?left', '$right']),
    ('"literal ? | ||" # comment ? | ||\n', [], []),
])
def test_property_path_operator_boundaries(code, operators, variables):
    tokens = list(SparqlLexer().get_tokens(code))
    assert not any(token is Error for token, _ in tokens)
    assert [value for token, value in tokens if token is Operator] == operators
    assert [value for token, value in tokens if token is Name.Variable] == variables
