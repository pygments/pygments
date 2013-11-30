# -*- coding: utf-8 -*-
"""
    pygments.lexers.cypher
    ~~~~~~~~~~~~~~~~~~~~~~

    A Lexer for the cypher graph query language use in the neo4j graph database.

    `CypherLexer`

    the ``tests/examplefiles`` contains file "test.cyp" which has valid
    cypher queries that execute against the example database shipped with Neo4J


    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, bygroups
from pygments.token import Punctuation, Text, Comment, Operator, Name, \
String, Number, Generic


__all__ = ['CypherLexer']


class CypherLexer(RegexLexer):
    """
    For Cypher Query Language
    http://docs.neo4j.org/chunked/milestone/cypher-query-lang.html
    For the Cypher version in Neo4J 2.0
    """
    name = 'Cypher'
    aliases = ['cypher']
    filenames = ['*.cyp','*.cypher']
    
    tokens = {
        'root': [
            include('comment'),
            include('keywords'),
            include('clauses'),
            include('relations')
            ],
        'comment': [(r'^.*//.*\n', Comment.Single)],
        'keywords': [(
            r''
        )],
        'clauses': [],
        'relations': []
        }


    

