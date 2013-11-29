# -*- coding: utf-8 -*-
"""
    pygments.lexers.cypher
    ~~~~~~~~~~~~~~~~~~~~~~

    A Lexer for the cypher graph query language use in the neo4j graph database.

    `CypherLexer`

    the ``tests/examplefiles`` contains file "movie_queries.cyp" which has valid
    cypher queries that execute against the example database shipped with Neo4J


    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer, RegexLexer, do_insertions, bygroups
from pygments.token import Punctuation, Text, Comment, Operator, Name, \
String, Number, Generic
from pygments.lexers import get_lexer_by_name, ClassNotFound

__all__ = ['CypherLexer']

line_re = re.compile('.*?\n')

name = 'Cypher'
aliases = ['cypher']
filenames = ['*.cyp','*.cypher']
flags = re.MULTILINE | re.DOTALL
