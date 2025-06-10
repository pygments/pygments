"""
    Tests for inheritance in RegexLexer
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import typing
from pygments.lexer import RegexLexer, inherit
from pygments.token import Text


class One(RegexLexer):
    tokens: typing.ClassVar = {
        'root': [
            ('a', Text),
            ('b', Text),
        ],
    }


class Two(One):
    tokens: typing.ClassVar = {
        'root': [
            ('x', Text),
            inherit,
            ('y', Text),
        ],
    }


class Three(Two):
    tokens: typing.ClassVar = {
        'root': [
            ('i', Text),
            inherit,
            ('j', Text),
        ],
    }


class Beginning(Two):
    tokens: typing.ClassVar = {
        'root': [
            inherit,
            ('m', Text),
        ],
    }


class End(Two):
    tokens: typing.ClassVar = {
        'root': [
            ('m', Text),
            inherit,
        ],
    }


class Empty(One):
    tokens: typing.ClassVar = {}


class Skipped(Empty):
    tokens: typing.ClassVar = {
        'root': [
            ('x', Text),
            inherit,
            ('y', Text),
        ],
    }


def test_single_inheritance_position():
    t = Two()
    pats = [x[0].__self__.pattern for x in t._tokens['root']]
    assert ['x', 'a', 'b', 'y'] == pats


def test_multi_inheritance_beginning():
    t = Beginning()
    pats = [x[0].__self__.pattern for x in t._tokens['root']]
    assert ['x', 'a', 'b', 'y', 'm'] == pats


def test_multi_inheritance_end():
    t = End()
    pats = [x[0].__self__.pattern for x in t._tokens['root']]
    assert ['m', 'x', 'a', 'b', 'y'] == pats


def test_multi_inheritance_position():
    t = Three()
    pats = [x[0].__self__.pattern for x in t._tokens['root']]
    assert ['i', 'x', 'a', 'b', 'y', 'j'] == pats


def test_single_inheritance_with_skip():
    t = Skipped()
    pats = [x[0].__self__.pattern for x in t._tokens['root']]
    assert ['x', 'a', 'b', 'y'] == pats
