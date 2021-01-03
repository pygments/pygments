# -*- coding: utf-8 -*-
"""
    Tests for pygments.regexopt
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
import random
from itertools import combinations_with_replacement

from pygments.regexopt import regex_opt

ALPHABET = ['a', 'b', 'c', 'd', 'e']

N_TRIES = 15


def generate_keywordlist(length):
    return [''.join(p) for p in
            combinations_with_replacement(ALPHABET, length)]


def test_randomly():
    # generate a list of all possible keywords of a certain length using
    # a restricted alphabet, then choose some to match and make sure only
    # those do
    for n in range(3, N_TRIES):
        kwlist = generate_keywordlist(n)
        to_match = random.sample(kwlist,
                                 random.randint(1, len(kwlist) - 1))
        no_match = set(kwlist) - set(to_match)
        rex = re.compile(regex_opt(to_match))
        assert rex.groups == 1
        for w in to_match:
            assert rex.match(w)
        for w in no_match:
            assert not rex.match(w)


def test_prefix():
    opt = regex_opt(('a', 'b'), prefix=r':{1,2}')
    print(opt)
    rex = re.compile(opt)
    assert not rex.match('a')
    assert rex.match('::a')
    assert not rex.match(':::')  # fullmatch


def test_suffix():
    opt = regex_opt(('a', 'b'), suffix=r':{1,2}')
    print(opt)
    rex = re.compile(opt)
    assert not rex.match('a')
    assert rex.match('a::')
    assert not rex.match(':::')  # fullmatch


def test_suffix_opt():
    # test that detected suffixes remain sorted.
    opt = regex_opt(('afoo', 'abfoo'))
    print(opt)
    rex = re.compile(opt)
    m = rex.match('abfoo')
    assert m.end() == 5


def test_different_length_grouping():
    opt = regex_opt(('a', 'xyz'))
    print(opt)
    rex = re.compile(opt)
    assert rex.match('a')
    assert rex.match('xyz')
    assert not rex.match('b')
    assert rex.groups == 1


def test_same_length_grouping():
    opt = regex_opt(('a', 'b'))
    print(opt)
    rex = re.compile(opt)
    assert rex.match('a')
    assert rex.match('b')
    assert not rex.match('x')

    assert rex.groups == 1
    groups = rex.match('a').groups()
    assert groups == ('a',)


def test_same_length_suffix_grouping():
    opt = regex_opt(('a', 'b'), suffix='(m)')
    print(opt)
    rex = re.compile(opt)
    assert rex.match('am')
    assert rex.match('bm')
    assert not rex.match('xm')
    assert not rex.match('ax')
    assert rex.groups == 2
    groups = rex.match('am').groups()
    assert groups == ('a', 'm')
