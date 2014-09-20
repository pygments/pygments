# -*- coding: utf-8 -*-
"""
    Tests for pygments.regexopt
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
import random
import unittest
import itertools

from pygments.regexopt import regex_opt

ALPHABET = ['a', 'b', 'c', 'd', 'e']
N_TRIES = 15


class RegexOptTestCase(unittest.TestCase):

    def generate_keywordlist(self, length):
        return [''.join(p) for p in
                itertools.combinations_with_replacement(ALPHABET, length)]

    def test_randomly(self):
        # generate a list of all possible keywords of a certain length using
        # a restricted alphabet, then choose some to match and make sure only
        # those do
        for n in range(3, N_TRIES):
            kwlist = self.generate_keywordlist(n)
            to_match = random.sample(kwlist,
                                     random.randint(1, len(kwlist) - 1))
            no_match = set(kwlist) - set(to_match)
            rex = re.compile(regex_opt(to_match))
            for w in to_match:
                self.assertTrue(rex.match(w))
            for w in no_match:
                self.assertFalse(rex.match(w))
