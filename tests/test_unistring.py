# -*- coding: utf-8 -*-
"""
    Test suite for the unistring module
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re
import random

from pygments import unistring as uni


def test_cats_exist_and_compilable():
    for cat in uni.cats:
        s = getattr(uni, cat)
        if s == '':  # Probably Cs on Jython
            continue
        print("%s %r" % (cat, s))
        re.compile('[%s]' % s)


def _cats_that_match(c):
    matching_cats = []
    for cat in uni.cats:
        s = getattr(uni, cat)
        if s == '':  # Probably Cs on Jython
            continue
        if re.compile('[%s]' % s).match(c):
            matching_cats.append(cat)
    return matching_cats


def test_spot_check_types():
    # Each char should match one, and precisely one, category
    random.seed(0)
    for i in range(1000):
        o = random.randint(0, 65535)
        c = chr(o)
        if o > 0xd800 and o <= 0xdfff and not uni.Cs:
            continue  # Bah, Jython.
        print(hex(o))
        cats = _cats_that_match(c)
        assert len(cats) == 1, "%d (%s): %s" % (o, c, cats)
