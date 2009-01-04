# -*- coding: utf-8 -*-
"""
    Pygments tests with example files
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2009 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import os
import unittest

from pygments import highlight
from pygments.lexers import get_lexer_for_filename, get_lexer_by_name
from pygments.token import Error
from pygments.util import ClassNotFound


# generate methods
def test_example_files():
    testdir = os.path.dirname(__file__)
    for fn in os.listdir(os.path.join(testdir, 'examplefiles')):
        absfn = os.path.join(testdir, 'examplefiles', fn)
        if not os.path.isfile(absfn):
            continue

        try:
            lx = get_lexer_for_filename(absfn)
        except ClassNotFound:
            if "_" not in fn:
                raise AssertionError('file %r has no registered extension, '
                                     'nor is of the form <lexer>_filename '
                                     'for overriding, thus no lexer found.'
                                    % fn)
            try:
                name, rest = fn.split("_", 1)
                lx = get_lexer_by_name(name)
            except ClassNotFound:
                raise AssertionError('no lexer found for file %r' % fn)
        yield check_lexer, lx, absfn

def check_lexer(lx, absfn):
    text = open(absfn, 'U').read()
    text = text.strip('\n') + '\n'
    try:
        text = text.decode('utf-8')
    except UnicodeError:
        text = text.decode('latin1')
    ntext = []
    for type, val in lx.get_tokens(text):
        ntext.append(val)
        assert type != Error, 'lexer %s generated error token for %s' % \
                (lx, absfn)
    if u''.join(ntext) != text:
        raise AssertionError('round trip failed for ' + absfn)
