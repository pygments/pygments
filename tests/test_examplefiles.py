# -*- coding: utf-8 -*-
"""
    Pygments tests with example files
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: 2006 by Georg Brandl.
    :license: GNU LGPL, see LICENSE for more details.
"""

import unittest
import os

from pygments import highlight
from pygments.lexers import get_lexer_for_filename, get_lexer_by_name
from pygments.token import Error


class ExampleFileTest(unittest.TestCase):
    pass

lfd = 0

# generate methods
for fn in os.listdir(os.path.join(testdir, 'examplefiles')):
    absfn = os.path.join(testdir, 'examplefiles', fn)
    if not os.path.isfile(absfn):
        continue

    try:
        lx = get_lexer_for_filename(absfn)
    except ValueError:
        try:
            name, rest = fn.split("_", 1)
            lx = get_lexer_by_name(name)
        except ValueError:
            raise AssertionError('no lexer found for file %r' % fn)

    def test(self, lx=lx, absfn=absfn):
        text = file(absfn, 'U').read()
        text = text.strip('\n') + '\n'
        text = text.decode('latin1')
        ntext = []
        for type, val in lx.get_tokens(text):
            ntext.append(val)
            self.failIf(type == Error, 'lexer generated error token for '+absfn)
        if u''.join(ntext) != text:
            self.fail('round trip failed for '+absfn)

    setattr(ExampleFileTest, 'test_%i' % lfd, test)
    lfd += 1
