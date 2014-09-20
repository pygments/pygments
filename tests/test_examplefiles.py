# -*- coding: utf-8 -*-
"""
    Pygments tests with example files
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import os
import pprint
import difflib
import pickle

from pygments.lexers import get_lexer_for_filename, get_lexer_by_name
from pygments.token import Error
from pygments.util import ClassNotFound

STORE_OUTPUT = False

STATS = {}

TESTDIR = os.path.dirname(__file__)


# generate methods
def test_example_files():
    global STATS
    STATS = {}
    outdir = os.path.join(TESTDIR, 'examplefiles', 'output')
    if STORE_OUTPUT and not os.path.isdir(outdir):
        os.makedirs(outdir)
    for fn in os.listdir(os.path.join(TESTDIR, 'examplefiles')):
        if fn.startswith('.') or fn.endswith('#'):
            continue

        absfn = os.path.join(TESTDIR, 'examplefiles', fn)
        if not os.path.isfile(absfn):
            continue

        print(absfn)
        code = open(absfn, 'rb').read()
        try:
            code = code.decode('utf-8')
        except UnicodeError:
            code = code.decode('latin1')

        lx = None
        if '_' in fn:
            try:
                lx = get_lexer_by_name(fn.split('_')[0])
            except ClassNotFound:
                pass
        if lx is None:
            try:
                lx = get_lexer_for_filename(absfn, code=code)
            except ClassNotFound:
                raise AssertionError('file %r has no registered extension, '
                                     'nor is of the form <lexer>_filename '
                                     'for overriding, thus no lexer found.'
                                     % fn)
        yield check_lexer, lx, fn

    N = 7
    stats = list(STATS.items())
    stats.sort(key=lambda x: x[1][1])
    print('\nExample files that took longest absolute time:')
    for fn, t in stats[-N:]:
        print('%-30s  %6d chars  %8.2f ms  %7.3f ms/char' % ((fn,) + t))
    print()
    stats.sort(key=lambda x: x[1][2])
    print('\nExample files that took longest relative time:')
    for fn, t in stats[-N:]:
        print('%-30s  %6d chars  %8.2f ms  %7.3f ms/char' % ((fn,) + t))


def check_lexer(lx, fn):
    absfn = os.path.join(TESTDIR, 'examplefiles', fn)
    fp = open(absfn, 'rb')
    try:
        text = fp.read()
    finally:
        fp.close()
    text = text.replace(b'\r\n', b'\n')
    text = text.strip(b'\n') + b'\n'
    try:
        text = text.decode('utf-8')
        if text.startswith(u'\ufeff'):
            text = text[len(u'\ufeff'):]
    except UnicodeError:
        text = text.decode('latin1')
    ntext = []
    tokens = []
    import time
    t1 = time.time()
    for type, val in lx.get_tokens(text):
        ntext.append(val)
        assert type != Error, \
            'lexer %s generated error token for %s: %r at position %d' % \
            (lx, absfn, val, len(u''.join(ntext)))
        tokens.append((type, val))
    t2 = time.time()
    STATS[os.path.basename(absfn)] = (len(text),
                                      1000 * (t2 - t1), 1000 * (t2 - t1) / len(text))
    if u''.join(ntext) != text:
        print('\n'.join(difflib.unified_diff(u''.join(ntext).splitlines(),
                                             text.splitlines())))
        raise AssertionError('round trip failed for ' + absfn)

    # check output against previous run if enabled
    if STORE_OUTPUT:
        # no previous output -- store it
        outfn = os.path.join(TESTDIR, 'examplefiles', 'output', fn)
        if not os.path.isfile(outfn):
            fp = open(outfn, 'wb')
            try:
                pickle.dump(tokens, fp)
            finally:
                fp.close()
            return
        # otherwise load it and compare
        fp = open(outfn, 'rb')
        try:
            stored_tokens = pickle.load(fp)
        finally:
            fp.close()
        if stored_tokens != tokens:
            f1 = pprint.pformat(stored_tokens)
            f2 = pprint.pformat(tokens)
            print('\n'.join(difflib.unified_diff(f1.splitlines(),
                                                 f2.splitlines())))
            assert False, absfn
