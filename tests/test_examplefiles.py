# -*- coding: utf-8 -*-
"""
    Pygments tests with example files
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import os
import pprint
import difflib
import pickle

import pytest

from pygments.lexers import get_lexer_for_filename, get_lexer_by_name
from pygments.token import Error
from pygments.util import ClassNotFound

# You can set this to True to store the exact token type output of example
# files in tests/examplefiles/output, and on the next run the test will
# want them to stay the same.  In the repository, this should stay False.
STORE_OUTPUT = False

STATS = {}

TESTDIR = os.path.dirname(__file__)


def get_example_files():
    STATS.clear()
    outdir = os.path.join(TESTDIR, 'examplefiles', 'output')
    if STORE_OUTPUT and not os.path.isdir(outdir):
        os.makedirs(outdir)
    for fn in os.listdir(os.path.join(TESTDIR, 'examplefiles')):
        if fn.startswith('.') or fn.endswith('#'):
            continue

        absfn = os.path.join(TESTDIR, 'examplefiles', fn)
        if not os.path.isfile(absfn):
            continue

        extension = os.getenv('TEST_EXT')
        if extension and not absfn.endswith(extension):
            continue

        yield fn


@pytest.mark.parametrize('filename', get_example_files())
def test_examplefile(filename):
    absfn = os.path.join(TESTDIR, 'examplefiles', filename)
    with open(absfn, 'rb') as f:
        text = f.read()
    try:
        utext = text.decode('utf-8')
    except UnicodeError:
        utext = text.decode('latin1')

    lx = None
    if '_' in filename:
        try:
            lx = get_lexer_by_name(filename.split('_')[0])
        except ClassNotFound:
            pass
    if lx is None:
        try:
            lx = get_lexer_for_filename(absfn, code=utext)
        except ClassNotFound:
            raise AssertionError('file %r has no registered extension, '
                                 'nor is of the form <lexer>_filename '
                                 'for overriding, thus no lexer found.'
                                 % filename)

    text = text.replace(b'\r\n', b'\n')
    text = text.strip(b'\n') + b'\n'
    try:
        text = text.decode('utf-8')
        if text.startswith('\ufeff'):
            text = text[len('\ufeff'):]
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
            (lx, absfn, val, len(''.join(ntext)))
        tokens.append((type, val))
    t2 = time.time()
    STATS[os.path.basename(absfn)] = \
        (len(text), 1000 * (t2 - t1), 1000 * (t2 - t1) / len(text))
    if ''.join(ntext) != text:
        print('\n'.join(difflib.unified_diff(''.join(ntext).splitlines(),
                                             text.splitlines())))
        raise AssertionError('round trip failed for ' + absfn)

    # check output against previous run if enabled
    if STORE_OUTPUT:
        # no previous output -- store it
        outfn = os.path.join(TESTDIR, 'examplefiles', 'output', filename)
        if not os.path.isfile(outfn):
            with open(outfn, 'wb') as fp:
                pickle.dump(tokens, fp)
            return
        # otherwise load it and compare
        with open(outfn, 'rb') as fp:
            stored_tokens = pickle.load(fp)
        if stored_tokens != tokens:
            f1 = pprint.pformat(stored_tokens)
            f2 = pprint.pformat(tokens)
            print('\n'.join(difflib.unified_diff(f1.splitlines(),
                                                 f2.splitlines())))
            assert False, absfn


def teardown_module():
    N = 7
    stats = list(STATS.items())
    stats.sort(key=lambda x: x[1][1])
    print('\nExample files that took longest absolute time:')
    for fn, t in stats[-N:]:
        print('%-30s  %6d chars  %8.2f ms  %7.3f ms/char' % ((fn,) + t))
    stats.sort(key=lambda x: x[1][2])
    print('\nExample files that took longest relative time:')
    for fn, t in stats[-N:]:
        print('%-30s  %6d chars  %8.2f ms  %7.3f ms/char' % ((fn,) + t))
