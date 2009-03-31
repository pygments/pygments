# -*- coding: utf-8 -*-
"""
    Pygments unit tests
    ~~~~~~~~~~~~~~~~~~

    Usage::

        python run.py [testfile ...]


    :copyright: Copyright 2006-2009 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import sys, os

if sys.version_info >= (3,):
    # copy test suite over to "build/lib" and convert it
    print ('Copying and converting sources to build/lib/test...')
    from distutils.util import copydir_run_2to3
    testroot = os.path.dirname(__file__)
    newroot = os.path.join(testroot, '..', 'build/lib/test')
    copydir_run_2to3(testroot, newroot)
    os.chdir(os.path.join(newroot, '..'))

try:
    import nose
except ImportError:
    print ("nose is required to run the test suites")
    sys.exit(1)

nose.main()
