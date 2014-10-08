# -*- coding: utf-8 -*-
"""
    Pygments unit tests
    ~~~~~~~~~~~~~~~~~~

    Usage::

        python run.py [testfile ...]


    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import print_function

import os
import sys

# only find tests in this directory
if os.path.dirname(__file__):
    os.chdir(os.path.dirname(__file__))


try:
    import nose
except ImportError:
    print('nose is required to run the Pygments test suite')
    sys.exit(1)

try:
    # make sure the current source is first on sys.path
    sys.path.insert(0, '..')
    import pygments
except SyntaxError as err:
    print('Syntax error: %s' % err)
    sys.exit(1)
except ImportError as err:
    print('Cannot find Pygments to test: %s' % err)
    sys.exit(1)
else:
    print('Pygments %s test suite running (Python %s)...' %
          (pygments.__version__, sys.version.split()[0]))

nose.main()
