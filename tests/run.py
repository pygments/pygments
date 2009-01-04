# -*- coding: utf-8 -*-
"""
    Pygments unit tests
    ~~~~~~~~~~~~~~~~~~

    Usage::

        python run.py [testfile ...]


    :copyright: Copyright 2006-2009 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import sys

try:
    import nose
except ImportError:
    print >> sys.stderr, "nose is required to run the test suites"
    sys.exit(1)

nose.main()
