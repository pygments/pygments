# -*- coding: utf-8 -*-
"""
    Pygments unit tests
    ~~~~~~~~~~~~~~~~~~

    Usage::

        python run.py [testfile ...]


    :copyright: 2006-2007 by Georg Brandl.
    :license: GNU GPL, see LICENSE for more details.
"""

import sys

try:
    import nose
except ImportError:
    print >> sys.stderr, "nose is required to run the test suites"
    sys.exit(1)

nose.main()
