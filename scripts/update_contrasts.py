#!/usr/bin/env python3
"""
    Updates tests/contrast/min_contrasts.json
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Whenever you have improved the minimum contrast of a style you should run
    this script, so that the test_contrasts.py test prevents future degredations.
"""

import os
import sys

# always prefer Pygments from source if exists
srcpath = os.path.join(os.path.dirname(__file__), "..")
if os.path.isdir(os.path.join(srcpath, "pygments")):
    sys.path.insert(0, srcpath)

import tests.contrast.test_contrasts # noqa: E402

tests.contrast.test_contrasts.test_contrasts(fail_if_improved=False)
tests.contrast.test_contrasts.update_json()
