# coding: utf-8
"""
Support for Pygments tests
"""

import os
import random


def _get_all_test_files():
    tests = []
    here = os.path.abspath(os.path.dirname(__file__))
    for dirpath, dirs, files in os.walk(here):
        tests.extend(os.path.join(dirpath, fn) for fn in files
                     if fn.startswith("test_") and fn.endswith(".py"))
        dirs[:] = [d for d in dirs if d.startswith("test_")]
    return tests

_testfiles = _get_all_test_files()

def test_file():
    """
    Randomly choose a file to test.
    """
    return random.choice(_testfiles)
