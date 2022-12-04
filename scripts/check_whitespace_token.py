#!/usr/bin/env python
"""
    Checker for whitespace tokens
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Helper script to find whitespace which is not of token type `Whitespace`

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import argparse
import sys
import re

from utility import unpack_output_file, process_output_files


def check_file(path):
    whitespace_re = re.compile('\s+')

    for value, token, linenumber in unpack_output_file(path):
        if whitespace_re.fullmatch(value) and 'Whitespace' not in token:
            print(f'{path}:{linenumber}')
            return False

    return True


def main(args):
    if process_output_files(args.TEST_ROOT, check_file) > 0:
        return 1
    return 0


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('TEST_ROOT',
                        help='Root directory containing the tests')
    args = parser.parse_args()
    sys.exit(main(args))
