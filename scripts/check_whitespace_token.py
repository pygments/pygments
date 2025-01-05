#!/usr/bin/env python
"""
    Checker for whitespace tokens
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Helper script to find whitespace which is not of token type `Whitespace`

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import argparse
import sys
import re

from utility import unpack_output_file, process_output_files


def check_file(path):
    whitespace_re = re.compile('\\s+')

    for value, token, linenumber in unpack_output_file(path):
        if whitespace_re.fullmatch(value):
            # We allow " " if it's inside a Literal.String for example
            if 'Literal' in token:
                continue

            # If whitespace is part of a comment, we accept that as well,
            # as comments may be similarly highlighted to literals
            if 'Comment' in token:
                continue

            if 'Whitespace' in token:
                continue

            print(f'{path}:{linenumber}')
            return False

        if 'Whitespace' in token and value != '':
            print(f'{path}:{linenumber} - '
                  'Incorrectly marked as whitespace')
            return False
    return True


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('TEST_ROOT',
                        help='Root directory containing the tests')
    args = parser.parse_args()

    if process_output_files(args.TEST_ROOT, check_file) > 0:
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
