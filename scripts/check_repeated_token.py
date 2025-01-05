#!/usr/bin/env python
"""
    Checker for repeated tokens
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Helper script to find suspicious lexers which produce the same token
    repeatedly, i.e. for example:

    .. code::

      'd'           Text
      'a'           Text
      't'           Text
      'a'           Text
      'b'           Text
      'a'           Text
      's'           Text
      'e'           Text

    This script has two test modes: Check for tokens repeating more often than
    a given threshold, and exclude anything but single-character tokens.
    Repeated single-character tokens are quite problematic as they result in
    bloated output and are usually an indication that someone is missing
    a + or * in the regex.

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import argparse
import sys

from utility import unpack_output_file, process_output_files


def check_file(path, threshold, single_only):
    current_token = ''
    current_token_repeat_count = 1

    for value, token, linenumber in unpack_output_file(path):
        if single_only and len(value) > 1:
            token = ''
            current_token_repeat_count = 1
            continue

        if token != current_token:
            current_token = token
            current_token_repeat_count = 1
        else:
            current_token_repeat_count += 1

        if current_token_repeat_count > threshold:
            print(f'{path}:{linenumber}')
            return False

    return True


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('TEST_ROOT',
                        help='Root directory containing the tests')
    parser.add_argument('-t', '--threshold', type=int, default=5,
                        help='Warn if a token repeats itself more often then '
                             'this number.')
    parser.add_argument('-s', '--single', action='store_true', default=False,
                        help='Only look at tokens matching a single character')
    args = parser.parse_args()

    def check_file_callback(path):
        return check_file(path, args.threshold, args.single)

    if process_output_files(args.TEST_ROOT, check_file_callback) > 0:
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
