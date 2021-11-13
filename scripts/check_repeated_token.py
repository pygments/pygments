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
    bloated output and are usually an indication that someone is missing a + or *
    in the regex. 

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import argparse
import os
import sys


def unpack_file(path):
    """Unpack a file into text, token pairs."""
    from collections import namedtuple
    pair = namedtuple('TextTokenPair', ['text', 'token'])
    for line in open(path).readlines():
        line = line.strip()
        if line:
            # Line can start with ' or ", so let's check which one it is
            # and find the matching one
            quotation_start = 0
            quotation_end = line.rfind(line[0])
            text = line[quotation_start+1:quotation_end]
            token = line.split()[-1]
            text = text.replace('\\n', '\n')
            text = text.replace('\\t', '\t')
            yield pair(text, token)


def check_file(path, threshold, single_only):
    current_token = ''
    current_token_repeat_count = 1

    is_suspicious = False

    for value, token in unpack_file(path):
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
            is_suspicious = True
            break

    if is_suspicious:
        print(path)

    return not is_suspicious
        

def main(args):
    errors = 0
    for dir, _, files in os.walk(args.TEST_ROOT):
        for file in files:
            if not file.endswith('.output'):
                continue
            
            path = os.path.join(dir, file)
            if not check_file(path, args.threshold, args.single):
                errors += 1

    if errors > 0:
        return 1
    return 0


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('TEST_ROOT',
        help='Root directory containing the tests')
    parser.add_argument('-t', '--threshold', type=int, default=5,
        help='Warn if a token repeats itself more often then this number.')
    parser.add_argument('-s', '--single', action='store_true', default=False,
        help='Only look at tokens matching a single character')
    args = parser.parse_args()
    sys.exit(main(args))