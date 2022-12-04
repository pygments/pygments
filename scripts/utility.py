"""
    Utility functions for test scripts
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import os


def unpack_output_file(path):
    """
    Unpack an output file into objects contining the line number, the text,
    and the token name.
    """
    from collections import namedtuple
    entry = namedtuple('OutputEntry', ['text', 'token', 'linenumber'])
    for linenumber, line in enumerate(open(path).readlines()):
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
            yield entry(text, token, linenumber + 1)


def process_output_files(root_directory, callback):
    """
    Process all output files in a directory using the provided callback.
    The callback should return `True` in case of success, `False` otherwise.

    The function returns the number of files for which the callback returned
    `False`.
    """
    errors = 0
    for dir, _, files in os.walk(root_directory):
        for file in files:
            if not file.endswith('.output'):
                continue

            path = os.path.join(dir, file)
            if not callback(path):
                errors += 1

    return errors
