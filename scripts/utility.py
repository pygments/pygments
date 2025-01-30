"""
    Utility functions for test scripts
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import os
import os.path


def unpack_output_file(path):
    """
    Unpack an output file into objects contining the line number, the text,
    and the token name. The output file can be either a ``.output`` file
    containing a token stream, or a ``.txt`` with input and tokens.
    """
    from collections import namedtuple
    entry = namedtuple('OutputEntry', ['text', 'token', 'linenumber'])

    skip_until_tokens = path.endswith('.txt')

    for linenumber, line in enumerate(open(path, encoding='utf-8').readlines()):
        line = line.strip()
        if not line:
            continue

        if skip_until_tokens:
            if line != '---tokens---':
                continue

            skip_until_tokens = False

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
    Process all output (i.e. .output and .txt files for snippets) files
    in a directory tree using the provided callback.
    The callback should return ``True`` in case of success, ``False``
    otherwise.

    The function returns the number of files for which the callback returned
    ``False``.
    """
    errors = 0
    for directory, _, files in os.walk(root_directory):
        for file in files:
            _, ext = os.path.splitext(file)

            if ext not in {'.txt', '.output'}:
                continue

            path = os.path.join(directory, file)
            if not callback(path):
                errors += 1

    return errors
