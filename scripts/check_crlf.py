#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Checker for line endings
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Make sure each Python does not use Windows-style file endings.

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import sys
import os

if __name__ == '__main__':
    for directory in sys.argv[1:]:
        if not os.path.exists(directory):
            continue

        for root, dirs, files in os.walk(directory):
            for filename in files:
                if not filename.endswith('.py') and not filename.endswith('.bashcomp'):
                    continue

                with open(os.path.join(root, filename), 'rb') as f:
                    if b'\r\n' in f.read():
                        sys.exit(1)

    sys.exit(0)
