"""
    detect_missing_analyse_text
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import sys

from pygments.lexers import get_all_lexers, find_lexer_class
from pygments.lexer import Lexer

import argparse


def main(args):
    uses = {}

    for name, aliases, filenames, mimetypes in get_all_lexers(plugins=False):
        cls = find_lexer_class(name)
        if not cls.aliases and not args.skip_no_aliases:
            print(cls, "has no aliases")
        for f in filenames:
            uses.setdefault(f, []).append(cls)

    ret = 0
    for k, v in uses.items():
        if len(v) > 1:
            # print("Multiple for", k, v)
            for i in v:
                if i.analyse_text is None:
                    print(i, "has a None analyse_text")
                    ret |= 1
                elif Lexer.analyse_text.__doc__ == i.analyse_text.__doc__:
                    print(i, "needs analyse_text, multiple lexers for", k)
                    ret |= 2
    return ret


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--skip-no-aliases',
                        help='Skip checks for a lexer with no aliases',
                        action='store_true',
                        default=False)
    args = parser.parse_args()
    sys.exit(main(args))
