"""
    pygments._shtab
    ~~~~~~~~~~~~~~~

    Fake shtab.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
from argparse import Action

FILE = None
DIRECTORY = DIR = None


class PrintCompletionAction(Action):
    def __call__(self, parser, namespace, values, option_string=None):
        print(
            "Please install completion support for Pygments, e.g., "
            "'pip install pygments[completion]'"
        )
        parser.exit(0)


def add_argument_to(parser, *args, **kwargs):
    Action.complete = None
    parser.add_argument(
        "--print-completion",
        choices=["bash", "zsh", "tcsh"],
        action=PrintCompletionAction,
        help="print shell completion script",
    )
    return parser
