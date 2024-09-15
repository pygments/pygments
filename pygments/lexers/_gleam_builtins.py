"""
    pygments.lexers._gleam_builtins
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Gleam builtins.

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

# Gleam keywords
KEYWORDS = [
    'as', 'assert', 'case', 'const', 'external', 'fn', 'if', 'import',
    'let', 'opaque', 'pub', 'todo', 'try', 'type', 'use', 'module'
]

# Gleam built-in types
BUILTINS = [
    'Int', 'Float', 'Bool', 'String', 'List', 'Result', 'Option', 'Iterator'
]

# Gleam constants
CONSTANTS = [
    'Nil', 'Ok', 'Error', 'Stop', 'Continue'
]
