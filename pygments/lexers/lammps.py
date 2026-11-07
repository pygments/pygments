"""
    pygments.lexers.lammps
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexer for LAMMPS input scripts.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, default, include
from pygments.token import Comment, Keyword, Name, Number, Punctuation, String, Whitespace

__all__ = ['LammpsLexer']


class LammpsLexer(RegexLexer):
    """
    For LAMMPS molecular dynamics input scripts.

    This covers the command language, not LAMMPS data or trajectory files.
    """
    name = 'LAMMPS'
    url = 'https://www.lammps.org/'
    aliases = ['lammps']
    filenames = ['*.lammps', '*.lmp']
    version_added = '2.22'

    # https://docs.lammps.org/Commands_parse.html
    # Commands are the first word of a logical line. Keeping this positional
    # also recognizes commands added by optional packages and plugins.
    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'&[^\S\n]*\n', Punctuation),
            (r'#', Comment.Single, 'comment'),
            (r'''[^\s#$'"&]+''', Keyword, 'command'),
            default('arguments'),
        ],
        'command': [
            # A continuation can split the command name itself.
            (r'&[^\S\n]*\n', Punctuation),
            (r'''[^\s#$'"&]+''', Keyword),
            default(('#pop', 'arguments')),
        ],
        'arguments': [
            (r'[^\S\n]+', Whitespace),
            (r'&[^\S\n]*\n', Punctuation),
            (r'#', Comment.Single, 'comment'),
            include('quotes'),
            (r'\\\$', String.Escape),
            (r'\$', Name.Variable, 'variable-start'),
            (r'[+-]?(?:(?:[0-9]+\.[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?'
             r'|[0-9]+[eE][+-]?[0-9]+)(?=[\s#]|$)', Number.Float),
            (r'[+-]?[0-9]+(?=[\s#]|$)', Number.Integer),
            (r'''[^\s#$'"&\\]+''', Name),
            (r'[&\\]', Name),
        ],
        'variable-start': [
            (r'&[^\S\n]*\n', Punctuation),
            (r'#', Comment.Single, 'comment'),
            (r'\{', Name.Variable, ('#pop', 'braced')),
            (r'\(', Name.Variable, ('#pop', 'immediate')),
            (r'\S', Name.Variable, '#pop'),
            default('#pop'),
        ],
        'braced': [
            (r'&[^\S\n]*\n', Punctuation),
            (r'#', Comment.Single, 'comment'),
            (r'\}', Name.Variable, '#pop'),
            (r'[^}#&\n]+', Name.Variable),
            (r'&', Name.Variable),
        ],
        'immediate': [
            # Parentheses in an immediate expression can nest. Comments are
            # stripped before substitutions, while quotes protect their #.
            (r'\(', Name.Variable, '#push'),
            (r'\)', Name.Variable, '#pop'),
            (r'&[^\S\n]*\n', Punctuation),
            (r'#', Comment.Single, 'comment'),
            include('quotes'),
            (r'''[^()&\n#'"]+''', Name.Variable),
            (r'&', Name.Variable),
        ],
        'comment': [
            # Continuations are joined before comments are removed, even if
            # the first physical line starts with a comment marker.
            (r'&[^\S\n]*\n', Comment.Single),
            (r'[^&\n]+', Comment.Single),
            (r'&', Comment.Single),
            # An unmatched newline resets RegexLexer to root, regardless of
            # the nesting depth of an interrupted variable or quoted argument.
        ],
        'quotes': [
            (r'"""', String.Double, 'triple'),
            (r'"', String.Double, 'double'),
            (r"'", String.Single, 'single'),
        ],
        'single': [
            (r"'", String.Single, '#pop'),
            (r'&[^\S\n]*\n', String.Escape),
            (r"[^'&\n]+", String.Single),
            (r'&', String.Single),
        ],
        'double': [
            (r'"', String.Double, '#pop'),
            (r'&[^\S\n]*\n', String.Escape),
            (r'[^"&\n]+', String.Double),
            (r'&', String.Double),
        ],
        'triple': [
            # Triple-quoted arguments retain newlines and literal ampersands.
            # Quotes protect # and $ at the input-parser level.
            (r'"""', String.Double, '#pop'),
            (r'[^"]+', String.Double),
            (r'"{1,2}', String.Double),
        ],
    }
