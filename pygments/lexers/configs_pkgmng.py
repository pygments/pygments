# -*- coding: utf-8 -*-
"""
    pygments.lexers.configs_pkgmng
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for package manager configuration file formats.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, bygroups, include, words
from pygments.token import Text, Comment, Operator, Name, \
    Punctuation, String, Keyword

__all__ = ['PkgConfigLexer', 'PacmanConfLexer']


class PkgConfigLexer(RegexLexer):
    """
    Lexer for `pkg-config
    <http://www.freedesktop.org/wiki/Software/pkg-config/>`_
    (see also `manual page <http://linux.die.net/man/1/pkg-config>`_).

    .. versionadded:: 2.1
    """

    name = 'PkgConfig'
    aliases = ['pkgconfig',]
    filenames = ['*.pc',]
    mimetypes = []

    tokens = {
        'root': [
            (r'#.*$', Comment.Single),

            # variable definitions
            (r'^(\w+)(=)', bygroups(Name.Attribute, Operator)),

            # keyword lines
            (r'^([\w.]+)(:)',
             bygroups(Name.Tag, Punctuation), 'spvalue'),

            # variable references
            include('interp'),

            # fallback
            (r'[^${}#=:\n.]+', Text),
            (r'.', Text),
        ],
        'interp': [
            # you can escape literal "$" as "$$"
            (r'\$\$', Text),

            # variable references
            (r'\$\{', String.Interpol, 'curly'),
        ],
        'curly': [
            (r'\}', String.Interpol, '#pop'),
            (r'\w+', Name.Attribute),
        ],
        'spvalue': [
            include('interp'),

            (r'#.*$', Comment.Single, '#pop'),
            (r'\n', Text, '#pop'),

            # fallback
            (r'[^${}#\n]+', Text),
            (r'.', Text),
        ],
    }


class PacmanConfLexer(RegexLexer):
    """
    Lexer for `pacman.conf
    <https://www.archlinux.org/pacman/pacman.conf.5.html>`_.

    Actually, IniLexer works almost fine for this format,
    but it yield error token. It is because pacman.conf has
    a form without assignment like:

        UseSyslog
        Color
        TotalDownload
        CheckSpace
        VerbosePkgLists

    These are flags to switch on.

    .. versionadded:: 2.1
    """

    name = 'PacmanConf'
    aliases = ['pacmanconf',]
    filenames = ['pacman.conf',]
    mimetypes = []

    tokens = {
        'root': [
            # comment
            (r'#.*$', Comment.Single),

            # section header
            (r'^\s*\[.*?\]\s*$', Keyword),

            # variable definitions
            # (Leading space is allowed...)
            (r'(\w+)(\s*)(=)',
             bygroups(Name.Attribute, Text, Operator)),

            # flags to on
            (r'^(\s*)(\w+)(\s*)$',
             bygroups(Text, Name.Attribute, Text)),

            # built-in special values
            (words((
                '$repo',  # repository
                '$arch',  # architecture
                '%o',     # outfile
                '%u',     # url
                ), suffix=r'\b'),
             Name.Variable),
            
            # fallback
            (r'.', Text),
        ],
    }
