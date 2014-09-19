# -*- coding: utf-8 -*-
"""
    pygments.lexers.console
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for misc console output.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer
from pygments.token import Generic, Comment, String, Text

__all__ = ['VCTreeStatusLexer']


class VCTreeStatusLexer(RegexLexer):
    """
    For colorizing output of version control status commans, like "hg
    status" or "svn status".

    .. versionadded:: 2.0
    """
    name = 'VCTreeStatus'
    aliases = ['vctreestatus']
    filenames = []
    mimetypes = []

    tokens = {
        'root': [
            (r'^A  \+  C\s+', Generic.Error),
            (r'^A\s+\+?\s+', String),
            (r'^M\s+', Generic.Inserted),
            (r'^C\s+', Generic.Error),
            (r'^D\s+', Generic.Deleted),
            (r'^[\?!]\s+', Comment.Preproc),
            (r'      >\s+.*\n', Comment.Preproc),
            (r'.*\n', Text)
        ]
    }
