# -*- coding: utf-8 -*-
"""
    pygments.formatter
    ~~~~~~~~~~~~~~~~~~

    Base formatter class.

    :copyright: 2006 by Georg Brandl, Armin Ronacher.
    :license: GNU LGPL, see LICENSE for more details.
"""

from pygments.util import get_bool_opt
from pygments.styles import get_style_by_name

__all__ = ['Formatter']


def _lookup_style(style):
    if isinstance(style, basestring):
        return get_style_by_name(style)
    return style


class Formatter(object):
    """
    Converts a token stream to text.

    Options accepted:

    ``style``
        The style to use, can be a string or a Style subclass
        (default: "default"). Not used by e.g. the
        TerminalFormatter.
    ``full``
        Tells the formatter to output a "full" document, i.e.
        a complete self-contained document. This doesn't have
        any effect for some formatters (default: false).
    ``title``
        If ``full`` is true, the title that should be used to
        caption the document (default: '').
    """

    def __init__(self, **options):
        self.style = _lookup_style(options.get('style', 'default'))
        self.full  = get_bool_opt(options, 'full', False)
        self.title = options.get('title', '')
        self.options = options

    def get_style_defs(self, arg=''):
        """
        Return the style definitions for the current style as a string.

        ``arg`` is an additional argument whose meaning depends on the
        formatter used.
        """
        return ''

    def format(self, tokensource, outfile):
        """
        Format ``tokensource``, an iterable of ``(tokentype, tokenstring)``
        tuples and write it into ``outfile``.
        """
        raise NotImplementedError()
