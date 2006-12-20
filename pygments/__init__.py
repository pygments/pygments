# -*- coding: utf-8 -*-
"""
    Pygments
    ~~~~~~~~

    Pygments is a syntax highlighting package written in Python.

    It aims to be a generic syntax highlighter for general use in all
    kinds of software such as forum systems, wikis or other applications
    that need to prettify source code. Highlights are:

    * a wide range of common languages and markup formats is supported
    * special attention is paid to details, increasing quality by a fair amount
    * support for new languages and formats are added easily
    * a number of output formats, presently HTML, LaTeX and ANSI sequences
    * it is usable as a command-line tool and as a library
    * ... and it highlights even Brainfuck!

    The `Pygments trunk <http://trac.pocoo.org/repos/pygments/trunk#egg=Pygments-dev>`__
    is installable via *easy_install* with ``easy_install Pygments==dev``.

    :copyright: 2006 by Georg Brandl, Armin Ronacher, Lukas Meuser and others.
    :license: BSD, see LICENSE for more details.
"""

__version__ = '0.6'
__docformat__ = 'restructuredtext'
__license__ = 'BSD License'
__author__ = 'Georg Brandl <g.brandl@gmx.net>'
__url__ = 'http://pygments.pocoo.org/'

__all__ = ['lex', 'format', 'highlight']


import sys, os
from StringIO import StringIO
from cStringIO import StringIO as CStringIO


def lex(code, lexer):
    """
    Lex ``code`` with ``lexer`` and return an iterable of tokens.
    """
    return lexer.get_tokens(code)


def format(tokens, formatter, outfile=None):
    """
    Format a tokenlist ``tokens`` with the formatter ``formatter``.

    If ``outfile`` is given and a valid file object (an object
    with a ``write`` method), the result will be written to it, otherwise
    it is returned as a string.
    """
    if not outfile:
        # if we want Unicode output, we have to use Python StringIO
        realoutfile = formatter.encoding and CStringIO() or StringIO()
        formatter.format(tokens, realoutfile)
        return realoutfile.getvalue()
    else:
        formatter.format(tokens, outfile)


def highlight(code, lexer, formatter, outfile=None):
    """
    Lex ``code`` with ``lexer`` and format it with the formatter
    ``formatter``.

    If ``outfile`` is given and a valid file object (an object
    with a ``write`` method), the result will be written to it, otherwise
    it is returned as a string.
    """
    return format(lex(code, lexer), formatter, outfile)


def cmdline_main(args):
    """
    Make pygments usable as a command line utility.
    """
    from pygments.cmdline import main
    return main(args)


if __name__ == '__main__':
    from pygments.cmdline import main
    sys.exit(main(sys.argv))
