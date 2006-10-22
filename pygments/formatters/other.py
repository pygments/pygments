# -*- coding: utf-8 -*-
"""
    pygments.formatters.other
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Other formatters: NullFormatter, RawTokenFormatter.

    :copyright: 2006 by Georg Brandl, Armin Ronacher.
    :license: GNU LGPL, see LICENSE for more details.
"""

from pygments.formatter import Formatter
import StringIO

__all__ = ['NullFormatter', 'RawTokenFormatter']


class NullFormatter(Formatter):
    """
    Output the text unchanged without any formatting.
    """
    def format(self, tokensource, outfile):
        for ttype, value in tokensource:
            outfile.write(value)


class RawTokenFormatter(Formatter):
    """
    Output a raw token representation for storing token streams.

    The format is ``tokentype<TAB>repr(tokenstring)``

    Additional options accepted:

    ``compress``
        If set to "gz" or "bz2", compress the token stream with
        the given compression algorithm (default: '').
    """

    def __init__(self, **options):
        Formatter.__init__(self, **options)
        self.compress = options.get('compress', '')

    def format(self, tokensource, outfile):
        if self.compress == 'gz':
            import gzip
            outfile = gzip.GzipFile('', 'wb', 9, outfile)
            write = outfile.write
            flush = outfile.flush
        elif self.compress == 'bz2':
            import bz2
            compressor = bz2.BZ2Compressor(9)
            def write(text):
                outfile.write(compressor.compress(text))
            def flush():
                outfile.write(compressor.flush())
                outfile.flush()
        else:
            write = outfile.write
            flush = outfile.flush

        lasttype = None
        lastval = ''
        for ttype, value in tokensource:
            if ttype is lasttype:
                lastval += value
            else:
                if lasttype:
                    write("%s\t%r\n" % (lasttype, lastval))
                lastval = value
                lasttype = ttype
        write("%s\t%r\n" % (lasttype, lastval))
        flush()
