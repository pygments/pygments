# -*- coding: utf-8 -*-
"""
    pygments.formatters.other
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Other formatters: NullFormatter, RawTokenFormatter.

    :copyright: 2006 by Georg Brandl, Armin Ronacher.
    :license: BSD, see LICENSE for more details.
"""

from pygments.formatter import Formatter


__all__ = ['NullFormatter', 'RawTokenFormatter']


class NullFormatter(Formatter):
    """
    Output the text unchanged without any formatting.
    """
    def format(self, tokensource, outfile):
        enc = self.encoding
        for ttype, value in tokensource:
            if enc:
                outfile.write(value.encode(enc))
            else:
                outfile.write(value)


class RawTokenFormatter(Formatter):
    r"""
    Formats tokens as a raw representation for storing token streams.

    The format is ``tokentype<TAB>repr(tokenstring)\n``. The output can later
    be converted to a token stream with the `RawTokenLexer`, described in the
    `lexer list <lexers.txt>`_.

    Only one option is accepted:

    `compress`
        If set to ``'gz'`` or ``'bz2'``, compress the output with the given
        compression algorithm after encoding (default: ``''``).
    """

    unicodeoutput = False

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
        lastval = u''
        for ttype, value in tokensource:
            value = repr(value)
            if ttype is lasttype:
                lastval += value
            else:
                if lasttype:
                    write("%s\t%s\n" % (lasttype, lastval))
                lastval = value
                lasttype = ttype
        write("%s\t%s\n" % (lasttype, lastval))
        flush()
