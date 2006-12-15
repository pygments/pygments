# -*- coding: utf-8 -*-
"""
    pygments.formatters.rtf
    ~~~~~~~~~~~~~~~~~~~~~~~

    A formatter that generates RTF files.

    :copyright: 2006 by Armin Ronacher.
    :license: BSD, see LICENSE for more details.
"""
import cStringIO

from pygments.formatter import Formatter
from pygments.token import Token
from pygments.util import get_bool_opt, get_int_opt


__all__ = ['RtfFormatter']


class RtfFormatter(Formatter):

    def __init__(self, **options):
        """
        Additional options accepted:

        ``fontface``
            Name of the font used. Could for example be ``'Courier New'``
            to further specify the default which is ``'\fmodern'``. The RTF
            specification claims that ``\fmodern`` are "Fixed-pitch serif
            and sans serif fonts". Hope every RTF implementation thinks
            the same about modern...


        """
        Formatter.__init__(self, **options)
        self.fontface = options.get('fontface') or ''

    def _escape(self, text):
        return text.replace('\\', '\\\\') \
                   .replace('{', '\\{') \
                   .replace('}', '\\}')

    def _escape_text(self, text):
        # empty strings, should give a small performance improvment
        if not text:
            return ''

        # escape text
        text = self._escape(text)

        # byte strings
        if isinstance(text, str):
            for c in xrange(128, 256):
                text = text.replace(chr(c), '\\\'%x' % c)
        
        # unicode strings
        elif isinstance(text, unicode):
            buf = []
            for c in text:
                o = ord(c)
                if o > 128:
                    ansic = c.encode('iso-8859-1', 'ignore') or '?'
                    if ord(ansic) > 128:
                        ansic = '\\\'%x' % ord(ansic)
                    buf.append(r'\ud{\u%d%s}' % (o, ansic))
                else:
                    buf.append(str(c))
            text = ''.join(buf)

        return text.replace('\n', '\\par\n')

    def format(self, tokensource, outfile):
        outfile.write(r'{\rtf1\ansi\deff0'
                      r'{\fonttbl{\f0\fmodern\fprq1\fcharset0%s;}}{\colortbl;' %
                      (self.fontface and ' ' + self._escape(self.fontface) or ''))
        
        # convert colors and save them in a mapping to access them later.
        color_mapping = {}
        offset = 1
        for _, style in self.style:
            for color in style['color'], style['bgcolor'], style['border']:
                if not color or color in color_mapping:
                    continue
                color_mapping[color] = offset
                outfile.write(r'\red%d\green%d\blue%d;' % (
                    int(color[0:2], 16),
                    int(color[2:4], 16),
                    int(color[4:6], 16)
                ))
                offset += 1
        outfile.write(r'}\f0')

        # highlight stream
        for ttype, value in tokensource:
            try:
                style = self.style.style_for_token(ttype)
            except KeyError:
                start = ''
            else:
                buf = []
                if style['bgcolor']:
                    buf.append(r'\cb%d' % color_mapping[style['bgcolor']])
                if style['color']:
                    buf.append(r'\cf%d' % color_mapping[style['color']])
                if style['bold']:
                    buf.append(r'\b')
                if style['italic']:
                    buf.append(r'\i')
                if style['underline']:
                    buf.append(r'\ul')
                if style['border']:
                    buf.append(r'\chbrdr\chcfpat%d' % color_mapping[style['border']])
                start = ''.join(buf)
            if start:
                outfile.write('{%s ' % start)
            outfile.write(self._escape_text(value))
            if start:
                outfile.write('}')

        outfile.write('}')
