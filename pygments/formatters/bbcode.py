# -*- coding: utf-8 -*-
"""
    pygments.formatters.bbcode
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    BBcode formatter.

    :copyright: 2006 by Lukas Meuser.
    :license: GNU LGPL, see LICENSE for more details.
"""


from pygments.formatter import Formatter
from pygments.util import get_bool_opt

__all__ = ['BBCodeFormatter']


class BBCodeFormatter(Formatter):
    """
    Output BBCode tags with appropiate colors and formatting.

    This formatter doesn't support background colors and borders, as there are
    no common BBcodes for that.
    
    Some board systems (e.g. phpBB) don't support colors in their [code] tag,
    so you can't use the highlighting together with that tag.
    Text in a [code] tag usually is shown with a monospace font (which this
    formatter can do with the ``monofont`` option) and no spaces (which you
    need for indentation) are removed.

    Additional options accepted:

    ``codetag``
        If set to true, put the output into [code] tags (default: false).

    ``monofont``
        If set to true, add a tag to show the code with a monospace font
        (default: false).
    """

    def __init__(self, **options):
        Formatter.__init__(self, **options)
        self._make_styles()
        self._code = get_bool_opt(options, 'codetag', False)
        self._mono = get_bool_opt(options, 'monofont', False)

    def _make_styles(self):
        self.styles = {}
        for token, style in self.style._styles.iteritems():
            start = end = ''
            color, bold, italic, underline, bg, border = style
            if color:
                start += '[color=#%s]' % color
                end = '[/color]' + end
            if bold:
                start += '[b]'
                end = '[/b]' + end
            if italic:
                start += '[i]'
                end = '[/i]' + end
            if underline:
                start += '[u]'
                end = '[/u]' + end
            # there are no common BBcodes for background-color and border

            self.styles[token] = start, end

    def format(self, tokensource, outfile):
        if self._code:
            outfile.write('[code]')
        if self._mono:
            outfile.write('[font=monospace]')

        lastval = ''
        lasttype = None

        for ttype, value in tokensource:
            while ttype not in self.styles:
                ttype = ttype.parent
            if ttype == lasttype:
                lastval += value
            else:
                if lastval:
                    start, end = self.styles[lasttype]
                    outfile.write(''.join((start, lastval, end)))
                lastval = value
                lasttype = ttype

        if lastval:
            start, end = self.styles[lasttype]
            outfile.write(''.join((start, lastval, end)))

        if self._mono:
            outfile.write('[/font]')
        if self._code:
            outfile.write('[/code]')
        if self._code or self._mono:
            outfile.write('\n')
