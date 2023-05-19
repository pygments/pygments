"""
    pygments.formatters.bbcode
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    BBcode formatter.

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""


from pygments.formatter import Formatter
from pygments.util import get_bool_opt
import re
__all__ = ['BBCodeFormatter']


class BBCodeFormatter(Formatter):
    """
    Format tokens with BBcodes. These formatting codes are used by many
    bulletin boards, so you can highlight your sourcecode with pygments before
    posting it there.

    This formatter has no support for background colors and borders, as there
    are no common BBcode tags for that.

    Some board systems (e.g. phpBB) don't support colors in their [code] tag,
    so you can't use the highlighting together with that tag.
    Text in a [code] tag usually is shown with a monospace font (which this
    formatter can do with the ``monofont`` option) and no spaces (which you
    need for indentation) are removed.

    Additional options accepted:

    `style`
        The style to use, can be a string or a Style subclass (default:
        ``'default'``).

    `codetag`
        If set to true, put the output into ``[code]`` tags (default:
        ``false``)

    `monofont`
        If set to true, add a tag to show the code with a monospace font
        (default: ``false``).
    """
    name = 'BBCode'
    aliases = ['bbcode', 'bb']
    filenames = []

    def __init__(self, **options):
        Formatter.__init__(self, **options)
        self._code = get_bool_opt(options, 'codetag', False)
        self._mono = get_bool_opt(options, 'monofont', False)

        self.styles = {}
        self._make_styles()

    def _make_styles(self):
        for ttype, ndef in self.style:
            start = end = ''
            if ndef['color']:
                start += '[color=#%s]' % ndef['color']
                end = '[/color]' + end
            if ndef['bold']:
                start += '[b]'
                end = '[/b]' + end
            if ndef['italic']:
                start += '[i]'
                end = '[/i]' + end
            if ndef['underline']:
                start += '[u]'
                end = '[/u]' + end
            # there are no common BBcodes for background-color and border

            self.styles[ttype] = start, end

def format_unencoded(self, tokensource, outfile):
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

    # Validate the produced XHTML
    outfile.seek(0)
    xhtml = outfile.getvalue()

    # Validate XHTML structure
    if not self.validate_xhtml_structure(xhtml):
        raise ValueError("The produced XHTML is not correctly formatted.")

    # Validate HTML tags and attributes
    if not self.validate_html_tags_attributes(xhtml):
        raise ValueError("The produced XHTML contains invalid HTML tags or attributes.")

def validate_xhtml_structure(self, xhtml):
    # Ensure opening and closing tags match for all elements
    stack = []
    pos = 0
    while pos < len(xhtml):
        match = re.search(r"<([^\s>/]+)", xhtml[pos:])
        if match:
            tag = match.group(1)
            if tag.startswith("/"):
                if not stack or stack[-1] != tag[1:]:
                    return False
                stack.pop()
            else:
                stack.append(tag)
            pos += match.end()
        else:
            pos += 1

    return not stack

def validate_html_tags_attributes(self, xhtml):
    # Ensure all HTML tags are <span> and have only the class attribute
    pattern = r"<span\s+class=[\"'][^\"']*?[\"']\s*>"
    matches = re.findall(pattern, xhtml)
    return ''.join(matches) == xhtml
