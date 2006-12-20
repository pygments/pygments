# -*- coding: utf-8 -*-
"""
    pygments.formatters.latex
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Formatter for LaTeX fancyvrb output.

    :copyright: 2006 by Georg Brandl.
    :license: BSD, see LICENSE for more details.
"""
import cStringIO

from pygments.formatter import Formatter
from pygments.token import Token
from pygments.util import get_bool_opt, get_int_opt


__all__ = ['LatexFormatter']


def escape_tex(text):
    return text.replace('@', '\x00').    \
                replace('[', '\x01').    \
                replace(']', '\x02').    \
                replace('\x00', '@at[]').\
                replace('\x01', '@lb[]').\
                replace('\x02', '@rb[]')


DOC_TEMPLATE = r'''
\documentclass{%(docclass)s}
\usepackage{fancyvrb}
\usepackage{color}
\usepackage[%(encoding)s]{inputenc}
%(preamble)s

%(styledefs)s

\begin{document}

\section*{%(title)s}

%(code)s
\end{document}
'''


class LatexFormatter(Formatter):
    """
    Output LaTeX "color" and "fancyvrb" control sequences.
    """

    def __init__(self, **options):
        """
        Additional options accepted:

        ``docclass``
            If ``full`` is true, this is the document class to use (default: 'article').
        ``preamble``
            If ``full`` is true, this can be further preamble commands (default: '').
        ``linenos``
            If true, output line numbers (default: False).
        ``linenostart``
            The line number for the first line (default: 1).
        ``linenostep``
            If set to a number n > 1, only every nth line number is printed (default: 1).
        ``verboptions``
            Additional options given to the Verbatim environment (default: '').
        ``nobackground``
            If set to ``True`` the formatter won't output the background color
            for the overall element (default: ``False``)
            Note that light colors on dark background with this option disabled
            won't be readable very good.
        """
        Formatter.__init__(self, **options)
        self.docclass = options.get('docclass', 'article')
        self.preamble = options.get('preamble', '')
        self.linenos = get_bool_opt(options, 'linenos', False)
        self.linenostart = abs(get_int_opt(options, 'linenostart', 1))
        self.linenostep = abs(get_int_opt(options, 'linenostep', 1))
        self.verboptions = options.get('verboptions', '')
        self.nobackground = get_bool_opt(options, 'nobackground', False)

        self._create_stylecmds()


    def _create_stylecmds(self):
        t2c = self.ttype2cmd = {Token: ''}
        c2d = self.cmd2def = {}

        letters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        first = iter(letters)
        second = iter(letters)
        firstl = first.next()

        def rgbcolor(col):
            if col:
                return ','.join(['%.2f' %(int(col[i] + col[i + 1], 16) / 255.0)
                                 for i in (0, 2, 4)])
            else:
                return '1,1,1'

        for ttype, ndef in self.style:
            cmndef = '#1'
            if ndef['bold']:
                cmndef = r'\textbf{' + cmndef + '}'
            if ndef['italic']:
                cmndef = r'\textit{' + cmndef + '}'
            if ndef['underline']:
                cmndef = r'\underline{' + cmndef + '}'
            if ndef['color']:
                cmndef = r'\textcolor[rgb]{%s}{%s}' % (
                    rgbcolor(ndef['color']),
                    cmndef
                )
            if ndef['border']:
                cmndef = r'\fcolorbox[rgb]{%s}{%s}{%s}' % (
                    rgbcolor(ndef['border']),
                    rgbcolor(ndef['bgcolor']),
                    cmndef
                )
            elif ndef['bgcolor']:
                cmndef = r'\colorbox[rgb]{%s}{%s}' % (
                    rgbcolor(ndef['bgcolor']),
                    cmndef
                )
            if cmndef == '#1':
                continue
            try:
                alias = 'C' + firstl + second.next()
            except StopIteration:
                firstl = first.next()
                second = iter(letters)
                alias = 'C' + firstl + second.next()
            t2c[ttype] = alias
            c2d[alias] = cmndef

    def get_style_defs(self, arg=''):
        """
        Return the \\newcommand sequences needed to define the commands
        used to format text in the verbatim environment. If ``arg`` is
        given and true, use \\renewcommand instead.
        """
        nc = (arg and r'\renewcommand' or r'\newcommand')
        return '%s\\at{@}\n%s\\lb{[}\n%s\\rb{]}\n' % (nc, nc, nc) + \
               '\n'.join(['%s\\%s[1]{%s}' % (nc, alias, cmndef)
                          for alias, cmndef in self.cmd2def.iteritems()
                          if cmndef != '#1'])

    def format(self, tokensource, outfile):
        # TODO: add support for background colors
        enc = self.encoding

        if self.full:
            realoutfile = outfile
            outfile = cStringIO.StringIO()

        outfile.write(r'\begin{Verbatim}[commandchars=@\[\]')
        if self.linenos:
            start, step = self.linenostart, self.linenostep
            outfile.write(',numbers=left' +
                          (start and ',firstnumber=%d' % start or '') +
                          (step and ',stepnumber=%d' % step or ''))
        if self.verboptions:
            outfile.write(',' + self.verboptions)
        outfile.write(']\n')

        for ttype, value in tokensource:
            if enc:
                value = value.encode(enc)
            value = escape_tex(value)
            cmd = self.ttype2cmd.get(ttype)
            while cmd is None:
                ttype = ttype.parent
                cmd = self.ttype2cmd.get(ttype)
            if cmd:
                spl = value.split('\n')
                for line in spl[:-1]:
                    if line:
                        outfile.write("@%s[%s]" % (cmd, line))
                    outfile.write('\n')
                if spl[-1]:
                    outfile.write("@%s[%s]" % (cmd, spl[-1]))
            else:
                outfile.write(value)

        outfile.write('\n\\end{Verbatim}\n')

        if self.full:
            realoutfile.write(DOC_TEMPLATE %
                dict(docclass  = self.docclass,
                     preamble  = self.preamble,
                     title     = self.title,
                     encoding  = self.encoding,
                     styledefs = self.get_style_defs(),
                     code      = outfile.getvalue()))
