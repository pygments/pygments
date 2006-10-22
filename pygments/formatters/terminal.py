# -*- coding: utf-8 -*-
"""
    pygments.formatters.terminal
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Formatter for terminal output with ANSI sequences.

    :copyright: 2006 by Georg Brandl.
    :license: GNU LGPL, see LICENSE for more details.
"""

from pygments.formatter import Formatter
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic, Token
from pygments.console import ansiformat
from pygments.util import get_bool_opt


__all__ = ['TerminalFormatter']


#: Map token types to a tuple of color values for light and dark
#: backgrounds.
TERMINAL_COLORS = {
    Token:              ('',            ''),

    Comment:            ('lightgray',   'darkgray'),
    Keyword:            ('darkblue',    'blue'),
    Keyword.Type:       ('teal',        'turquoise'),
    Operator.Word:      ('purple',      'fuchsia'),
    Name.Builtin:       ('teal',        'turquoise'),
    Name.Function:      ('darkgreen',   'green'),
    Name.Namespace:     ('_teal_',      '_turquoise_'),
    Name.Class:         ('_darkgreen_', '_green_'),
    Name.Exception:     ('teal',        'turquoise'),
    Name.Decorator:     ('darkgray',    'lightgray'),
    Name.Variable:      ('darkred',     'red'),
    Name.Constant:      ('darkred',     'red'),
    Name.Attribute:     ('teal',        'turquoise'),
    Name.Tag:           ('blue',        'blue'),
    String:             ('brown',       'brown'),
    Number:             ('darkblue',    'blue'),

    Generic.Deleted:    ('red',        'red'),
    Generic.Inserted:   ('darkgreen',  'green'),
    Generic.Heading:    ('**',         '**'),
    Generic.Subheading: ('*purple*',   '*fuchsia*'),
    Generic.Error:      ('red',        'red'),

    Error:              ('_red_',      '_red_'),
}


class TerminalFormatter(Formatter):
    """
    Output plain text with coloring ANSI sequences.
    """

    def __init__(self, **options):
        """
        Accepted options:

        ``bg``
            Set to ``'light'`` or ``'dark'`` depending on the
            terminal's background.

        ``colorscheme``
            ``None`` or a dictionary mapping token types to
            ``(lightbg, darkbg)`` color names.

        ``debug``
            If true, output "<<ERROR>>" after each error token.
        """
        Formatter.__init__(self, **options)
        self.darkbg = options.get('bg', 'light') == 'dark'
        self.colorscheme = options.get('colorscheme', None) or TERMINAL_COLORS
        self.debug = get_bool_opt(options, 'debug', False)

    def format(self, tokensource, outfile):
        dbg = self.debug
        for ttype, value in tokensource:
            color = self.colorscheme.get(ttype)
            while color is None:
                ttype = ttype[:-1]
                color = self.colorscheme.get(ttype)
            if color:
                color = color[self.darkbg]
                spl = value.split('\n')
                for line in spl[:-1]:
                    if line:
                        outfile.write(ansiformat(color, line))
                    outfile.write('\n')
                if spl[-1]:
                    outfile.write(ansiformat(color, spl[-1]))
            else:
                outfile.write(value)
            if dbg and ttype is Error:
                outfile.write('<<ERROR>>')
