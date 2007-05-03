# -*- coding: utf-8 -*-
"""
    pygments.styles.borland
    ~~~~~~~~~~~~~~~~~~~~~~~

    Style similar to the style used in the borland ides.

    :copyright: 2006-2007 by Armin Ronacher.
    :license: BSD, see LICENSE for more details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic, Whitespace


class BorlandStyle(Style):
    """
    Style similar to the style used in the borland IDEs.
    """

    default_style = ''

    styles = {
        Whitespace:             '#bbbbbb',

        Comment:                'italic #008800',
        Comment.Preproc:        'noitalic',
        Comment.Special:        'noitalic bold',

        String:                 '#0000FF',
        Number:                 '#0000FF',
        Keyword:                'bold',
        Operator.Word:          'bold',
        Name.Tag:               'bold',
        Name.Attribute:         'italic',

        Generic.Heading:        '#999999',
        Generic.Subheading:     '#aaaaaa',
        Generic.Deleted:        'bg:#ffdddd #000000',
        Generic.Inserted:       'bg:#ddffdd #000000',
        Generic.Error:          '#aa0000',
        Generic.Emph:           'italic',
        Generic.Strong:         'bold',
        Generic.Prompt:         '#555555',
        Generic.Output:         '#888888',
        Generic.Traceback:      '#aa0000',

        Error:                  'bg:#e3d2d2 #a61717'
    }
