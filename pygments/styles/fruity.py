# -*- coding: utf-8 -*-
"""
    pygments.styles.fruity
    ~~~~~~~~~~~~~~~~~~~~~~

    pygments version of my "native" vim theme.

    :copyright: 2007 by Armin Ronacher.
    :license: BSD, see LICENSE for more details.
"""

from pykleur.style import Style
from pykleur.token import Comment, Name, Keyword, Generic, Number, Operator, String

class FruityStyle(Style):
    default_style = 'ffffff bg:111111'
    styles = {
        Generic.Output:     '444444 bg:222222',
        Keyword:            'fb660a bold',
        Number:             '0086f7 bold',
        Name.Tag:           'fb660a bold',
        Comment:            '008800 bg:0f140f italic',
        Name.Attribute:     'ff0086 bold',
        String:             '0086d2',
        Name.Function:      'ff0086 bold',
        Generic.Heading:    'ffffff bold',
        Keyword.Type:       'cdcaa9 bold',
        Generic.Subheading: 'ffffff bold',
        Name.Constant:      '0086d2',
        Comment.Preproc:    'ff0007 bold'
    }
