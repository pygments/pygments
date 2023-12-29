"""
    pygments.styles.cds
    ~~~~~~~~~~~~~~~~~~~~

    Eclipse like style for CDS docs.

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
    Number, Operator, Punctuation


__all__ = ['CDSStyle']


class CDSStyle(Style):
    name = 'cds'

    styles = {
        Comment.Decorator:      '#335CA2',
        Comment.Single:         '#888',
        Keyword:                'bold #7F0074',
        Operator:               '#7F0074',
        Punctuation:            '#7F0074',
        Name.Variable:          '#000',
        Name.Builtin:           'italic #000',
        Number:                 '#3af',
        String:                 '#5a2',

        Error:                  '#F00',
    }
