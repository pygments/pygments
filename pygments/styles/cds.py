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
        Comment:                '#888',
        Keyword:                'bold #f80',
        Operator:               '#f80',
        Punctuation:            '#f80',
        Name.Variable:          '#000',
        Name.Builtin:           'italic #000',
        Number:                 '#3af',
        String:                 '#5a2',

        Error:                  '#F00',
    }
