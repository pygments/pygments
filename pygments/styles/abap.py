"""
    pygments.styles.abap
    ~~~~~~~~~~~~~~~~~~~~

    ABAP workbench like style.

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
    Number, Operator, Punctuation


__all__ = ['AbapStyle']


class AbapStyle(Style):
    name = 'abap'

    styles = {
        Comment:                'italic #888',
        Comment.Special:        '#888',
        Keyword:                '#00f',
        Operator.Word:          '#00f',
        Operator.Primary:       '#808',
        Operator.Secondary:     '#888',
        Punctuation.Primary:    '#808',
        Punctuation.Secondary:  '#888',
        Name:                   '#000',
        Number:                 '#3af',
        String:                 '#5a2',

        Error:                  '#F00',
    }
