"""
    pygments.styles.abap_eclipse
    ~~~~~~~~~~~~~~~~~~~~

    ABAP Development Tools (ADT) for Eclipse like style.

    :copyright: Copyright 2006-2023 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
    Number, Operator, Punctuation


__all__ = ['AbapEclipseStyle']


class AbapEclipseStyle(Style):
    name = 'abap_eclipse'

    styles = {
        Comment:                '#888',
        Comment.Special:        '#888',
        Keyword:                '#00f',
        Operator.Word:          '#00f',
        Operator.Primary:       '#00f',
        Operator.Secondary:     '#00f',
        Punctuation.Primary:    '#00f',
        Punctuation.Secondary:  '#00f',
        Name:                   '#000',
        Number:                 '#3af',
        String:                 '#5a2',

        Error:                  '#F00',
    }
