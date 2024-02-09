"""
    pygments.styles.onedark_vivid
    ~~~~~~~~~~~~~~~~~~~~~~~

    One Dark Vivid Theme for Pygments by Tristan Partin (https://github.com/tristan957).

    Inspired by OneDark-Pro for the code editor Visual Studio Code
    (https://github.com/Binaryify/OneDark-Pro).

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Token


__all__ = ['OneDarkVividStyle']


class OneDarkVividStyle(Style):
    """
    Theme inspired by One Dark Pro for Atom.

    .. versionadded:: 2.18
    """
    name = 'one-dark-vivid'

    background_color = '#282C34'

    styles = {
        Token:                  '#ABB2BF',

        Punctuation:            '#ABB2BF',
        Punctuation.Marker:     '#ABB2BF',

        Keyword:                '#D55FDE',
        Keyword.Constant:       '#E5C07B',
        Keyword.Declaration:    '#D55FDE',
        Keyword.Namespace:      '#D55FDE',
        Keyword.Reserved:       '#D55FDE',
        Keyword.Type:           '#E5C07B',

        Name:                   '#EF596F',
        Name.Attribute:         '#EF596F',
        Name.Builtin:           '#E5C07B',
        Name.Class:             '#E5C07B',
        Name.Function:          'bold #61AFEF',
        Name.Function.Magic:    'bold #2BBAC5',
        Name.Other:             '#EF596F',
        Name.Tag:               '#EF596F',
        Name.Decorator:         '#61AFEF',
        Name.Variable.Class:    '',

        String:                 '#89CA78',

        Number:                 '#D19A66',

        Operator:               '#2BBAC5',

        Comment:                '#7F848E'
    }
