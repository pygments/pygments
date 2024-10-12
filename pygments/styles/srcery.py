"""
    pygments.styles.srcery
    ~~~~~~~~~~~~~~~~~~~~~~~

    pygments version of the "srcery" vim theme.
    https://github.com/srcery-colors/srcery-vim

    :copyright: Copyright 2006-2024 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Token, Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic


__all__ = ['SrceryDarkStyle']


class SrceryDarkStyle(Style):
    """
    Pygments version of the "srcery" dark vim theme.
    """

    name = 'srcery-dark'

    background_color = '#1C1B19'
    highlight_color = '#FCE8C3'

    styles = {
        Token:              '#dddddd',

        Comment:            'italic #808080',
        Comment.PreProc:    '#2BE4D0',
        Comment.Special:    'bold italic #FCE8C3',

        Keyword:            '#F75341',
        Operator.Word:      '#F75341',

        String:             '#98BC37',
        String.Escape:      '#FF8700',

        Number:             '#98BC37',

        Name.Builtin:       '#FF8700',
        Name.Variable:      '#68A8E4',
        Name.Constant:      '#98BC37',
        Name.Class:         '#2BE4D0',
        Name.Function:      '#2BE4D0',
        Name.Namespace:     '#2BE4D0',
        Name.Exception:     '#F75341',
        Name.Tag:           '#2BE4D0',
        Name.Attribute:     '#FED06E',
        Name.Decorator:     '#F75341',

        Generic.Heading:    'bold #FCE8C3',
        Generic.Subheading: 'underline #FCE8C3',
        Generic.Deleted:    'bg:#F75341 #1C1B19',
        Generic.Inserted:   'bg:#b8bb26 #1C1B19',
        Generic.Error:      '#F75341',
        Generic.Emph:       'italic',
        Generic.Strong:     'bold',
        Generic.EmphStrong: 'bold italic',
        Generic.Prompt:     '#8A8A8A',
        # this is actually from gruvbox-dark
        Generic.Output:     '#f2e5bc',
        Generic.Traceback:  '#F75341',

        Error:              'bg:#F75341 #1C1B19'
    }
