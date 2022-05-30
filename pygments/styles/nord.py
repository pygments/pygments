"""
    pygments.styles.nord
    ~~~~~~~~~~~~~~~~~~~~~~~

    pygments version of the "nord" theme by Arctic Ice Studio
    https://www.nordtheme.com/

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD-3 Clause License. See LICENSE for details
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, Number, Operator, Generic, Whitespace, Punctuation, Text


class NordStyle(Style):
    """
    Pygments version of the "nord" theme by Arctic Ice Studio
    """

    linenos = "#FF0000"
    line_number_color = "#D8DEE9"
    line_number_background_color = "#242933"
    line_number_special_color = "#242933"
    line_number_special_background_color = "#D8DEE9"

    background_color = "#242933"
    highlight_color = "#3B4252"
    default = "#d8dee9"

    # fmt: off
    styles = {
        Whitespace:                 '#d8dee9',
        Punctuation:                '#eceff4',

        Comment:                    'italic #616e87',
        Comment.Preproc:            '#5e81ac',

        Keyword:                    'bold #81a1c1',
        Keyword.Pseudo:             'nobold #81a1c1',
        Keyword.Type:               'nobold #81a1c1',

        Operator:                   'bold #81a1c1',
        Operator.Word:              'bold #81a1c1',

        Name:                       '#d8dee9',
        Name.Builtin:               '#81a1c1',
        Name.Function:              '#88c0d0',
        Name.Class:                 '#8fbcbb',
        Name.Namespace:             '#8fbcbb',
        Name.Exception:             '#bf616a',
        Name.Variable:              '#d8dee9',
        Name.Constant:              '#8fbcbb',
        Name.Entity:                '#d08770',
        Name.Attribute:             '#8fbcbb',
        Name.Tag:                   '#81a1c1',
        Name.Decorator:             '#d08770',

        String:                     '#a3be8c',
        String.Doc:                 '#616e87',
        String.Interpol:            '#a3be8c',
        String.Escape:              '#ebcb8b',
        String.Regex:               '#ebcb8b',
        String.Symbol:              '#a3be8c',
        String.Other:               '#a3be8c',

        Number:                     '#b48ead',

        Generic.Heading:            'bold #88c0d0',
        Generic.Subheading:         'bold #88c0d0',
        Generic.Deleted:            '#bf616a',
        Generic.Inserted:           '#a3be8c',
        Generic.Error:              '#bf616a',
        Generic.Emph:               'italic',
        Generic.Strong:             'bold',
        Generic.Prompt:             'bold #4c566a',
        Generic.Output:             '#d8dee9',
        Generic.Traceback:          '#bf616a',

        Error:                      '#bf616a',
        Text:                       '#d8dee9',
    }
