"""
    pygments.styles.nord
    ~~~~~~~~~~~~~~~~~~~~~~~

    pygments version of the "nord" theme by Arctic Ice Studio
    https://www.nordtheme.com/

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD-3 Clause License. See LICENSE for details
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic


class NordStyle(Style):
    background_color = nord0
    default = nord4

    styles = {
        Whitespace: nord4,
        Comment: f"italic {nord3_bright}",
        Comment.Preproc: nord10,
        Keyword: f"bold {nord9}",
        Keyword.Pseudo: f"nobold {nord9}",
        Keyword.Type: f"nobold {nord9}",
        Operator: nord9,
        Operator.Word: f"bold {nord9}",
        Name: nord4,
        Name.Builtin: nord9,
        Name.Function: nord8,
        Name.Class: nord7,
        Name.Namespace: nord7,
        Name.Exception: nord11,
        Name.Variable: nord4,
        Name.Constant: nord7,
        Name.Label: nord7,
        Name.Entity: nord12,
        Name.Attribute: nord7,
        Name.Tag: nord9,
        Name.Decorator: nord12,
        Punctuation: nord6,
        String: nord14,
        String.Doc: nord3_bright,
        String.Interpol: nord14,
        String.Escape: nord13,
        String.Regex: nord13,
        String.Symbol: nord14,
        String.Other: nord14,
        Number: nord15,
        Generic.Heading: f"bold {nord8}",
        Generic.Subheading: f"bold {nord8}",
        Generic.Deleted: nord11,
        Generic.Inserted: nord14,
        Generic.Error: nord11,
        Generic.Emph: "italic",
        Generic.Strong: "bold",
        Generic.Prompt: f"bold {nord3}",
        Generic.Output: nord4,
        Generic.Traceback: nord11,
        Error: nord11,
    }


class NordStyle(Style):
    """
    Pygments version of the "nord" theme by Arctic Ice Studio
    """

    background_color = '#2e3440'
    default = '#d8dee9'

    styles = {
        Whitespace:                 '#d8dee9',

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

        Error:                      '#bf616a'
    }
