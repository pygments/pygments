# -*- coding: utf-8 -*-
"""
    pygments.styles.native
    ~~~~~~~~~~~~~~~~~~~~~

    pygments version of my "native" vim theme.

    :copyright: 2006 by Armin Ronacher.
    :license: GNU LGPL, see LICENSE for more details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic, Text, Token


class NativeStyle(Style):

    background_color = '#202020'

    styles = {
        Token:              '#d0d0d0',

        Comment:            'italic #999999',
        Comment.Preproc:    'noitalic bold #cd2828',

        Keyword:            'bold #6ab825',
        Keyword.Pseudo:     'nobold',

        String:             '#ed9d13',
        String.Other:       '#ffa500',

        Number:             '#3677a9',

        Name.Builtin:       '#24909d',
        Name.Variable:      '#40ffff',
        Name.Constant:      '#40ffff',
        Name.Class:         'underline #447fcf',
        Name.Function:      '#447fcf',
        Name.Namespace:     'underline #447fcf',
        Name.Exception:     '#bbbbbb',
        Name.Tag:           'bold #6ab825',
        Name.Attribute:     '#bbbbbb',
        Name.Decorator:     '#ffa500',

        Generic.Heading:    'bold #ffffff',
        Generic.Subheading: 'underline #ffffff',
        Generic.Deleted:    '#d22323',
        Generic.Inserted:   '#589819',
        Generic.Error:      '#d22323',
        Generic.Emph:       'italic',
        Generic.Strong:     'bold',
        Generic.Prompt:     '#aaaaaa',
        Generic.Output:     '#cccccc',
        Generic.Traceback:  '#d22323',

        Error:              'bg:#e3d2d2 #a61717'
    }
