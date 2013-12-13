# -*- coding: utf-8 -*-
"""
    pygments.styles.xcode
    ~~~~~~~~~~~~~~~~~~~~~~

    Style similar to the `Xcode`_ default style.

    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic, Whitespace, Text, Other, Literal, Punctuation


class XcodeStyle(Style):
    """
    Style similar to the Xcode default style.
    """

    default_style = ''

    styles = {
        Comment:                '#177500',
        Comment.Preproc:        '#633820',

        String:                 '#C41A16',
        String.Char:            '#2300CE',
        
        Operator:               '#000000',
        # Operator.Word:          '#ff0072',

        Keyword:                '#AA0D92',
        # Keyword.Type:           '#000000',

        Name:                   '#000000',
        Name.Attribute:         '#836C28',
        Name.Class:             '#000000',
        Name.Function:          '#000000',
        # Name.Property:          '#000000',
        # Name.Namespace:         '#000000',
        Name.Builtin:           '#AA0D92',
        Name.Builtin.Pseudo:    '#5B269A',          # In Obj-C code this token is used to colour Cocoa types, TODO new token type
        Name.Variable:          '#000000',          # it was method argument (but not always recognized successfully)
        # Name.Variable.Class:    '#000000',
        # Name.Variable.Instance: '#000000',
        # Name.Variable.Global:   '#000000',
        # Name.Constant:          '#000000',
        Name.Tag:               '#000000',
        Name.Decorator:         '#000000',          # category name in braces
        Name.Label:             '#000000',          # here is a little bug, it treats multiline method signatres as labels (second and later lines)

        Number:                 '#2300CE',
        Error:                  '#000000',           # @ char when using as acronym for NSNumber @(10) and also in nonrecognized tokens like: @autoreleasepool, @required - only @ char
    }
