"""
    pygments.styles.tomorrow
    ~~~~~~~~~~~~~~~~~~~~~~~~

    Pygments versions of the "Tomorrow" theme family by Chris Kempson.
    See https://github.com/chriskempson/tomorrow-theme

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Comment, Error, Generic, Keyword, Literal, Name, \
    Number, Operator, Other, Punctuation, String, Text, Whitespace

__all__ = ['TomorrowStyle', 'TomorrowNightStyle', 'TomorrowNightEightiesStyle',
           'TomorrowNightBlueStyle', 'TomorrowNightBrightStyle']


def _tomorrow_styles(fg, comment, red, orange, yellow, green, aqua, blue,
                      purple, invalid_fg, invalid_bg):
    """Build the Token -> style mapping shared by every Tomorrow variant.

    The color-to-role assignment follows the family's canonical TextMate
    grammar: comments are grey/italic, keywords and storage are purple,
    functions are blue, classes/types are yellow, strings are green,
    numbers/constants are orange, variables/tags/regexes are red, and
    operators are aqua.
    """
    return {
        Whitespace: fg,
        Text: fg,
        Text.Whitespace: fg,
        Other: fg,
        Error: f'{invalid_fg} bg:{invalid_bg}',

        Comment: f'italic {comment}',
        Comment.Multiline: f'italic {comment}',
        Comment.Single: f'italic {comment}',
        Comment.Special: f'italic bold {comment}',
        Comment.Preproc: f'noitalic {purple}',

        Keyword: purple,
        Keyword.Constant: orange,
        Keyword.Declaration: purple,
        Keyword.Namespace: purple,
        Keyword.Pseudo: purple,
        Keyword.Reserved: purple,
        Keyword.Type: yellow,

        Operator: aqua,
        Operator.Word: aqua,

        Punctuation: fg,

        Name: fg,
        Name.Attribute: red,
        Name.Builtin: blue,
        Name.Builtin.Pseudo: orange,
        Name.Class: yellow,
        Name.Constant: orange,
        Name.Decorator: yellow,
        Name.Entity: red,
        Name.Exception: yellow,
        Name.Function: blue,
        Name.Label: blue,
        Name.Namespace: green,
        Name.Other: fg,
        Name.Tag: red,
        Name.Variable: red,
        Name.Variable.Class: red,
        Name.Variable.Global: red,
        Name.Variable.Instance: red,

        Number: orange,
        Number.Float: orange,
        Number.Hex: orange,
        Number.Integer: orange,
        Number.Integer.Long: orange,
        Number.Oct: orange,

        Literal: fg,
        Literal.Date: orange,

        String: green,
        String.Backtick: green,
        String.Char: green,
        String.Doc: f'italic {green}',
        String.Double: green,
        String.Escape: orange,
        String.Heredoc: green,
        String.Interpol: orange,
        String.Other: green,
        String.Regex: red,
        String.Single: green,
        String.Symbol: green,

        Generic: fg,
        Generic.Deleted: red,
        Generic.Emph: 'italic',
        Generic.EmphStrong: 'bold italic',
        Generic.Error: red,
        Generic.Heading: f'bold {green}',
        Generic.Inserted: f'bold {green}',
        Generic.Output: comment,
        Generic.Prompt: comment,
        Generic.Strong: 'bold',
        Generic.Subheading: f'bold {green}',
        Generic.Traceback: red,
    }


class TomorrowStyle(Style):
    """
    Pygments version of the "Tomorrow" light theme.
    """

    name = 'tomorrow'

    background_color = '#FFFFFF'
    highlight_color = '#D6D6D6'
    line_number_color = '#8E908C'
    line_number_background_color = '#EFEFEF'

    styles = _tomorrow_styles(
        fg='#4D4D4C', comment='#8E908C', red='#C82829', orange='#F5871F',
        yellow='#C99E00', green='#718C00', aqua='#3E999F', blue='#4271AE',
        purple='#8959A8', invalid_fg='#FFFFFF', invalid_bg='#C82829',
    )


class TomorrowNightStyle(Style):
    """
    Pygments version of the "Tomorrow Night" theme.
    """

    name = 'tomorrow-night'

    background_color = '#1D1F21'
    highlight_color = '#373B41'
    line_number_color = '#969896'
    line_number_background_color = '#282A2E'

    styles = _tomorrow_styles(
        fg='#C5C8C6', comment='#969896', red='#CC6666', orange='#DE935F',
        yellow='#F0C674', green='#B5BD68', aqua='#8ABEB7', blue='#81A2BE',
        purple='#B294BB', invalid_fg='#CED2CF', invalid_bg='#DF5F5F',
    )


class TomorrowNightEightiesStyle(Style):
    """
    Pygments version of the "Tomorrow Night Eighties" theme.
    """

    name = 'tomorrow-night-eighties'

    background_color = '#2D2D2D'
    highlight_color = '#515151'
    line_number_color = '#999999'
    line_number_background_color = '#393939'

    styles = _tomorrow_styles(
        fg='#CCCCCC', comment='#999999', red='#F2777A', orange='#F99157',
        yellow='#FFCC66', green='#99CC99', aqua='#66CCCC', blue='#6699CC',
        purple='#CC99CC', invalid_fg='#CDCDCD', invalid_bg='#F2777A',
    )


class TomorrowNightBlueStyle(Style):
    """
    Pygments version of the "Tomorrow Night Blue" theme.
    """

    name = 'tomorrow-night-blue'

    background_color = '#002451'
    highlight_color = '#003F8E'
    line_number_color = '#7285B7'
    line_number_background_color = '#00346E'

    styles = _tomorrow_styles(
        fg='#FFFFFF', comment='#7285B7', red='#FF9DA4', orange='#FFC58F',
        yellow='#FFEEAD', green='#D1F1A9', aqua='#99FFFF', blue='#BBDAFF',
        purple='#EBBBFF', invalid_fg='#FFFFFF', invalid_bg='#F99DA5',
    )


class TomorrowNightBrightStyle(Style):
    """
    Pygments version of the "Tomorrow Night Bright" theme.
    """

    name = 'tomorrow-night-bright'

    background_color = '#000000'
    highlight_color = '#424242'
    line_number_color = '#969896'
    line_number_background_color = '#2A2A2A'

    styles = _tomorrow_styles(
        fg='#DEDEDE', comment='#969896', red='#D54E53', orange='#E78C45',
        yellow='#E7C547', green='#B9CA4A', aqua='#70C0B1', blue='#7AA6DA',
        purple='#C397D8', invalid_fg='#CED2CF', invalid_bg='#DF5F5F',
    )
