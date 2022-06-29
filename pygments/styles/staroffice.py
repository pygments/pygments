"""
    pygments.styles.staroffice
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    pygments version of my "StarOffice" theme.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic

class StarofficeStyle(Style):
    default_style = ""
    styles = {
        Comment:                '#808080',   # Gray
        Error:                  '#800000',   # Red
        Keyword:                '#000080',   # Blue
        Name:                   '#008000',   # Green
        Number:                 '#FF0000',   # Lightred
        Operator:               '#000080',   # Blue
        String:                 '#FF0000',   # Lightred
    }
