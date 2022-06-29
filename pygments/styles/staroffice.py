"""
    pygments.styles.staroffice
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    Style similar to StarOffice style, also in OpenOffice and LibreOffice.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic

class StarofficeStyle(Style):
    """
    Style similar to StarOffice style, also in OpenOffice and LibreOffice.
    """
    default_style = ""
    styles = {
        Comment:                '#666666',   # Gray
        Error:                  '#800000',   # Red
        Keyword:                '#000080',   # Blue
        Name:                   '#008000',   # Green
        Number:                 '#FF0000',   # Lightred
        Operator:               '#000080',   # Blue
        String:                 '#FF0000',   # Lightred
    }
