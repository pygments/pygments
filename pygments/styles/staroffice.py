"""
    pygments.styles.staroffice
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    Style similar to StarOffice style, also in OpenOffice and LibreOffice.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.style import Style
from pygments.token import Comment, Error, Literal, Name

class StarofficeStyle(Style):
    """
    Style similar to StarOffice style, also in OpenOffice and LibreOffice.
    """
    default_style =             '#000080'    # Blue
    styles = {
        Comment:                '#696969',   # DimGray
        Error:                  '#800000',   # Maroon
        Literal:                '#EE0000',   # Red
        Name:                   '#008000',   # Green
    }
