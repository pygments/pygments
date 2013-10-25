from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic

class IgorStyle(Style):
    default_style = ""
    styles = {
        Comment:                'italic #FF0000',
        Keyword:                '#0000FF',
        Name.Function:          '#C34E00',
        Name.Class:             '#007575',
        String:                 '#009C00'
    }
