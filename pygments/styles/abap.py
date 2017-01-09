from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic

class AbapStyle(Style):
    default_style = ""
    styles = {
        Comment:                'italic #888',
        Comment.Special:        '#888',
        Keyword:                '#00f',
        Operator.Word:          '#00f',
        Name:                   '#000',
        Number:                 '#3af',
        String:                 '#5a2',
        
        Error:                  '#F00'
    }