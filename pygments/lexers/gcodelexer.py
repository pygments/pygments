from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

__all__ = ['gcodeLexer']

class gcodeLexer(RegexLexer):
    name = 'g-code'
    aliases = ['gcode']
    filenames = ['*.gcode']
    
    tokens = {
        'root': [
            (r';.*\n', Comment),
            (r'^[gmGM]\d{1,4}\s',Name.Builtin), # M or G commands
            (r'([^gGmM])([+-]?\d*[.]?\d+)', bygroups(Keyword,Number)),
            (r'\s', Text.Whitespace),
            (r'.*\n', Text),
        ]
    }
    
