from pygments.lexer import RegexLexer
from pygments.token import Comment, String, Keyword, Name, Number, Punctuation, Text

class NSISLexer(RegexLexer):
    name = 'NSIS'
    aliases = ['nsis']
    filenames = ['*.nsi', '*.nsh']

    tokens = {
        'root': [
            (r';.*$', Comment.Single),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r'"(\\\\|\\"|[^"])*\\\n(\\\\|\\"|[^"])*"', String.Double),
            (r'\$\{[^\}]+\}', Name.Variable),
            (r'\$[a-zA-Z0-9_]+', Name.Variable),
            (r'\bFunction\b', Keyword, 'function'),
            (r'\bEndFunction\b', Keyword),
            (r'[{}()<>!=\[\]:;,]', Punctuation),
            (r'\.', Punctuation),
            (r'\b[+-]?[0-9]+\b', Number.Integer),
            (r'\b\w+\b', Name),
            (r'\s+', Text),
        ],
        'function': [
            (r'\bEndFunction\b', Keyword, '#pop'),
            (r'".*?[^\\]"', String.Double),
            (r'\'.*?[^\\]\'', String.Single),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            (r'".*?\\\n.*?"', String.Double),
            (r'\$\{[^\}]+\}', Name.Variable),
            (r'\$[a-zA-Z0-9_]+', Name.Variable),
            (r'\b[+-]?[0-9]+\b', Number.Integer),
            (r'[{}()<>!=\[\]:;,]', Punctuation),
            (r'\.', Punctuation),
            (r'\b\w+\b', Name),
            (r'\s+', Text),
        ],
    }
