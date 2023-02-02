from pygments.lexer import RegexLexer, words
from pygments.token import Comment, Punctuation, Whitespace,\
    Name, Operator, String, Number
from pygments.lexers._dax_builtins import FUNCTIONS, KEYWORDS

__all__ = ['DaxLexer']

class DaxLexer(RegexLexer):
    """
    Lexer for Power BI DAX
    Referenced from: https://github.com/sql-bi/SyntaxHighlighterBrushDax
    """
    name = 'Dax'
    aliases = ['dax']
    filenames = ['*.dax']

    functions_upper = [x.upper() for x in FUNCTIONS]
    keywords_upper = [x.upper() for x in KEYWORDS]

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r"--.*\n?", Comment.Single),	# Comment: #39A03B, Double dash comment
            (r"//.*\n?", Comment.Single),	# Comment: #39A03B Double backslash comment
            (r"/\*", Comment.Multiline, 'multiline-comments'),
            (words(FUNCTIONS, prefix=r'(?i), suffix=r'\b'), Name.Function), #Functions
            (words(KEYWORDS , prefix=r'(?i)', suffix=r'\b'), Name.Builtin), # Keyword
            (r':=|[-+*\/=^]', Operator),
            (r'\b(IN|NOT)\b', Operator.Word),
            (r'\"(?:[^\"]|\"\")*\"(?!\")', String.Single), #StringLiteral
            (r"'(?:[^']|'')*'(?!')(?:\[[ \w\xA0-\uFFFF]+\])?|\w+\[[ \w\xA0-\uFFFF]+\]"
                , Name.Attribute),	# Column reference
            (r"\[[ \w\xA0-\uFFFF]+\]", Name.Attribute), #Measure reference
            (r'\b\d+\.?\d*|\B\.\d+\b', Number),# Number
            (r'[\[\](){}`,.]', Punctuation), #Parenthesis
        ],
        'multiline-comments': [
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[^/*]+', Comment.Multiline),
            (r'[/*]', Comment.Multiline)
        ]
    }
