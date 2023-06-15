# https://github.com/infosec-intern/vscode-yara/blob/main/yara/syntaxes/yara.tmLanguage.json
from pygments.lexer import RegexLexer, words
#from pygments.util import get_bool_opt, shebang_matches
from pygments.token import Comment, String, Name, Text, Punctuation, Operator, Keyword, Whitespace

__all__ = ['YaraLexer']

class YaraLexer(RegexLexer):
    """
    For YARA rules

   .. versionadded:: 2.16.0
    """

    name = 'YARA'
    url = 'https://virustotal.github.io/yara/'
    aliases = ['yara', 'yar']
    filenames = ['*.yar']
    mimetypes = []

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'//.*?$', Comment.Singleline),
            (r'/\*', Comment.Multiline, 'comment'),
            (words(('rule', 'private', 'global', 'import', 'include'), prefix=r'\b', suffix=r'\b'), Keyword.Declaration),
            (words(('strings', 'condition', 'meta', 'import'), prefix=r'\b', suffix=r'\b'), Keyword),
            (r'(true|false)\b', Keyword.Constant),
            (r'(and|or|not)\b', Operator.Word),
            (r'(any|all)\b', Operator.Word),
            (r'(\$[\w\d]+)', Name.Variable),
            (r'"[^"]*"', String.Double),
            (r'\'[^\']*\'', String.Single),
            #(r'0x(?:[0-9A-Fa-f?]{2}\s*)+(?:\[\d+-\d+\])?', Number.Hex),
            #(r'0x\[[\da-fA-F?-]+\]', Number.Hex),
            #(r'\d+(?:-[a-z\d]+)?', Number.Integer),
            (r'[a-z_]\w*', Name),
            (r'[$(){}[\].?+*|]', Punctuation),
            (r'[:=,;]', Punctuation),
            (r'.', Text)
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline)
        ]
    }
