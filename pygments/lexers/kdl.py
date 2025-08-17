"""
    pygments.lexers.kdl
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for Kdl file format.

    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer
from pygments.token import Comment, Keyword, Name, Number, Operator, String, Whitespace

__all__ = ['KdlLexer']

class KdlLexer(RegexLexer):
    """
    For KDL config files.
    """
    name = 'KDL'
    aliases = ['kdl']
    version_added = '2.20.0'
    filenames = ['*.kdl']
    url = 'https://kdl.dev/'
    tokens = {
        'root': [
            (r'((?<={|;)|^)\s*(?![/\\\{\}#;\[\]\=])[<>:\w\-_~,\'`!\?@\$%^&*+|\.\(\)]+\d*?[<>:\w\-_~,\'`!\?@\$%^&*+|\.\(\)]*?', Name.Label),
            (r'(#true|#false|#null|#nan|#inf|#-inf)\b', Keyword.Constant),
            (r'[{}=;\\]', Operator),
            (r'(\b([0-9-\+]|-|\+)[0-9_]*?\.[0-9][0-9_]*?([eE][+-]?[0-9_]+)?\b|\b[0-9][0-9_]*?(\.[0-9][0-9_]*?)?[eE][+-]?[0-9_]+\b)', Number.Float),
            (r'\b[0-9\-\+][0-9_]*\b', Number.Decimal),
            (r'\b0x[a-fA-F0-9][a-fA-F0-9_]*?\b', Number.Hex),
            (r'\b0o[0-7][0-7_]*\b', Number.Oct),
            (r'\b0b[01][01_]*?\b', Number.Bin),
            (r'#+(\"""|").*?("""|")#+', String),
            (r'#?"""', String, 'multiline_string'),
            (r'#?"', String, 'string'),
            (r'/\*', Comment.Multiline, 'comment'),
            (r'/\*!', String.Doc, 'doccomment'),
            (r'/-\s*{', Comment.Multiline, 'slashdash_block_comment'),
            (r'\s*/-\s?[^\s=]*?\s?{', Comment.Multiline, 'slashdash_node_comment'),
            (r'(?<!^)\s*/-\s*(".*"|.*?)?\s', Comment.Single),
            (r'(?<=^)\s*/-[^{]+{', Comment.Multiline, 'slashdash_node_with_children_comment'),
            (r'(\/\/(.*?)\n|(?<!^)\s*/-\s*?\s)', Comment.Single),
            (r'(?![/\\\{\}#;\[\]\=])[<>:\w\-_~,\'`!\?@\$%^&*+|.\(\)]+\d*?[<>:\w\-_~,\'`!\?@\$%^&*+|.\(\)]*(=)', Name.Attribute),
            (r'(?![/\\\{\}#;\[\]\=])[<>:\w\-_~,\'`!\?@\$%^&*+|.\(\)]+\d*?[<>:\w\-_~,\'`!\?@\$%^&*+|.\(\)]*?', String),
            (r'\s', Whitespace),
        ],
        'string': [
            (r'"#?', String, '#pop'),
            (r"""\\['"\\nrt]|\\x[0-7][0-9a-fA-F]|\\0"""
             r"""|\\u\{[0-9a-fA-F]{1,6}\}""", String.Escape),
            (r'[^\\"]+', String),
            (r'\\', String),
        ],
        'multiline_string': [
            (r'"""#?', String, '#pop'),
            (r"""\\['"\\nrt]|\\x[0-7][0-9a-fA-F]|\\0"""
             r"""|\\u\{[0-9a-fA-F]{1,6}\}""", String.Escape),
            (r'"', String),
            (r'[^\\"]+', String),
            (r'\\', String),
        ],
        'slashdash_block_comment': [
            (r'[^}]+', Comment.Multiline),
            (r'/-\s*{', Comment.Multiline, '#push'),
            (r'\}', Comment.Multiline, '#pop'),
            (r'[\}]', Comment.Multiline),
        ],

        'slashdash_node_comment': [
            (r'[^\}]+', Comment.Multiline),
            (r'^\s*?/-.*?\s?{', Comment.Multiline, '#push'),
            (r'\}', Comment.Multiline, '#pop'),
            (r'[\}]', Comment.Multiline),
        ],
        'slashdash_node_with_children_comment': [
            (r'[^\}]+', Comment.Multiline),
            (r'(?<=^)\s*/-[^{]+{', Comment.Multiline, '#push'),
            (r'\}', Comment.Multiline, '#pop'),
            (r'[\}]', Comment.Multiline),
        ],
        'comment': [
            (r'[^*/]+', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
        'doccomment': [
            (r'[^*/]+', String.Doc),
            (r'/\*', String.Doc, '#push'),
            (r'\*/', String.Doc, '#pop'),
            (r'[*/]', String.Doc),
        ],
    }