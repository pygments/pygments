"""
    pygments.lexers.kdl
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for Kdl file format.

    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer
from pygments.token import Comment, Keyword, Name, Number, Operator, String, Whitespace

__all__ = ['KdlLexer']

class CustomLexer(RegexLexer):
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
            # # null, booleans, float keywoards
            (r'(#true|#false|#null|#nan|#inf|#-inf)\b', Keyword.Constant),
            # # some operators (should probably add more, idk what counts as what)
            (r'[{}=;\\]', Operator),
            # # floats like 1.23 and 4.56e10
            (r'(\b([0-9-\+]|-|\+)[0-9_]*?\.[0-9][0-9_]*?([eE][+-]?[0-9_]+)?\b|\b[0-9][0-9_]*?(\.[0-9][0-9_]*?)?[eE][+-]?[0-9_]+\b)', Number.Float),
            # decimal
            (r'\b[0-9\-\+][0-9_]*\b', Number.Decimal),
            # # hexadecimal
            (r'\b0x[a-fA-F0-9][a-fA-F0-9_]*?\b', Number.Hex),
            # # octal
            (r'\b0o[0-7][0-7_]*\b', Number.Oct),
            # # binary
            (r'\b0b[01][01_]*?\b', Number.Bin),
            # # raw string #"yeah"#
            (r'#+(\"""|").*?("""|")#+', String),
            # # regular strings and multiline strings (unsure if this classifies properly)
            (r'#?"""', String, 'multiline_string'),
            (r'#?"', String, 'string'),
            # # comment like /* hello */
            (r'/\*', Comment.Multiline, 'comment'),
            # # document comment like /*! hello */
            (r'/\*!', String.Doc, 'doccomment'),
            # # /- { commented; block; }
            (r'/-\s*{', Comment.Multiline, 'slashdash_block_comment'),
            # # > /-really {
            # # >    blah
            # # > }
            (r'\s*/-\s?[^\s=]*?\s?{', Comment.Multiline, 'slashdash_node_comment'),
            
            # # mynode /-"commented" {
            # #        ^
            # # or
            # # mynode /-abc=123 {
            # #        ^
            (r'(?<!^)\s*/-\s*(".*"|.*?)?\s', Comment.Single),
            # # this one's causing a lot of trouble at the moment, but it's supposed to catch
            # # /-some node with=children { stuff; that; should; be; commented; out; }
            # # // blah blah, and...
            (r'(?<=^)\s*/-[^{]+{', Comment.Multiline, 'slashdash_node_with_children_comment'),
            (r'(\/\/(.*?)\n|(?<!^)\s*/-\s*?\s)', Comment.Single),
            (r'(?![/\\{\}#;\[\]\=])[<>:\w\-_~,\'`!\?@\$%^&*+|.\(\)\u0080-\uFFFF]+\d*[<>:\w\-_~,\'`!\?@\$%^&*+|.\(\)\u0080-\uFFFF]*(=)', Name.Attribute),
            (r'((?<={|;)|^)\s*(?![/\\{\}#;\[\]\=\d])[<>:\w\-_~,\'`!\?@\$%^&*+|\.\(\)\u0080-\uFFFF]+\d*[<>:\w\-_~,\'`!\?@\$%^&*+|\.\(\)\u0080-\uFFFF]*?', Name.Label),
            (r'(?![/\\{\}#;\[\]\=])[<>:\w\-_~,\'`!\?@\$%^&*+|.\(\)\u0080-\uFFFF]+\d*[<>:\w\-_~,\'`!\?@\$%^&*+|.\(\)\u0080-\uFFFF]*?', String),
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