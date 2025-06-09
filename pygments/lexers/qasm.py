from pygments.lexer import RegexLexer, bygroups
from pygments.token import Keyword, Name, Operator, Punctuation, Comment, Number, Whitespace

__all__ = ['QasmLexer']

class QasmLexer(RegexLexer):
    """
    For QASM code.
    """

    name = 'QASM'
    aliases = ['qasm']
    filenames = ['*.qasm', '*.oqs']
    url = 'https://openqasm.com/'
    version_added = '2.20.0'

    tokens = {
        'root': [
            (r'//.*$', Comment.Single),
            (r'(OPENQASM)(\s+)(\d+\.\d+)', bygroups(Keyword, Whitespace, Number.Float)),
            (r'\binclude\b', Keyword.Reserved),
            (r'(qreg|creg|gate|measure|reset|barrier)', Keyword),  # Keywords
            (r'\b(cx|h|x|y|z|s|t|sdg|tdg|rx|ry|rz|p|ccx|u1|u2|u3)\b', Name.Builtin),  # Gate names
            (r'[{},;()\[\]."\']', Punctuation),  # Delimiters
            (r'[+\-*/^=]', Operator),  # Operators
            (r'[0-9]+', Number.Integer),  # Integer numbers
            (r'\s+', Whitespace),  # Spaces and newlines
            (r'[a-zA-Z_][a-zA-Z0-9_]*', Name),  # Variable names
        ]
    }
