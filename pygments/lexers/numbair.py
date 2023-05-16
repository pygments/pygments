from pygments.styles.default import DefaultStyle
from pygments.styles.manni import ManniStyle
from pygments.styles.monokai import MonokaiStyle
from pygments.styles.native import NativeStyle

from pygments.lexer import RegexLexer, include, bygroups, words
from pygments.token import Text, Name, String,  Punctuation, Keyword, \
    Operator, Number

from pygments.style import Style

__all__ = ["NumbaIRLexer"]

class NumbaIRLexer(RegexLexer):
    """
    Lexer for Numba IR
    """
    name = 'Numba_IR'
    aliases = ['numba_ir', 'numbair']
    filenames = ['*.numba_ir']

    identifier = r'\$[a-zA-Z0-9._]+'
    fun_or_var = r'([a-zA-Z_]+[a-zA-Z0-9]*)'

    tokens = {
        'root' : [
            (r'(label)(\ [0-9]+)(:)$',
                bygroups(Keyword, Name.Label, Punctuation)),

            (r' = ', Operator),
            include('whitespace'),
            include('keyword'),

            (identifier, Name.Variable),
            (fun_or_var + r'(\()',
                bygroups(Name.Function, Punctuation)),
            (fun_or_var + r'(\=)',
                bygroups(Name.Attribute, Punctuation)),
            (fun_or_var, Name.Constant),
            (r'[0-9]+', Number),

            # <built-in function some>
            (r'<[^>\n]*>', String),

            (r'[=<>{}\[\]()*.,!\':]|x\b', Punctuation)
        ],

        'keyword':[
            (words((
                'del', 'jump', 'call', 'branch',
            ), suffix=' '), Keyword),
        ],

        'whitespace': [
            (r'(\n|\s)', Text),
        ],
    }
