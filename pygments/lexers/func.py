from pygments.lexer import RegexLexer, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Whitespace

__all__ = ['FuncLexer']


class FuncLexer(RegexLexer):
    name = 'FunC'
    aliases = ['func', 'fc']
    filenames = ['*.fc', '*.func']

    tokens = {
        'root': [
            (r'\n', Whitespace),
            (r'\s+', Whitespace),

            include('keywords'),
            include('strings'),
            include('directives'),
            include('numeric'),
            include('comments'),
            include('storage'),
            include('functions'),
            include('variables'),
        ],
        'keywords': [
            (r'(?<=\s)(<=>|>=|<=|!=|==|\^>>|\~>>|>>|<<|\/%|\^%|\~%|\^\/|\~\/|\+=|-=|\*=|\/=|~\/=|\^\/=|%=|\^%=|<<=|>>=|~>>=|\^>>=|&=|\|=|\^=|\^|=|~|\/|%|-|\*|\+|>|<|&|\||:|\?)(?=\s)', Operator),
            (r'\b(if|ifnot|else|elseif|elseifnot|while|do|until|repeat|return|impure|method_id|forall|asm|inline|inline_ref)\b', Keyword),
            (r'\b(false|true)\b', Keyword.Constant),
        ],
        'directives': [
            (r'#include|#pragma', Keyword, 'directive'),
        ],
        'directive': [
            include('strings'),
            (r'version|not-version', Keyword),
            (r'(>=|<=|=|>|<|\^)?([0-9]+)(.[0-9]+)?(.[0-9]+)?', Number), # version
            (r';', Text, '#pop')
        ],
        'strings': [
            (r'\"([^\n\"]+)\"(H|h|c|u|s|a)?', String),
        ],
        'numeric': [
            (r'(-?([\d]+|0x[\da-fA-F]+))\b', Number)
        ],
        'comments': [
            (r';;([^\n]*)', Comment.Singleline),
            (r'{-', Comment.Multiline, 'comment'),
        ],
        'comment': [
            (r'[^-}]+', Comment.Multiline),
            (r'{-', Comment.Multiline, '#push'),
            (r'-}', Comment.Multiline, '#pop'),
            (r'[-}]', Comment.Multiline),
        ],
        'storage': [
            (r'\b(var|int|slice|tuple|cell|builder|cont|_)(?=[\s\),\[\]])', Keyword.Type),
            (r'\b(global|const)\b', Keyword.Constant),
        ],
        'variables': [
            (r'(?!")(`([^`]+)`|(?=_[\w\d]*)(_([^;,\[\]\(\)\s~.\{\}]+))|(?![_`][\w\d]*)([^;,\[\]\(\)\s~.\{\}]+))', Name.Variable),
        ],
        'functions': [
            (r'(?!")(`([^`]+)`|(?=_[\w\d]*)(_([^;,\[\]\(\)\s~.\{\}]+))|(?![_`][\w\d]*)([^;,\[\]\(\)\s~.\{\}]+))(?=[\(])', Name.Function),
        ]
    }
