import re
from pygments.lexer import RegexLexer, words, default, include, bygroups
from pygments.token import Text, Comment, Whitespace, Operator, Keyword, Name, String, Number, Punctuation

__all__ = ['BerryLexer']

line_re = re.compile('.*?\n')

class BerryLexer(RegexLexer):

    # For Berry <https://github.com/berry-lang/berry> source code (version v0.1.0)
    # Contributed by Shaun Brown (Beormund) <https://github.com/Beormund>
    
    name = 'Berry'
    aliases = ['berry', 'be']
    filenames = ['*.be']
    mimetypes = ['text/x-berry', 'application/x-berry']

    _name = r'\b[^\W\d]\w*'

    # (def)\b((?:\s)*)
    # (def)\b(\s*$)(?<!\()
    # (def)\b((?:\s)*)(?=[^\W\d]\w*)

    tokens = {
        'root': [
            include('whitespace'),
            include('numbers'),
            include('keywords'),
            (r'(def)\b((?:\s)*)' + '(?=' + _name + ')', bygroups(Keyword.Declaration, Text), 'funcname'),
            (r'(class)\b((?:\s)*)', bygroups(Keyword.Declaration, Text), 'classname'),
            (r'(import)\b((?:\s)*)', bygroups(Keyword.Namespace, Text), 'import'),
            include('expr')
        ],
        'expr': [
            (r'[^\S\n]+', Text),
            (r'\.\.|[~!%^&*+=|?:<>/-]', Operator),
            (r'[(){}\[\],.;]', Punctuation),
            include('controls'),
            include('builtins'),
            include('funccall'),
            include('member'),
            include('name'),
            include('strings')
        ],
        'whitespace': [
            (r'\s+', Whitespace),
            (r'#-(.|\n)*?-#', Comment.Multiline),
            (r'#(\n|[\w\W]*?\n)', Comment.Single)
        ],
        'keywords': [
            (words((
                'as', 'break', 'continue', 'import', 'static', 'self', 'super'), 
                suffix=r'\b'), Keyword.Reserved),
            (r'(true|false|nil)\b', Keyword.Constant),
            (r'(var|def)\b', Keyword.Declaration)
        ],
        'controls': [
            (words((
                'if', 'elif', 'else', 'for', 'while', 'do', 'end', 'break', 
                'continue', 'return', 'try', 'except', 'raise'),
                suffix=r'\b'), Operator.Word)
        ],
        'builtins': [
            (words((
                'assert', 'bool', 'input', 'classname', 'classof', 'number', 'real',
                'bytes', 'compile', 'map', 'list', 'int', 'isinstance', 'print',
                'range', 'str', 'super', 'module', 'size', 'issubclass', 'open',
                'file', 'type', 'call'), 
                suffix=r'\b'), Name.Builtin)
        ],
        'numbers': [
            (r'0[xX](?:_?[a-fA-F0-9])+', Number.Hex),
            (r'[-]?\d+', Number.Integer),
            (r'([-]?\d*\.?\d+)(?:[eE]([-+]?\d+))?', Number.Float)
        ],
        'name': [
            (_name, Name)
        ],
        'funcname': [
            (_name, Name.Function, '#pop')
        ],
        'funccall': [
            ( _name + r'(?=\s*\()', Name.Function, '#pop')
        ],
        'member': [
            ( r'(?<=\.)' + _name + r'\b(?!\()', Name.Attribute, '#pop')
        ],
        'classname': [
            (_name, Name.Class, '#pop'),
        ],
        'import': [
            (r'(\s+)(as)(\s+)', bygroups(Text, Keyword, Text)),
        ],
        'strings': [
            (r'"(?:\\?.)*?"', String.Double, '#pop'),
            (r'\'(?:\\?.)*?\'', String.Single, '#pop')
        ]
    }
