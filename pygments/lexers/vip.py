from pygments.lexer import RegexLexer, inherit, words, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Whitespace

__all__ = ['VisualPrologLexer', 'VisualPrologGrammarLexer']


class VisualPrologBaseLexer(RegexLexer):
    minorendkw = ('try', 'foreach', 'if')
    minorkwexp = ('and', 'catch', 'do', 'else', 'elseif', 'erroneous', 'externally', 'failure', 'finally', 'foreach', 'if', 'or', 'orelse', 'otherwise', 'then',
        'try', 'div', 'mod', 'rem', 'quot')
    dockw = ('short', 'detail', 'end', 'withdomain')
    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (words(minorendkw, prefix=r'\bend\s+', suffix=r'\b'), Keyword.Minor),
            (r'end', Keyword),
            (words(minorkwexp, suffix=r'\b'), Keyword.Minor),
            (r'0[xo][\da-fA-F_]+', Number),
            (r'((\d[\d_]*)?\.)?\d[\d_]*([eE][\-+]?\d+)?', Number),
            (r'_\w*', Name.Variable.Anonymous),
            (r'[A-Z]\w*', Name.Variable),
            (r'@\w+', Name.Variable),
            (r'[a-z]\w*', Name),
            (r'/\*', Comment, 'comment'),
            (r'\%', Comment, 'commentline'),
            (r'"', String.Symbol, 'string'),
            (r'\'', String.Symbol, 'stringsingle'),
            (r'@"', String.Symbol, 'atstring'),
            (r'[\-+*^/!?<>=~:]+', Operator),
            (r'[$,.[\]|(){}\\]+', Punctuation),
            (r'.', Text),
        ],
        'commentdoc' : [
            (words(dockw, prefix=r'@', suffix=r'\b'), Comment.Preproc),
            (r'@', Comment),
        ],
        'commentline' : [
            include('commentdoc'),
            (r'[^@\n]+', Comment),
            (r'$', Comment, '#pop'),
        ],
        'comment': [
            include('commentdoc'),
            (r'[^@*/]+', Comment),
            (r'/\*', Comment, '#push'),
            (r'\*/', Comment, '#pop'),
            (r'[*/]', Comment),
        ],
        'stringescape' : [
            (r'\\u[0-9a-fA-F]{4}', String.Escape),
            (r'\\u[0-9a-fA-F]{0,3}', String.Escape.Error),
            (r'\\[\'"ntr\\]', String.Escape),
            (r'\\[^\'"ntr\\]', String.Escape.Error),
        ],
        'stringsingle' : [
            include('stringescape'),
            (r'\'', String.Symbol, '#pop'),
            (r'[^\'\\\n]+', String),
            (r'\n', String.Escape.Error, '#pop'),
        ],
        'string' : [
            include('stringescape'),
            (r'"', String.Symbol, '#pop'),
            (r'[^"\\\n]+', String),
            (r'\n', String.Escape.Error, '#pop'),
        ],
        'atstring' : [
            (r'""', String.Escape),
            (r'"', String.Symbol, '#pop'),
            (r'[^"]+', String),
        ]
    }

class VisualPrologLexer(VisualPrologBaseLexer):
    name = 'Visual Prolog'
    aliases = ['visualprolog']
    filenames = ['*.pro', '*.cl', '*.i', '*.pack', '*.ph']

    majorkw = ('goal', 'namespace', 'interface', 'class', 'implement', 'where', 'open', 'inherits', 'supports', 'resolve', 'delegate', 'monitor', 'constants', 'domains', 'predicates', 'constructors', 'properties', 'clauses', 'facts')
    minorkw = ('align', 'anyflow', 'as', 'bitsize', 'determ', 'digits', 'erroneous', 'externally', 'failure', 'from', 'guard', 'multi', 'nondeterm', 'or', 'orelse', 'otherwise', 'procedure', 'resolve', 'single', 'suspending')
    directivekw = ('bininclude', 'else', 'elseif', 'endif', 'error', 'export', 'externally', 'from', 'grammargenerate', 'grammarinclude', 'if', 'include', 'message', 'options', 'orrequires', 'requires', 'stringinclude', 'then')
    tokens = {
        'root': [
            (words(minorkw, suffix=r'\b'), Keyword.Minor),
            (words(majorkw, suffix=r'\b'), Keyword),
            (words(directivekw, prefix='#', suffix=r'\b'), Keyword.Directive),
            inherit
        ]
    }

class VisualPrologGrammarLexer(VisualPrologBaseLexer):
    name = 'Visual Prolog Grammar'
    aliases = ['visualprologgrammar']
    filenames = ['*.vipgrm']

    majorkw = ('open', 'namespace', 'grammar', 'nonterminals', 'startsymbols', 'terminals', 'rules', 'precedence')
    directivekw = ('bininclude', 'stringinclude')
    tokens = {
        'root': [
            (words(majorkw, suffix=r'\b'), Keyword),
            (words(directivekw, prefix='#', suffix=r'\b'), Keyword.Directive),
            inherit
        ]
    }