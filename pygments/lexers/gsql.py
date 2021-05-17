import re

from pygments.lexer import RegexLexer, include, bygroups, using, this
from pygments.token import Keyword, Punctuation, Comment, Operator, Name,\
    String, Number, Whitespace


__all__ = ["GSQLLexer"]

class GSQLLexer(RegexLexer):
    name = 'GSQL'
    aliases = ['gsql']
    filenames = ['*.gsql']

    flags = re.MULTILINE | re.IGNORECASE

    tokens = {
        'root': [
            include('comment'),
            include('keywords'),
            include('clauses'),
            include('accums'),
            include('relations'),
            include('strings'),
            include('whitespace'),
            include('barewords'),
            include('operators'),
        ],
        'comment': [
            (r'.*\#.*\n', Comment.Single),
            (r'.*\/\*\s*.*\s*\*\/', Comment.Multiline),
        ],
        'keywords': [
            (r'(ACCUM|AND|ANY|API|AS|ASC|AVG|BAG|BATCH|BETWEEN|BOOL|BOTH|'
             r'BREAK|BY|CASE|CATCH|COALESCE|COMPRESS|CONTINUE|COUNT|'
             r'CREATE|DATETIME|DATETIME_ADD|DATETIME_SUB|DELETE|DESC|DISTRIBUTED|DO|'
             r'DOUBLE|EDGE|ELSE|END|ESCAPE|EXCEPTION|FALSE|FILE|FILTER|FLOAT|FOREACH|FOR|'
             r'FROM|GRAPH|GROUP|GSQL_INT_MAX|GSQL_INT_MIN|GSQL_UINT_MAX|HAVING|IF|'
             r'IN|INSERT|INT|INTERPRET|INTERSECT|INTERVAL|INTO|IS|ISEMPTY|JSONARRAY|JSONOBJECT|LASTHOP|'
             r'LEADING|LIKE|LIMIT|LIST|LOAD_ACCUM|LOG|MAP|MATCH|MAX|MIN|MINUS|NOT|'
             r'NOW|NULL|OFFSET|OR|ORDER|PATH|PER|PINNED|POST_ACCUM|POST-ACCUM|PRIMARY_ID|PRINT|'
             r'QUERY|RAISE|RANGE|REPLACE|RESET_COLLECTION_ACCUM|RETURN|RETURNS|RUN|SAMPLE|SELECT|SELECT_VERTEX|'
             r'SET|SRC|STATIC|STRING|SUM|SYNTAX|TARGET|TAGSTGT|THEN|TO|TO_CSV|TO_DATETIME|TRAILING|TRIM|TRUE|'
             r'TRY|TUPLE|TYPEDEF|UINT|UNION|UPDATE|VALUES|VERTEX|WHEN|WHERE|WHILE|WITH)\b', Keyword),
        ],
        'clauses': [
            (r'(accum|having|limit|order|postAccum|sample|where)\b', Keyword),
        ],
        'accums': [
            (r'(andaccum|arrayaccum|avgaccum|bagaccum|bitwiseandaccum|'
             r'bitwiseoraccum|groupbyaccum|heapaccum|listaccum|MapAccum|'
             r'maxaccum|minaccum|oraccum|setaccum|sumaccum)\b', Keyword),
        ],
        # 'name': [
        #     (r'(\@\@w+)\b', Name),
        # ],
        'relations': [
            (r'(-\s?)(\(.*\:\w?\))(\s?-)', bygroups(Operator, using(this), Operator)),
            (r'->|<-', Operator),
            (r'[.*{}]', Punctuation),
        ],
        'strings': [
            (r'"(?:\\[tbnrf\'"\\]|[^\\"])*"', String),
            (r'@{1,2}\w+', Name.Variable),
            (r'(\<\w+)?\<(\w+\>?\,?\s?)+\>+', Name.Constant),
        ],
        'whitespace': [
            (r'\s+', Whitespace),
        ],
        'barewords': [
            (r'[a-z]\w*', Name),
            (r'(\d+\.\d+|\d+)', Number),
        ],
        'operators': [
            (r'[^0-9|\/|\-](\-\=|\+\=|\*\=|\\\=|\=|\=\=|\=\=\=|\+|\-|\*|\\|\+\=|\>|\<)[^\>|\/]', Operator),
            (r'(\(|\)|\,|\;|\=|\-|\+|\*|\/|\>|\<|\:)', Operator),
        ],
    }
