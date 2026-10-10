"""
    pygments.lexers.nlp
    ~~~~~~~~~~~~~~~~~~~

    Lexers for NLP++ and the VisualText file formats.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, bygroups, include, words
from pygments.token import Comment, Generic, Keyword, Name, Number, \
    Operator, Punctuation, String, Text, Whitespace

__all__ = ['NLPPlusLexer', 'NLPPlusDictLexer', 'NLPPlusKbbLexer',
           'NLPPlusSeqLexer', 'NLPPlusTreeLexer']

# Region markers introduce the parts of a pass: @RULES holds the pattern rules,
# @POST/@PRE/@CHECK the code attached to them, @NODES the tree region to run over.
REGIONS = (
    'CHECK', 'CODE', 'DECL', 'MULTI', 'NODES', 'PATH', 'POST', 'PRE', 'RULES'
)

# Node constants matching classes of text rather than a named node.
CONSTANTS = (
    'xALPHA', 'xANY', 'xBLANK', 'xCAP', 'xCAPLET', 'xCTRL', 'xEMOJI', 'xEND',
    'xEOF', 'xLET', 'xNIL', 'xNUM', 'xPUNCT', 'xSTART', 'xVAR', 'xWHITE',
    'xWILD'
)

# Modifiers inside a rule element's [...] block.
ATTRIBUTES = (
    'attr', 'attrs', 'base', 'da', 'deacc', 'deaccent', 'except', 'excepts',
    'fail', 'fails', 'gp', 'group', 'layer', 'layers', 'look', 'lookahead',
    'match', 'matches', 'max', 'min', 'nest', 'o', 'one', 'opt', 'option',
    'optional', 'pass', 'passes', 'plus', 'recurse', 'ren', 'rename', 's',
    'singlet', 'star', 't', 'tree', 'trig', 'trigger', 'unsealed'
)

# Engine builtins callable from code regions.
BUILTINS = (
    'LJ', 'abs', 'addarg', 'addattr', 'addcnode', 'addconcept', 'addconval',
    'addnode', 'addnumval', 'addstmt', 'addstrs', 'addstrval', 'addsval',
    'addword', 'arraylength', 'attrchange', 'attrexists', 'attrname',
    'attrtype', 'attrvals', 'attrwithval', 'batchstart', 'cap', 'cbuf',
    'ceiling', 'closefile', 'conceptname', 'conceptpath', 'conval', 'cout',
    'coutreset', 'dballocstmt', 'dbbindcol', 'dbclose', 'dbexec', 'dbexecstmt',
    'dbfetch', 'dbfreestmt', 'dbopen', 'deaccent', 'debug', 'dictfindword',
    'dictfirst', 'dictgetword', 'dictnext', 'down', 'eltnode', 'excise',
    'exitpass', 'exittopopup', 'factorial', 'fail', 'fileout', 'findana',
    'findattr', 'findattrs', 'findconcept', 'findhierconcept', 'findnode',
    'findphrase', 'findroot', 'findvals', 'findwordpath', 'firstnode', 'floor',
    'flt', 'fltval', 'fncallstart', 'fprintgvar', 'fprintnvar', 'fprintvar',
    'fprintxvar', 'gdump', 'getconcept', 'getconval', 'getnumval',
    'getpopupdata', 'getstrval', 'getsval', 'ginc', 'gp', 'group', 'gtolower',
    'guniq', 'hitconf', 'inc', 'inheritval', 'inputrange', 'inputrangetofile',
    'interactive', 'kbdumptree', 'lasteltnode', 'lastnode', 'length',
    'lengthr', 'levenshtein', 'lextagger', 'listadd', 'listnode', 'lj', 'log',
    'logten', 'lookup', 'lowercase', 'makeconcept', 'makeparentconcept',
    'makephrase', 'makestmt', 'merge', 'merger', 'mkdir', 'mod', 'movecleft',
    'movecright', 'movesem', 'ndump', 'next', 'nextattr', 'nextval', 'ninc',
    'nodeconcept', 'nodeowner', 'noop', 'num', 'numrange', 'numval',
    'openfile', 'or', 'pathconcept', 'percentstr', 'permuten', 'phraselength',
    'phraseraw', 'phrasetext', 'pncopyvars', 'pndeletechilds', 'pndown',
    'pninsert', 'pnmakevar', 'pnname', 'pnnext', 'pnprev', 'pnrename',
    'pnreplaceval', 'pnroot', 'pnsingletdown', 'pnup', 'pnvar', 'pnvarnames',
    'pow', 'pranchor', 'prchild', 'preaction', 'prev', 'print', 'printr',
    'printvar', 'prlit', 'prrange', 'prtree', 'prunephrases', 'prxtree',
    'randomint', 'regexp', 'regexpi', 'renameattr', 'renamechild',
    'renameconcept', 'renamenode', 'replaceval', 'resolveurl', 'returnstmt',
    'rfaaction', 'rfaactions', 'rfaarg', 'rfaargtolist', 'rfacode',
    'rfaelement', 'rfaelt', 'rfaexpr', 'rfalist', 'rfalitelt',
    'rfalittoaction', 'rfalittopair', 'rfaname', 'rfanodes', 'rfanonlit',
    'rfanonlitelt', 'rfanum', 'rfaop', 'rfapair', 'rfapairs', 'rfapostunary',
    'rfapres', 'rfarange', 'rfarecurse', 'rfarecurses', 'rfaregion',
    'rfaregions', 'rfarule', 'rfarulelts', 'rfarulemark', 'rfarules',
    'rfarulesfile', 'rfaselect', 'rfastr', 'rfasugg', 'rfaunary', 'rfavar',
    'rfbarg', 'rfbdecl', 'rfbdecls', 'rightjustifynum', 'rmattr', 'rmattrs',
    'rmattrval', 'rmchild', 'rmchildren', 'rmconcept', 'rmcphrase', 'rmnode',
    'rmphrase', 'rmval', 'rmvals', 'rmword', 'round', 'sdump', 'setbase',
    'setlookahead', 'setunsealed', 'single', 'singler', 'singlex', 'singlezap',
    'sortchilds', 'sortconsbyattr', 'sorthier', 'sortphrase', 'sortvals',
    'spellcandidates', 'spellcorrect', 'spellword', 'splice', 'split',
    'sqlstr', 'sqrt', 'startout', 'stem', 'stopout', 'str', 'strchar',
    'strchr', 'strchrcount', 'strclean', 'strcontains', 'strcontainsnocase',
    'strendswith', 'strequal', 'strequalnocase', 'strescape', 'strgreaterthan',
    'strisalpha', 'strisdigit', 'strislower', 'strisupper', 'strlength',
    'strlessthan', 'strnotequal', 'strnotequalnocase', 'strpiece', 'strrchr',
    'strspellcandidate', 'strspellcompare', 'strstartswith', 'strsubst',
    'strtolower', 'strtotitle', 'strtoupper', 'strtrim', 'strunescape',
    'strval', 'strwrap', 'succeed', 'suffix', 'system', 'take', 'today',
    'topdir', 'truncate', 'unknown', 'unpackdirs', 'up', 'uppercase',
    'urlbase', 'urltofile', 'var', 'vareq', 'varfn', 'varfnarray', 'varinlist',
    'varne', 'varstrs', 'varz', 'whilestmt', 'wnhypnymstoconcept', 'wninit',
    'wnsensestoconcept', 'wordindex', 'wordpath', 'writekb', 'xaddlen',
    'xaddnvar', 'xdump', 'xinc', 'xmlstr', 'xrename'
)


class NLPPlusLexer(RegexLexer):
    """
    For NLP++ source code.

    NLP++ is a programming language dedicated to natural language processing.
    An analyzer is a sequence of passes; each pass matches pattern rules
    against a parse tree and runs code regions when a rule fires.
    """

    name = 'NLP++'
    url = 'https://visualtext.org'
    aliases = ['nlp++', 'nlpplus']
    filenames = ['*.nlp', '*.pat']
    mimetypes = ['text/x-nlpplus']
    version_added = '2.22'

    tokens = {
        'root': [
            include('whitespace'),
            include('comments'),

            # @CODE ... @@CODE, @RULES, @POST, @NODES and friends
            (words(REGIONS, prefix=r'@@?', suffix=r'\b'), Keyword.Declaration),
            # the @@ that closes a rule
            (r'@@', Keyword.Declaration),

            # rule head: _noun [attrs] <- ... @@
            (r'<-', Operator),
            (r'\[', Punctuation, 'attributes'),

            # a literal character in a rule is escaped: \. \, \- \t
            (r'\\.', String.Escape),

            (words(CONSTANTS, prefix=r'_', suffix=r'\b'), Name.Constant),
            (r'_ROOT\b', Name.Constant),

            include('code'),

            # every other leading-underscore name is a node
            (r'_\w+', Name.Variable),
            (r'[\w$]+', Name),
            (r'[\[\]{}(),;]', Punctuation),
        ],

        # [...] is a rule element's modifier block, but the same brackets are an
        # array subscript in code, so this state has to handle expressions too.
        'attributes': [
            include('whitespace'),
            include('comments'),
            (r'\\.', String.Escape),
            (r'\]', Punctuation, '#pop'),
            (r'\[', Punctuation, '#push'),
            (words(ATTRIBUTES, prefix=r'\b(?i:', suffix=r')\b'), Name.Attribute),
            (words(CONSTANTS, prefix=r'_', suffix=r'\b'), Name.Constant),
            (r'_\w+', Name.Variable),
            include('code'),
            (r'[(),;]', Punctuation),
            (r'[\w$]+', Name),
        ],

        'code': [
            (r'\b(?i:if|else|while|return)\b', Keyword),
            # N("num"), S("sem"), X(...), G(...), L(...) reach parse-tree variables
            (r'\b[NSXGL](?=\s*\()', Name.Builtin.Pseudo),
            (words(BUILTINS, prefix=r'\b(?i:', suffix=r')\b(?=\s*\()'),
             Name.Builtin),
            (r'\b(?i:cap|cout|gp|group|inc)\b', Keyword),
            (r'[a-zA-Z_]\w*(?=\s*\()', Name.Function),
            include('literals'),
            (r'<<|>>|\+\+|--|==|!=|<=|>=|<>|&&|\|\||[-+*/%=<>!]', Operator),
        ],

        'literals': [
            (r'"', String.Double, 'string'),
            (r'0[xX][0-9a-fA-F]+', Number.Hex),
            (r'0[oO][0-7]+', Number.Oct),
            (r'0[bB][01]+', Number.Bin),
            (r'\d+\.\d+(?:[eE][+-]?\d+)?', Number.Float),
            (r'\d+', Number.Integer),
        ],

        'string': [
            (r'[^"\\]+', String.Double),
            (r'\\.', String.Escape),
            (r'"', String.Double, '#pop'),
        ],

        'comments': [
            (r'#.*?$', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment-block'),
        ],

        'comment-block': [
            (r'[^*/]+', Comment.Multiline),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],

        'whitespace': [
            (r'\s+', Whitespace),
        ],
    }

    def analyse_text(text):
        # *.pat is shared with unrelated formats, so look for the region
        # markers that structure every non-trivial NLP++ pass.
        if re.search(r'^\s*@(?:RULES|NODES|POST|PRE|CHECK|CODE)\b', text, re.M):
            # a rule rewrite alongside them makes it near certain
            return 0.9 if re.search(r'^\s*_\w+.*<-', text, re.M) else 0.5
        return 0.0


class _NLPPlusDataLexer(RegexLexer):
    """
    Shared states for the NLP++ data formats: comments, quoted strings,
    numbers, and the attribute=value pairs that .dict and .kbb files share.
    """

    tokens = {
        'comments': [
            (r'#.*?$', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment-block'),
        ],
        'comment-block': [
            (r'[^*/]+', Comment.Multiline),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
        'string': [
            (r'[^"\\\n]+', String.Double),
            (r'\\.', String.Escape),
            (r'"', String.Double, '#pop'),
            # an unterminated string ends with its line
            (r'\n', Whitespace, '#pop'),
        ],
        # name=value pairs; a value is a quoted string, a number, a bare word,
        # or a [list, "of", values]
        'attributes': [
            (r'[ \t]+', Whitespace),
            (r'([\w-]+)(\s*)(=)',
             bygroups(Name.Attribute, Whitespace, Operator)),
            (r'"', String.Double, 'string'),
            (r'-?\d+(?:\.\d+)?\b', Number),
            (r'[\[\],]', Punctuation),
            (r'[^\s"\[\],=]+', String),
        ],
    }


class NLPPlusDictLexer(_NLPPlusDataLexer):
    """
    For NLP++ dictionaries (.dict files).

    Each line is a headword (one or more words) followed by the
    attribute=value pairs the tokenizer attaches to matching text.
    """

    name = 'NLP++ Dictionary'
    url = 'https://visualtext.org'
    aliases = ['nlpplus-dict']
    filenames = ['*.dict']
    mimetypes = ['text/x-nlpplus-dict']
    version_added = '2.22'

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            include('comments'),
            # The headword: words separated by spaces, up to the first
            # attribute name. Words and spaces never overlap, so a long line
            # cannot make the match backtrack.
            (r'^[^\s#/=][^\s=]*(?:[ \t]+(?![\w-]+[ \t]*=)[^\s=]+)*',
             Name.Label, 'entry'),
        ],
        'entry': [
            (r'\n', Whitespace, '#pop'),
            include('attributes'),
        ],
    }


class NLPPlusKbbLexer(_NLPPlusDataLexer):
    """
    For NLP++ knowledge bases (.kbb files).

    An indented hierarchy of concepts; a concept may be followed by a colon
    and comma-separated attribute=value pairs.
    """

    name = 'NLP++ Knowledge Base'
    url = 'https://visualtext.org'
    aliases = ['nlpplus-kbb', 'kbb']
    filenames = ['*.kbb']
    mimetypes = ['text/x-nlpplus-kbb']
    version_added = '2.22'

    tokens = {
        'root': [
            # Indentation is the hierarchy, so it belongs to the concept rules
            # below; only newlines and blank or comment-only lines go here.
            (r'\n', Whitespace),
            (r'^[ \t]+(?=\n|#|/\*)', Whitespace),
            include('comments'),
            # a top-level concept
            (r'^([^\s#:/][^:\n]*?)(:)',
             bygroups(Name.Namespace, Punctuation), 'concept-attributes'),
            (r'^[^\s#:/][^:\n]*', Name.Namespace),
            # an indented concept
            (r'^([ \t]+)([^\s#:][^:\n]*?)(:)',
             bygroups(Whitespace, Name.Class, Punctuation),
             'concept-attributes'),
            (r'^([ \t]+)([^\s#:][^:\n]*)',
             bygroups(Whitespace, Name.Class)),
        ],
        'concept-attributes': [
            (r'\n', Whitespace, '#pop'),
            include('attributes'),
        ],
    }


class NLPPlusSeqLexer(RegexLexer):
    """
    For NLP++ analyzer sequences (analyzer.seq).

    One pass per line: the pass type, the pass file (or nil), and a comment.
    A leading slash makes a pass inactive; two slashes start a comment.
    """

    name = 'NLP++ Analyzer Sequence'
    url = 'https://visualtext.org'
    aliases = ['nlpplus-seq']
    filenames = ['analyzer.seq']
    mimetypes = ['text/x-nlpplus-seq']
    version_added = '2.22'

    # Pass types the engine's sequence reader accepts; "pat" is read as "nlp".
    PASS_TYPES = (
        'chartok', 'cmltok', 'cmltokenize', 'dicttok', 'dicttokz', 'end',
        'folder', 'gen', 'genhash', 'hash', 'intern', 'line', 'lines',
        'nintern', 'nlp', 'pat', 'python', 'rec', 'stub', 'tok', 'token',
        'tokenize'
    )

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'#.*?$', Comment.Single),
            (r'/\*', Comment.Multiline, 'comment-block'),
            (r'//.*?$', Comment.Single),
            # a lone slash at the start of a line switches the pass off
            (r'^/.*?$', Comment.Special),
            (words(PASS_TYPES, prefix=r'^(?i:', suffix=r')\b'), Keyword),
            (r'\bnil\b', Keyword.Constant),
            (r'[^\s#/]+', Name.Function),
        ],
        'comment-block': [
            (r'[^*/]+', Comment.Multiline),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
    }


class NLPPlusTreeLexer(RegexLexer):
    """
    For NLP++ parse trees (.tree files), as the engine writes them.

    Each line is a node, indented under its parent, with its offsets, type,
    flags and variables in brackets: _np [0,6,0,6,8,23,node,fired, ("x" 1)]
    """

    name = 'NLP++ Parse Tree'
    url = 'https://visualtext.org'
    aliases = ['nlpplus-tree']
    filenames = ['*.tree']
    mimetypes = ['text/x-nlpplus-tree']
    version_added = '2.22'

    tokens = {
        'root': [
            (r'\n', Whitespace),
            (r'^\*+[ \t]*$', Comment.Special),
            # pass headers between the per-pass snapshots
            (r'^([ \t]*)(PASS)([ \t]+)(\d+)([ \t]+)(\(.*?\))',
             bygroups(Whitespace, Generic.Heading, Whitespace, Number,
                      Whitespace, Generic.Subheading)),
            (r'^[A-Z][A-Z ]* TREE:[ \t]*$', Generic.Heading),
            # a node built by a rule, then a leaf token
            (r'^([ \t]*)(_\w+)([ \t]+)(?=\[)',
             bygroups(Whitespace, Name.Variable, Whitespace), 'record'),
            (r'^([ \t]*)(\S+)([ \t]+)(?=\[)',
             bygroups(Whitespace, Name, Whitespace), 'record'),
            # a whitespace token prints as blanks before its record
            (r'^[ \t]*(?=\[)', Whitespace, 'record'),
            (r'[^\n]+', Text),
        ],
        'record': [
            (r'\n', Whitespace, '#pop'),
            (r'[ \t]+', Whitespace),
            (r'\d+', Number),
            (words(('null', 'node', 'alpha', 'num', 'punct', 'white', 'ctrl',
                    'emoji'), suffix=r'\b'), Keyword.Type),
            (words(('b', 'un', 'sem', 'fired', 'blt'), suffix=r'\b'),
             Keyword),
            # ("name" value): the variable's name, then its value(s)
            (r'(\()([ \t]*)("(?:\\.|[^"\\\n])*")',
             bygroups(Punctuation, Whitespace, Name.Attribute)),
            (r'"(?:\\.|[^"\\\n])*"', String.Double),
            (r'[\[\](),]', Punctuation),
            (r'[^\s\[\](),"]+', Text),
        ],
    }
