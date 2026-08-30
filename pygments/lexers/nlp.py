"""
    pygments.lexers.nlp
    ~~~~~~~~~~~~~~~~~~~

    Lexers for NLP++ and the VisualText file formats.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include, words
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Whitespace

__all__ = ['NLPPlusLexer']

# Region markers introduce the parts of a pass: @RULES holds the pattern rules,
# @POST/@PRE/@CHECK the code attached to them, @NODES the tree region to run over.
REGIONS = (
    'CHECK', 'CODE', 'DECL', 'MULTI', 'NODES', 'PATH', 'POST', 'PRE', 'RULES'
)

# Node constants matching classes of text rather than a named node.
CONSTANTS = (
    'xALPHA', 'xCTRL', 'xEND', 'xNIL', 'xNUM', 'xSTART', 'xWHITE', 'xWILD'
)

# Modifiers inside a rule element's [...] block.
ATTRIBUTES = (
    'attr', 'attrs', 'da', 'deacc', 'deaccent', 'except', 'excepts', 'fail',
    'fails', 'gp', 'group', 'layer', 'layers', 'look', 'lookahead', 'match',
    'matches', 'max', 'min', 'nest', 'o', 'one', 'opt', 'option', 'optional',
    'pass', 'passes', 'plus', 'recurse', 'ren', 'rename', 's', 'singlet',
    'star', 't', 'tree', 'trig', 'trigger', 'unsealed'
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
    'openfile', 'pathconcept', 'percentstr', 'permuten', 'phraselength',
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
    version_added = '2.21'

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
            (r'\b(?i:and|or|not|in)\b', Operator.Word),
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
