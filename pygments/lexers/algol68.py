"""
    pygments.lexers.algol68
    ~~~~~~~~~~~~~~~~~~~~~~~

    Lexer for Algol 68.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include
from pygments.token import Comment, Keyword, Name, Number, Operator, \
    Punctuation, String, Whitespace

__all__ = ['Algol68Lexer']

# In UPPER stropping a bold word consists of capitals only (GNU Algol 68
# also allows single underscores between them), so 8776H1259 is a formula.
_BOLD = r'[A-Z]+(?:_[A-Z]+)*'
_END = r'(?![A-Z])'


class Algol68Lexer(RegexLexer):
    """
    Lexer for Algol 68 in UPPER stropping, in which bold words (keywords,
    modes and operators) are written in capitals and identifiers in lower
    case.  It covers the language of the Revised Report and the extensions
    of Algol 68 Genie and GNU Algol 68.

    Mode indicants and operators cannot be told apart lexically.  The lexer
    therefore looks at the declarations of the whole text (``MODE X =``,
    ``OP X =``, ``PRIO X =``) and colors every use of a bold word
    accordingly.  A bold word declared elsewhere, for instance in an
    included file, is taken as an operator if its uses mostly stand where
    only an operator can stand, and as a mode otherwise.
    """

    name = 'Algol 68'
    url = 'https://en.wikipedia.org/wiki/ALGOL_68'
    aliases = ['algol68', 'a68']
    filenames = ['*.a68']
    mimetypes = ['text/x-algol68']
    version_added = '2.22'

    keywords = {
        'BEGIN', 'END', 'IF', 'THEN', 'ELIF', 'ELSE', 'FI', 'CASE', 'IN',
        'OUSE', 'OUT', 'ESAC', 'FOR', 'FROM', 'BY', 'TO', 'DOWNTO', 'WHILE',
        'UNTIL', 'DO', 'OD', 'GOTO', 'GO', 'EXIT', 'PAR', 'ASSERT', 'THEF',
        'ELSF', 'HEAP', 'LOC', 'WORD', 'NEST',
    }
    declarations = {'MODE', 'OP', 'PRIO', 'PROC'}
    namespaces = {'ACCESS', 'PUB', 'MODULE', 'DEF', 'POSTLUDE', 'FED', 'EGG'}
    types = {
        'INT', 'REAL', 'BOOL', 'CHAR', 'STRING', 'VOID', 'BITS', 'BYTES',
        'COMPL', 'COMPLEX', 'SEMA', 'FILE', 'CHANNEL', 'FORMAT',
        'REF', 'FLEX', 'LONG', 'SHORT', 'STRUCT', 'UNION',
    }
    constants = {'TRUE', 'FALSE', 'NIL', 'SKIP', 'EMPTY'}
    word_operators = {
        # Part of the syntax.
        'ANDTH', 'OREL', 'ANDF', 'ORF', 'ANDTHEN', 'ORELSE', 'IS', 'ISNT',
        'OF', 'AT',
        # Declared in the standard prelude.
        'ABS', 'AND', 'ARG', 'BIN', 'CLEAR', 'CONJ', 'DIVAB', 'DOWN', 'ELEM',
        'ELEMS', 'ENTIER', 'EQ', 'GE', 'GT', 'I', 'IM', 'LE', 'LENG',
        'LEVEL', 'LT', 'LWB', 'MINUSAB', 'MOD', 'MODAB', 'NE', 'NOT', 'ODD',
        'OR', 'OVER', 'OVERAB', 'PLUSAB', 'PLUSTO', 'RE', 'REPR', 'ROUND',
        'SET', 'SHL', 'SHORTEN', 'SHR', 'SIGN', 'TEST', 'TIMESAB', 'UP',
        'UPB', 'XOR',
    }

    # Identifiers of the standard preludes, without blanks and underscores.
    # Those in long_builtins may be preceded by "long" or "short", as in
    # "long max int" or "long long pi".
    long_builtins = {
        'pi', 'maxint', 'minint', 'maxreal', 'minreal', 'smallreal',
        'maxbits', 'bitswidth', 'byteswidth', 'intwidth', 'realwidth',
        'expwidth', 'infinity', 'minusinfinity', 'sqrt', 'exp', 'ln', 'log',
        'sin', 'cos', 'tan', 'arcsin', 'arccos', 'arctan', 'random',
        'nextrandom', 'bitspack', 'bytespack', 'stringtoreal',
    }
    builtins = {
        # Revised Report.
        'blank', 'flip', 'flop', 'nullcharacter', 'errorchar', 'maxabschar',
        'intlengths', 'intshorths', 'reallengths', 'realshorths',
        'bitslengths', 'bitsshorths', 'byteslengths', 'bytesshorths',
        'standin', 'standout', 'standback', 'standinchannel',
        'standoutchannel', 'standbackchannel', 'print', 'printf', 'write',
        'writef', 'read', 'readf', 'put', 'putf', 'get', 'getf', 'newline',
        'newpage', 'space', 'backspace', 'whole', 'fixed', 'float', 'open',
        'close', 'establish', 'create', 'associate', 'lock', 'scratch',
        'reset', 'maketerm', 'onlogicalfileend', 'onphysicalfileend',
        'onpageend', 'onlineend', 'onformatend', 'onvalueerror',
        'oncharerror', 'charnumber', 'linenumber', 'pagenumber',
        'charinstring', 'stop',
        # Algol 68 Genie.
        'standerror', 'lastcharinstring', 'stringinstring', 'readint',
        'readlongint', 'readreal', 'readlongreal', 'readbool', 'readchar',
        'readstring', 'readline', 'eof', 'eoln', 'isalnum', 'isalpha',
        'isdigit', 'islower', 'isspace', 'isupper', 'isxdigit', 'tolower',
        'toupper',
        # GNU Algol 68.
        'eofchar', 'replacementchar', 'compllengths', 'complshorths',
        'firstrandom', 'rnd', 'stdin', 'stdout', 'stderr', 'argc', 'argv',
        'errno', 'strerror', 'fopen', 'fcreate', 'fconnect', 'fclose',
        'fsize', 'lseek', 'getchar', 'putchar', 'fgetc', 'fputc', 'gets',
        'puts', 'fgets', 'fputs', 'posixexit', 'perror', 'getenv',
    }

    def bold_word(lexer, match):
        word = match.group()
        if word in lexer.keywords:
            token = Keyword
        elif word in lexer.declarations:
            token = Keyword.Declaration
        elif word in lexer.namespaces:
            token = Keyword.Namespace
        elif word in lexer.types:
            token = Keyword.Type
        elif word in lexer.constants:
            token = Keyword.Constant
        elif word in lexer.word_operators:
            token = Operator.Word
        else:
            # A user-defined mode or operator, see get_tokens_unprocessed.
            token = Name.Class
        yield match.start(), token, word

    def identifier(lexer, match):
        # "max int", "max_int" and "maxint" denote the same identifier.
        name = re.sub(r'[\s_]', '', match.group())
        bare = re.sub(r'^(?:long|short)+', '', name)
        if name in lexer.builtins or bare in lexer.long_builtins:
            token = Name.Builtin
        else:
            token = Name
        yield match.start(), token, match.group()

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            include('pragments'),
            (r'"', String, 'string'),
            (r'\$', String.Other, 'format'),
            (r'\d+r[0-9a-z]+', Number.Hex),
            (r'(\d+\.\d*|\.\d+)([eE][+-]?\d+)?|\d+[eE][+-]?\d+', Number.Float),
            (r'\d+', Number.Integer),
            (_BOLD, bold_word),
            # An identifier consists of lower-case words, which may be
            # separated by blanks.
            (r'[a-z][a-z0-9_]*(?:[ \t]+[a-z0-9][a-z0-9_]*)*', identifier),
            (r':/=:|:=:|:=|=:', Operator),
            # Including the characters of the publication language.
            (r'[-+*/%^&~<>=!?\\≠≤≥×÷∧∨¬↑↓⌈⌊⊥]+(?::=|=:)?', Operator),
            (r'\|:|[()\[\],;:|@.]', Punctuation),
        ],
        # Comments and pragmats.  As in GNU Algol 68, the terminator is found
        # even inside a longer word, and only pragmats and nestable comments
        # skip strings.
        'pragments': [
            (r'#[^#]*#', Comment.Multiline),
            (r'¢[^¢]*¢', Comment.Multiline),
            (r'COMMENT' + _END + r'[\s\S]*?COMMENT', Comment.Multiline),
            (r'CO' + _END + r'[\s\S]*?CO', Comment.Multiline),
            (r'NOTE' + _END, Comment.Multiline, 'note'),
            (r'\{', Comment.Multiline, 'brace'),
            (r'PRAGMAT' + _END, Comment.Preproc, 'pragmat'),
            (r'PR' + _END, Comment.Preproc, 'pr'),
        ],
        'note': [
            (r'"[^"\n]*"', Comment.Multiline),
            (r'NOTE', Comment.Multiline, '#push'),
            (r'ETON', Comment.Multiline, '#pop'),
            (r'[^"NE]+|[\s\S]', Comment.Multiline),
        ],
        'brace': [
            (r'"[^"\n]*"', Comment.Multiline),
            (r'\{', Comment.Multiline, '#push'),
            (r'\}', Comment.Multiline, '#pop'),
            (r'[^"{}]+|"', Comment.Multiline),
        ],
        'pragmat': [
            (r'PRAGMAT', Comment.Preproc, '#pop'),
            include('pragmat-text'),
        ],
        'pr': [
            (r'PR', Comment.Preproc, '#pop'),
            include('pragmat-text'),
        ],
        'pragmat-text': [
            (r'''"[^"\n]*"|'[^'\n]*\'''', Comment.Preproc),
            (r'''[^"'P]+|[\s\S]''', Comment.Preproc),
        ],
        'string': [
            (r'""', String.Escape),
            # GNU Algol 68 escapes.
            (r"'[nfrt']|'\([^)\n]*\)", String.Escape),
            (r'"', String, '#pop'),
            (r"[^\"'\n]+|'", String),
        ],
        'format': [
            (r'\$', String.Other, '#pop'),
            (r'"', String, 'string'),
            (r'[^$"]+', String.Other),
        ],
    }

    def get_tokens_unprocessed(self, text, stack=('root',)):
        tokens = list(RegexLexer.get_tokens_unprocessed(self, text, stack))
        significant = [(token, value) for _, token, value in tokens
                       if token is not Whitespace and token not in Comment]
        kinds = self.declared_bold_words(significant)
        kinds.update(self.guessed_bold_words(significant, kinds))
        for index, token, value in tokens:
            if token is Name.Class:
                token = kinds.get(value, Name.Class)
            yield index, token, value

    @staticmethod
    def declared_bold_words(significant):
        """Maps the bold words declared in the text to their token type:
        modes declared by MODE, operators by OP or PRIO, and modules named
        after ACCESS or MODULE."""
        openers = {'(', '[', 'BEGIN', 'IF', 'CASE', 'DO', 'PAR'}
        closers = {')', ']', 'END', 'FI', 'ESAC', 'OD'}
        kinds = {}
        kind = None       # token type of the declaration being scanned
        depth = 0
        head = False      # before the "=" of a declared name
        for i, (token, value) in enumerate(significant):
            following = significant[i + 1][1] if i + 1 < len(significant) else ''
            if value in ('MODE', 'OP', 'PRIO') and token in Keyword:
                kind = Name.Class if value == 'MODE' else Operator.Word
                depth, head = 0, True
            elif value in ('ACCESS', 'MODULE') and token in Keyword:
                kind, depth, head = Name.Namespace, 0, True
            elif kind is None:
                continue
            elif token is Name.Class and head and depth == 0 and (
                    kind is Name.Namespace or following.startswith('=')
                    and not following.startswith('=:')):
                kinds.setdefault(value, kind)
                head = kind is Name.Namespace
            elif kind is Name.Namespace and value in (',', 'PUB'):
                continue
            elif kind is Name.Namespace:
                kind = None
            elif value in openers:
                depth += 1
            elif value in closers:
                depth -= 1
                if depth < 0:
                    kind = None
            elif depth == 0 and value == ',':
                head = True
            elif depth == 0 and value == ';':
                kind = None
        return kinds

    @staticmethod
    def guessed_bold_words(significant, declared):
        """Guesses which of the bold words not declared in the text are
        operators.  An operator follows an operand or another operator,
        precedes a denotation or an open parenthesis, or begins an argument
        of a call or an element of a display.  A mode follows REF, FLEX,
        HEAP, LOC, PROC or ], or begins a declaration "X x =" or "X x :="."""
        def is_operand(token):
            return token is not Name.Class and any(
                token in kind for kind in (Name, Number, String, Keyword.Constant))

        def in_arguments(i):
            # Whether significant[i] stands inside "f (...)" or "((...))",
            # where no formal parameters can stand.
            depth = 0
            for j in range(i - 1, max(0, i - 200), -1):
                value = significant[j][1]
                if value in (')', ']'):
                    depth += 1
                elif value in ('(', '[') and depth > 0:
                    depth -= 1
                elif value in ('(', '['):
                    token, value = significant[j - 1]
                    return value == '(' or token in (Name, Name.Builtin)
            return False

        mode_before = {'REF', 'FLEX', 'HEAP', 'LOC', 'PROC', ']'}
        end = [(Whitespace, '')] * 2
        votes = {}
        for i in range(1, len(significant)):
            token, value = significant[i]
            if token is not Name.Class or value in declared:
                continue
            before, prior = significant[i - 1]
            (after, next_value), (_, then) = (significant[i + 1:i + 3] + end)[:2]
            if prior in mode_before \
                    or after in (Name, Name.Builtin) and then in ('=', ':='):
                vote = -1
            elif is_operand(before) or before in Operator or prior == '|' \
                    or after in Number or after in String or next_value == '(' \
                    or prior in ('(', ',') and in_arguments(i):
                vote = 1
            else:
                vote = 0
            votes[value] = votes.get(value, 0) + vote
        return {word: Operator.Word for word, vote in votes.items() if vote > 0}
