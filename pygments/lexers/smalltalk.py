"""
    pygments.lexers.smalltalk
    ~~~~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Smalltalk and related languages.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from typing import Tuple, Iterator
from pygments.lexer import Lexer, RegexLexer, include, bygroups, default
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Error

__all__ = ['SmalltalkLexer', 'NewspeakLexer', 'PharoLexer']


class SmalltalkLexer(RegexLexer):
    """
    For Smalltalk syntax.
    Contributed by Stefan Matthias Aust.
    Rewritten by Nils Winter.
    """
    name = 'Smalltalk'
    url = 'http://www.smalltalk.org/'
    filenames = ['*.st']
    aliases = ['smalltalk', 'squeak', 'st']
    mimetypes = ['text/x-smalltalk']
    version_added = '0.10'

    tokens = {
        'root': [
            (r'(<)(\w+:)(.*?)(>)', bygroups(Text, Keyword, Text, Text)),
            include('squeak fileout'),
            include('whitespaces'),
            include('method definition'),
            (r'(\|)([\w\s]*)(\|)', bygroups(Operator, Name.Variable, Operator)),
            include('objects'),
            (r'\^|\:=|\_', Operator),
            # temporaries
            (r'[\]({}.;!]', Text),
        ],
        'method definition': [
            # Not perfect can't allow whitespaces at the beginning and the
            # without breaking everything
            (r'([a-zA-Z]+\w*:)(\s*)(\w+)',
             bygroups(Name.Function, Text, Name.Variable)),
            (r'^(\b[a-zA-Z]+\w*\b)(\s*)$', bygroups(Name.Function, Text)),
            (r'^([-+*/\\~<>=|&!?,@%]+)(\s*)(\w+)(\s*)$',
             bygroups(Name.Function, Text, Name.Variable, Text)),
        ],
        'blockvariables': [
            include('whitespaces'),
            (r'(:)(\s*)(\w+)',
             bygroups(Operator, Text, Name.Variable)),
            (r'\|', Operator, '#pop'),
            default('#pop'),  # else pop
        ],
        'literals': [
            (r"'(''|[^'])*'", String, 'afterobject'),
            (r'\$.', String.Char, 'afterobject'),
            (r'#\(', String.Symbol, 'parenth'),
            (r'\)', Text, 'afterobject'),
            (r'(\d+r)?-?\d+(\.\d+)?(e-?\d+)?', Number, 'afterobject'),
        ],
        '_parenth_helper': [
            include('whitespaces'),
            (r'(\d+r)?-?\d+(\.\d+)?(e-?\d+)?', Number),
            (r'[-+*/\\~<>=|&#!?,@%\w:]+', String.Symbol),
            # literals
            (r"'(''|[^'])*'", String),
            (r'\$.', String.Char),
            (r'#*\(', String.Symbol, 'inner_parenth'),
        ],
        'parenth': [
            # This state is a bit tricky since
            # we can't just pop this state
            (r'\)', String.Symbol, ('root', 'afterobject')),
            include('_parenth_helper'),
        ],
        'inner_parenth': [
            (r'\)', String.Symbol, '#pop'),
            include('_parenth_helper'),
        ],
        'whitespaces': [
            # skip whitespace and comments
            (r'\s+', Text),
            (r'"(""|[^"])*"', Comment),
        ],
        'objects': [
            (r'\[', Text, 'blockvariables'),
            (r'\]', Text, 'afterobject'),
            (r'\b(self|super|true|false|nil|thisContext)\b',
             Name.Builtin.Pseudo, 'afterobject'),
            (r'\b[A-Z]\w*(?!:)\b', Name.Class, 'afterobject'),
            (r'\b[a-z]\w*(?!:)\b', Name.Variable, 'afterobject'),
            (r'#("(""|[^"])*"|[-+*/\\~<>=|&!?,@%]+|[\w:]+)',
             String.Symbol, 'afterobject'),
            include('literals'),
        ],
        'afterobject': [
            (r'! !$', Keyword, '#pop'),  # squeak chunk delimiter
            include('whitespaces'),
            (r'\b(ifTrue:|ifFalse:|whileTrue:|whileFalse:|timesRepeat:)',
             Name.Builtin, '#pop'),
            (r'\b(new\b(?!:))', Name.Builtin),
            (r'\:=|\_', Operator, '#pop'),
            (r'\b[a-zA-Z]+\w*:', Name.Function, '#pop'),
            (r'\b[a-zA-Z]+\w*', Name.Function),
            (r'\w+:?|[-+*/\\~<>=|&!?,@%]+', Name.Function, '#pop'),
            (r'\.', Punctuation, '#pop'),
            (r';', Punctuation),
            (r'[\])}]', Text),
            (r'[\[({]', Text, '#pop'),
        ],
        'squeak fileout': [
            # Squeak fileout format (optional)
            (r'^"(""|[^"])*"!', Keyword),
            (r"^'(''|[^'])*'!", Keyword),
            (r'^(!)(\w+)( commentStamp: )(.*?)( prior: .*?!\n)(.*?)(!)',
                bygroups(Keyword, Name.Class, Keyword, String, Keyword, Text, Keyword)),
            (r"^(!)(\w+(?: class)?)( methodsFor: )('(?:''|[^'])*')(.*?!)",
                bygroups(Keyword, Name.Class, Keyword, String, Keyword)),
            (r'^(\w+)( subclass: )(#\w+)'
             r'(\s+instanceVariableNames: )(.*?)'
             r'(\s+classVariableNames: )(.*?)'
             r'(\s+poolDictionaries: )(.*?)'
             r'(\s+category: )(.*?)(!)',
                bygroups(Name.Class, Keyword, String.Symbol, Keyword, String, Keyword,
                         String, Keyword, String, Keyword, String, Keyword)),
            (r'^(\w+(?: class)?)(\s+instanceVariableNames: )(.*?)(!)',
                bygroups(Name.Class, Keyword, String, Keyword)),
            (r'(!\n)(\].*)(! !)$', bygroups(Keyword, Text, Keyword)),
            (r'! !$', Keyword),
        ],
    }


class NewspeakLexer(RegexLexer):
    """
    For Newspeak syntax.
    """
    name = 'Newspeak'
    url = 'http://newspeaklanguage.org/'
    filenames = ['*.ns2']
    aliases = ['newspeak', ]
    mimetypes = ['text/x-newspeak']
    version_added = '1.1'

    tokens = {
        'root': [
            (r'\b(Newsqueak2)\b', Keyword.Declaration),
            (r"'[^']*'", String),
            (r'\b(class)(\s+)(\w+)(\s*)',
             bygroups(Keyword.Declaration, Text, Name.Class, Text)),
            (r'\b(mixin|self|super|private|public|protected|nil|true|false)\b',
             Keyword),
            (r'(\w+\:)(\s*)([a-zA-Z_]\w+)',
             bygroups(Name.Function, Text, Name.Variable)),
            (r'(\w+)(\s*)(=)',
             bygroups(Name.Attribute, Text, Operator)),
            (r'<\w+>', Comment.Special),
            include('expressionstat'),
            include('whitespace')
        ],

        'expressionstat': [
            (r'(\d+\.\d*|\.\d+|\d+[fF])[fF]?', Number.Float),
            (r'\d+', Number.Integer),
            (r':\w+', Name.Variable),
            (r'(\w+)(::)', bygroups(Name.Variable, Operator)),
            (r'\w+:', Name.Function),
            (r'\w+', Name.Variable),
            (r'\(|\)', Punctuation),
            (r'\[|\]', Punctuation),
            (r'\{|\}', Punctuation),

            (r'(\^|\+|\/|~|\*|<|>|=|@|%|\||&|\?|!|,|-|:)', Operator),
            (r'\.|;', Punctuation),
            include('whitespace'),
            include('literals'),
        ],
        'literals': [
            (r'\$.', String),
            (r"'[^']*'", String),
            (r"#'[^']*'", String.Symbol),
            (r"#\w+:?", String.Symbol),
            (r"#(\+|\/|~|\*|<|>|=|@|%|\||&|\?|!|,|-)+", String.Symbol)
        ],
        'whitespace': [
            (r'\s+', Text),
            (r'"[^"]*"', Comment)
        ],
    }

TokenTriplet = Tuple[int, object, str]  # (position, token type, value)

class PharoLexer(Lexer):
    """
    For Pharo syntax.
    Mostly follows the OCScanner rules from Pharo 13.
    Written by Kilian Kier.
    """

    name = 'Pharo'
    url = 'https://pharo.org/'
    filenames = ['*.st']
    aliases = ['pharo']
    mimetypes = ['text/x-pharo', 'text/x-smalltalk']
    version_added = '2.21'

    tokens = {}

    reserved_words = {
        'nil', 'true', 'false', 'self', 'super', 'thisContext'
    }
    binary_character = set("+-/\\*~<>=@,%|&?!·÷±×")
    special_punctuation = set('()[]{}.;!:')
    return_character = '^'

    def get_tokens_unprocessed(self, text: str) -> Iterator[TokenTriplet]:
        length = len(text)
        pos = 0

        while pos < length:
            char = text[pos]
            start = pos

            if char.isspace():
                pos = self._scan_whitespace(text, pos)
                yield start, Text.Whitespace, text[start:pos]
                continue

            if char == '"':
                token, pos, ok = self._scan_comment(text, pos)
                yield start, Comment if ok else Error, token
                continue

            if char == "'":
                token, pos, ok = self._scan_string_literal(text, pos)
                yield start, String if ok else Error, token
                continue

            if char == '$':
                token, pos, ok = self._scan_character_literal(text, pos)
                yield start, String.Char if ok else Error, token
                continue

            if char == '#':
                token, pos, tok = self._scan_hash_literal(text, pos)
                yield start, tok, token
                continue

            if self._starts_number(text, pos):
                token, pos, ok = self._scan_number(text, pos)
                yield start, Number if ok else Error, token
                continue

            if self._is_binary_char(char):
                token, pos = self._scan_binary_selector(text, pos)
                yield start, Operator, token
                continue

            if self._is_identifier_start(char):
                token, pos, tok = self._scan_identifier_or_keyword(text, pos)
                yield start, tok, token
                continue

            if char == ':' and pos + 1 < length and text[pos + 1] == '=':
                yield start, Operator, ':='
                pos += 2
                continue

            if char in self.special_punctuation:
                yield start, Punctuation, char
                pos += 1
                continue

            if char == self.return_character:
                yield start, Operator, char
                pos += 1
                continue

            yield start, Error, char
            pos += 1

        if not text.endswith('\n'):
            yield length, Text.Whitespace, '\n'

    @staticmethod
    def _scan_whitespace(text: str, pos: int) -> int:
        while pos < len(text) and text[pos].isspace():
            pos += 1
        return pos

    @staticmethod
    def _scan_comment(text: str, pos: int) -> Tuple[str, int, bool]:
        start = pos
        pos += 1
        while pos < len(text):
            char = text[pos]
            if char == '"':
                if pos + 1 < len(text) and text[pos + 1] == '"':
                    pos += 2
                    continue
                pos += 1
                return text[start:pos], pos, True
            pos += 1
        return text[start:], len(text), False

    @staticmethod
    def _scan_string_literal(text: str, pos: int) -> Tuple[str, int, bool]:
        start = pos
        pos += 1
        while pos < len(text):
            char = text[pos]
            if char == "'":
                if pos + 1 < len(text) and text[pos + 1] == "'":
                    pos += 2
                    continue
                pos += 1
                return text[start:pos], pos, True
            pos += 1
        return text[start:], len(text), False

    @staticmethod
    def _scan_character_literal(text: str, pos: int) -> Tuple[str, int, bool]:
        start = pos
        pos += 1
        if pos >= len(text) or (text[pos] == '\n' and pos == len(text) - 1):
            return text[start:pos], pos, False
        return text[start:pos + 1], pos + 1, True

    def _scan_hash_literal(self, text: str, pos: int) -> Tuple[str, int, object]:
        start = pos
        pos += 1
        while pos < len(text) and text[pos] == '#':
            pos += 1
        if pos >= len(text):
            return text[start:], pos, Error

        char = text[pos]

        if char in '([':
            pos += 1
            return text[start:pos], pos, String.Symbol

        if char == "'":
            token, pos, ok = self._scan_string_literal(text, pos)
            return text[start:pos], pos, String.Symbol if ok else Error

        if self._is_identifier_start(char):
            end = self._scan_symbol_name(text, pos)
            return text[start:end], end, String.Symbol

        if self._is_binary_char(char):
            end = self._scan_binary_selector(text, pos)[1]
            return text[start:end], end, String.Symbol

        return text[start:pos], pos, Error

    def _scan_symbol_name(self, text: str, pos: int) -> int:
        pos = self._scan_name(text, pos)
        while pos < len(text) and text[pos] == ':':
            pos += 1
            pos = self._scan_name(text, pos)
        return pos

    def _scan_binary_selector(self, text: str, pos: int) -> Tuple[str, int]:
        start = pos
        while pos < len(text) and self._is_binary_char(text[pos]):
            pos += 1
        return text[start:pos], pos

    def _scan_identifier_or_keyword(self, text: str, pos: int) -> Tuple[str, int, object]:
        start = pos
        pos = self._scan_name(text, pos)
        token_text = text[start:pos]
        token_type = self._classify_identifier(token_text)

        if pos < len(text) and text[pos] == ':' and not (pos + 1 < len(text) and text[pos + 1] == '='):
            pos += 1
            token_text = text[start:pos]
            token_type = Name.Function

        return token_text, pos, token_type

    def _scan_name(self, text: str, pos: int) -> int:
        while pos < len(text) and self._is_identifier_part(text[pos]):
            pos += 1
        return pos

    def _scan_number(self, text: str, pos: int) -> Tuple[str, int, bool]:
        start = pos

        if text[pos] == '-':
            pos += 1

        radix_start = pos
        pos = self._consume_digits(text, pos)

        if len(text) > pos > radix_start and text[pos] in 'rR':
            base = int(text[radix_start:pos])
            pos += 1
            if base <= 1:
                pos = self._consume_literal_tail(text, pos)
                return text[start:pos], pos, False
            digits_consumed, pos = self._consume_radix_digits(text, pos, base)
            if not digits_consumed:
                return text[start:pos], pos, False
            return text[start:pos], pos, True

        if pos == radix_start:
            return text[start:start + 1], start + 1, False

        if pos < len(text) and text[pos] == '.' \
                and pos + 1 < len(text) and text[pos + 1].isdigit():
            pos += 1
            pos = self._consume_digits(text, pos)

        if pos < len(text) and text[pos] in 'eEdD':
            exp_pos = pos + 1
            if exp_pos < len(text) and text[exp_pos] in '+-':
                exp_pos += 1
            if exp_pos < len(text) and text[exp_pos].isdigit():
                pos = self._consume_digits(text, exp_pos)

        return text[start:pos], pos, True

    @staticmethod
    def _consume_digits(text: str, pos: int) -> int:
        while pos < len(text) and text[pos].isdigit():
            pos += 1
        return pos

    @staticmethod
    def _consume_literal_tail(text: str, pos: int) -> int:
        while pos < len(text) and (text[pos].isalnum() or text[pos] == '.'):
            pos += 1
        return pos

    def _consume_radix_digits(self, text: str, pos: int, base: int) -> Tuple[bool, int]:
        consumed = False
        has_fraction = False
        length = len(text)

        while pos < length:
            char = text[pos]
            if char == '.':
                if has_fraction:
                    break
                if pos + 1 < length and self._radix_digit_value(
                        text[pos + 1]
                ) < base:
                    has_fraction = True
                    pos += 1
                    continue
                break

            value = self._radix_digit_value(char)
            if 0 <= value < base:
                consumed = True
                pos += 1
                continue
            break
        return consumed, pos

    @staticmethod
    def _radix_digit_value(char: str) -> int:
        if char.isdigit():
            return ord(char) - ord('0')
        lower = char.lower()
        if 'a' <= lower <= 'z':
            return ord(lower) - ord('a') + 10
        return -1

    @staticmethod
    def _starts_number(text: str, pos: int) -> bool:
        if text[pos].isdigit():
            return True
        return (
                text[pos] == '-'
                and pos + 1 < len(text)
                and text[pos + 1].isdigit()
        )

    @staticmethod
    def _is_identifier_start(char: str) -> bool:
        return char.isalpha() or char == '_'

    @staticmethod
    def _is_identifier_part(char: str) -> bool:
        return char.isalnum() or char == '_'

    def _is_binary_char(self, char: str) -> bool:
        return char in self.binary_character

    def _classify_identifier(self, value: str) -> object:
        if value in self.reserved_words:
            return Name.Builtin.Pseudo
        if value and value[0].isupper():
            return Name.Class
        return Name.Variable
