import pytest
from pygments import lex
from pygments.token import Comment, Error, Name, Number, Operator, Punctuation, String, Text
from pygments.lexers.smalltalk import PharoLexer


def tokens_without_whitespace(source: str):
    lexer = PharoLexer()
    result = []
    for token_type, value in lex(source, lexer):
        if token_type is Text.Whitespace and value.isspace():
            continue
        result.append((token_type, value))
    return result


def first_token(source: str):
    toks = tokens_without_whitespace(source)
    assert toks, f'no tokens for {source!r}'
    return toks[0]


def test_parentheses_are_individual_tokens():
    toks = tokens_without_whitespace('((index')
    assert toks[:3] == [
        (Punctuation, '('),
        (Punctuation, '('),
        (Name.Variable, 'index'),
    ]


COMMENT_CASES = [
    ('"simple comment"', '"simple comment"'),
    (' "leading comment" token', '"leading comment"'),
    ('"""nested "" comment""" token', '"""nested "" comment"""'),
    ('"comment ""nested at end""" token', '"comment ""nested at end"""'),
    ('token "middle comment" next', '"middle comment"'),
    ('"multi\nline" token', '"multi\nline"'),
    ('first "trailing comment"', '"trailing comment"'),
]


@pytest.mark.parametrize('source, comment_text', COMMENT_CASES)
def test_single_comment_tokens(source, comment_text):
    token = next(tok for tok in tokens_without_whitespace(source)
                 if tok[0] in (Comment, Error))
    assert token == (Comment, comment_text)


def test_multiple_comments_remain_separate():
    tokens = tokens_without_whitespace('"first" "second"')
    assert tokens[0] == (Comment, '"first"')
    assert tokens[1] == (Comment, '"second"')


def test_multiple_comments_remain_separate2():
    tokens = tokens_without_whitespace('self "first" nil "second" true')
    assert len(tokens) == 5
    assert tokens[0] == (Name.Builtin.Pseudo, 'self')
    assert tokens[1] == (Comment, '"first"')
    assert tokens[2] == (Name.Builtin.Pseudo, 'nil')
    assert tokens[3] == (Comment, '"second"')
    assert tokens[4] == (Name.Builtin.Pseudo, 'true')


STRING_CASES = [
    ("'This is a string'", "'This is a string'"),
    ("' with ''escaped'' quotes '", "' with ''escaped'' quotes '"),
    ("'nested ''string'' example'", "'nested ''string'' example'"),
    ("'a'", "'a'"),
    ("'abc''def'", "'abc''def'"),
    ("' I need more space around me '", "' I need more space around me '"),
    ("'     more space!!!     '", "'     more space!!!     '"),
    ("'digits 123'", "'digits 123'"),
    ("'symbols +-*/'", "'symbols +-*/'"),
    ("'line\nbreak'", "'line\nbreak'"),
    ("self 'string'", "'string'"),
    ("'string' nil", "'string'"),
    ("true 'string' false", "'string'"),
]


@pytest.mark.parametrize('source, expected', STRING_CASES)
def test_string_literals(source, expected):
    token = next(tok for tok in tokens_without_whitespace(source)
                 if tok[0] in (String, Error))
    assert token == (String, expected)


CHAR_CASES = [
    ('$1', '$1'),
    ('$a', '$a'),
    ('$+', '$+'),
    ('$#', '$#'),
    ('$$', '$$'),
    ('$^', '$^'),
    ('$`', '$`'),
    ('$~', '$~'),
    ("'space '$ 'character'", "$ "),
]


@pytest.mark.parametrize('source, expected', CHAR_CASES)
def test_character_literals(source, expected):
    token = next(tok for tok in tokens_without_whitespace(source)
                 if tok[0] in (String.Char, Error))
    assert token == (String.Char, expected)


SYMBOL_CASES = [
    ('#foo', '#foo'),
    ('##foo', '##foo'),
    ('#foo:', '#foo:'),
    ('#foo::bar', '#foo::bar'),
    ('#foo::', '#foo::'),
    ("#'symbol string'", "#'symbol string'"),
    ("#'sym''bol'", "#'sym''bol'"),
    ('#+=/', '#+=/'),
    ('#al:pha', '#al:pha'),
    ('#alpha123beta456', '#alpha123beta456'),
    ('#foo::bar::', '#foo::bar::'),
    ('#FooBar', '#FooBar'),
    ('#alphaBeta', '#alphaBeta'),
    ('#+alpha', '#+'),
    ('#al=pha', '#al'),
    ('#alpha-', '#alpha'),
    ('#-literal', '#-'),
]


@pytest.mark.parametrize('source, expected', SYMBOL_CASES)
def test_symbol_literals(source, expected):
    assert first_token(source) == (String.Symbol, expected)


LITERAL_ARRAY_CASES = [
    ('#[', '#[', None),
    ('#(', '#(', None),
    ('#[1 2]', '#[', ']'),
    ('#(foo bar)', '#(', ')'),
    ('#[#[1 2]]', '#[', ']'),
    ('#(#[1 2] #[3])', '#(', ')'),
]


@pytest.mark.parametrize('source, first, last', LITERAL_ARRAY_CASES)
def test_literal_array_tokens(source, first, last):
    tokens = tokens_without_whitespace(source)
    assert tokens[0] == (String.Symbol, first)
    if last is not None:
        assert tokens[-1] == (Punctuation, last)
    else:
        assert tokens[-1][0] != Punctuation


IDENTIFIER_CASES = [
    ('identifier', Name.Variable, 'identifier'),
    ('_identifier', Name.Variable, '_identifier'),
    ('identifier123', Name.Variable, 'identifier123'),
    ('identifier_', Name.Variable, 'identifier_'),
    ('__iden__tifier__', Name.Variable, '__iden__tifier__'),
    ('first_Token', Name.Variable, 'first_Token'),
    ('_123', Name.Variable, '_123'),
    ('ZClass', Name.Class, 'ZClass'),
    ('Object', Name.Class, 'Object'),
    ('A', Name.Class, 'A'),
    ('A_B', Name.Class, 'A_B'),
    ('A-B', Name.Class, 'A'),
    ('foo123bar', Name.Variable, 'foo123bar'),
    ('firstToken+rest', Name.Variable, 'firstToken'),
    ('identifierWithUpper', Name.Variable, 'identifierWithUpper'),
]


@pytest.mark.parametrize('source, token_type, expected', IDENTIFIER_CASES)
def test_identifiers_and_classes(source, token_type, expected):
    assert first_token(source) == (token_type, expected)


RESERVED_CASES = [
    ('self', 'self'),
    ('super', 'super'),
    ('true', 'true'),
    ('false', 'false'),
    ('nil', 'nil'),
    ('thisContext', 'thisContext'),
]


@pytest.mark.parametrize('source, expected', RESERVED_CASES)
def test_reserved_words_pseudo_variables(source, expected):
    assert first_token(source) == (Name.Builtin.Pseudo, expected)


KEYWORD_CASES = [
    ('doSomething:', 'doSomething:'),
    ('firstToken:', 'firstToken:'),
    ('first:second:', 'first:'),
    ('first_Token:', 'first_Token:'),
    ('_firstToken:', '_firstToken:'),
    ('FooBar:', 'FooBar:'),
    ('alpha123:', 'alpha123:'),
    ('alpha:beta:', 'alpha:'),
    ('x:y:z:', 'x:'),
    ('identifierWithDigits1:', 'identifierWithDigits1:'),
    ('firstToken::', 'firstToken:'),
    ('firstToken::::::', 'firstToken:'),
]


@pytest.mark.parametrize('source, expected', KEYWORD_CASES)
def test_keyword_tokens(source, expected):
    assert first_token(source) == (Name.Function, expected)


def test_keyword_Function_call_tokens():
    tokens = tokens_without_whitespace('self alpha: 1 beta: 2 gamma: 3')
    assert len(tokens) == 7
    assert tokens[0] == (Name.Builtin.Pseudo, 'self')
    assert tokens[1] == (Name.Function, 'alpha:')
    assert tokens[2] == (Number, '1')
    assert tokens[3] == (Name.Function, 'beta:')
    assert tokens[4] == (Number, '2')
    assert tokens[5] == (Name.Function, 'gamma:')
    assert tokens[6] == (Number, '3')


BINARY_CASES = [
    ('+', '+'),
    ('+-=/', '+-=/'),
    ('+a', '+'),
    ('+1', '+'),
    ('+(', '+'),
    ('+`', '+'),
    ('|', '|'),
    ('~&', '~&'),
    ('@,%', '@,%'),
    ('·÷', '·÷'),
    ('- 12', '-'),
    ('*identifier', '*'),
]


@pytest.mark.parametrize('source, expected', BINARY_CASES)
def test_binary_selectors(source, expected):
    assert first_token(source) == (Operator, expected)


NUMBER_CASES = [
    ('1a', '1'),
    ('123b', '123'),
    ('3.2c', '3.2'),
    ('3 .2d', '3'),
    ('3. 2e', '3'),
    ('-12f', '-12'),
    ('-3.2g', '-3.2'),
    ('10r10h', '10r10'),
    ('2r10i', '2r10'),
    ('16rfaj', '16rfa'),
    ('400r380z', '400r380z'),
    ('10r10.2l', '10r10.2'),
    ('2r1010.1m', '2r1010.1'),
    ('16ra.an', '16ra.a'),
    ('8r12.44o', '8r12.44'),
    ('-10r10p', '-10r10'),
    ('10r1aq', '10r1'),
    ('2r10210r', '2r10'),
    ('16r3gs', '16r3'),
    ('8r192t', '8r1'),
    ('3.2e3u', '3.2e3'),
    ('1e-2v', '1e-2'),
]


@pytest.mark.parametrize('source, expected', NUMBER_CASES)
def test_number_literals(source, expected):
    assert first_token(source) == (Number, expected)


ERROR_CASES = [
    ('$', '$'),
    ('#', '#'),
    ('#123', '#'),
    ('#^', '#'),
    ('#`', '#'),
    ('# 1', '#'),
    ('1r1a', '1r1a'),
    ('#"', '#'),
    ('"unterminated', '"unterminated\n'),
    ("'unterminated", "'unterminated\n"),
    ('`', '`'),
    ('¿', '¿'),
    ('#12.3', '#'),
    ('##', '##'),
]


@pytest.mark.parametrize('source, expected', ERROR_CASES)
def test_error_tokens(source, expected):
    token_type, value = first_token(source)
    assert token_type is Error
    assert value == expected


PUNCTUATION_CASES = [
    ('(', '('),
    (')', ')'),
    ('[', '['),
    (']', ']'),
    ('{', '{'),
]


@pytest.mark.parametrize('source, expected', PUNCTUATION_CASES)
def test_punctuation_tokens(source, expected):
    assert first_token(source) == (Punctuation, expected)


def test_assignment_operator():
    assert first_token(':= value') == (Operator, ':=')


def test_message_send_sequence():
    tokens = tokens_without_whitespace('receiver doSomething: argument.')
    assert tokens[:4] == [
        (Name.Variable, 'receiver'),
        (Name.Function, 'doSomething:'),
        (Name.Variable, 'argument'),
        (Punctuation, '.'),
    ]


def test_comments_are_ignored_between_tokens():
    tokens = tokens_without_whitespace('first "comment" second')
    assert tokens[0] == (Name.Variable, 'first')
    assert tokens[1] == (Comment, '"comment"')
    assert tokens[2] == (Name.Variable, 'second')


def test_identifier_cannot_start_with_digit():
    tokens = tokens_without_whitespace('123firstToken')
    assert tokens[0] == (Number, '123')
    assert tokens[1] == (Name.Variable, 'firstToken')


def test_identifier_cannot_begin_with_binary_selector():
    tokens = tokens_without_whitespace('+firstToken')
    assert tokens[0] == (Operator, '+')
    assert tokens[1] == (Name.Variable, 'firstToken')


def test_identifier_is_split_by_binary_characters():
    tokens = tokens_without_whitespace('first+Token')
    assert tokens[0] == (Name.Variable, 'first')
    assert tokens[1] == (Operator, '+')
    assert tokens[2] == (Name.Class, 'Token')


def test_keyword_cannot_begin_with_binary_selector():
    tokens = tokens_without_whitespace('+alpha:')
    assert tokens[0] == (Operator, '+')
    assert tokens[1] == (Name.Function, 'alpha:')


def test_keyword_cannot_begin_with_digit():
    tokens = tokens_without_whitespace('123alpha:')
    assert tokens[0] == (Number, '123')
    assert tokens[1] == (Name.Function, 'alpha:')


def test_double_colon_produces_separate_token():
    tokens = tokens_without_whitespace('firstToken::')
    assert tokens[0] == (Name.Function, 'firstToken:')
    assert tokens[1] == (Punctuation, ':')


def test_unterminated_comment_after_identifier():
    tokens = tokens_without_whitespace('first "unfinished comment secondToken')
    assert len(tokens) == 2
    assert tokens[0] == (Name.Variable, 'first')
    assert tokens[1][0] is Error
    assert tokens[1][1].startswith('"unfinished comment secondToken')
