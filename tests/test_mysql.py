# -*- coding: utf-8 -*-
"""
    Pygments MySQL lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers.sql import MySqlLexer

from pygments.token import \
    Comment, \
    Keyword, \
    Literal, \
    Name, \
    Number, \
    Operator, \
    Punctuation, \
    String, \
    Text


@pytest.fixture(scope='module')
def lexer():
    yield MySqlLexer()


@pytest.mark.parametrize('text', ('1', '22', '22 333', '22 a', '22+', '22)', '22\n333', '22\r\n333'))
def test_integer_literals_positive_match(lexer, text):
    """Validate that integer literals are tokenized as integers."""
    token = list(lexer.get_tokens(text))[0]
    assert token[0] == Number.Integer
    assert token[1] in {'1', '22'}


@pytest.mark.parametrize('text', ('1a', '1A', '1.', '1ひ', '1$', '1_', '1\u0080', '1\uffff'))
def test_integer_literals_negative_match(lexer, text):
    """Validate that non-integer texts are not matched as integers."""
    assert list(lexer.get_tokens(text))[0][0] != Number.Integer


@pytest.mark.parametrize(
    'text',
    (
        '.123', '1.23', '123.',
        '1e10', '1.0e10', '1.e-10', '.1e+10',
    ),
)
def test_float_literals(lexer, text):
    assert list(lexer.get_tokens(text))[0] == (Number.Float, text)


@pytest.mark.parametrize('text', ("X'0af019'", "x'0AF019'", "0xaf019"))
def test_hexadecimal_literals(lexer, text):
    assert list(lexer.get_tokens(text))[0] == (Number.Hex, text)


@pytest.mark.parametrize('text', ("B'010'", "b'010'", "0b010"))
def test_binary_literals(lexer, text):
    assert list(lexer.get_tokens(text))[0] == (Number.Bin, text)


@pytest.mark.parametrize(
    'text',
    (
        "{d'2020-01-01'}", "{ d ' 2020^01@01 ' }",
        "{t'8 9:10:11'}", "{ t ' 09:10:11.12 ' }", "{ t ' 091011 ' }",
        '{ts"2020-01-01 09:10:11"}', "{ ts ' 2020@01/01  09:10:11 ' }",
    ),
)
def test_temporal_literals(lexer, text):
    assert list(lexer.get_tokens(text))[0] == (Literal.Date, text)


@pytest.mark.parametrize(
    'text, expected_types',
    (
        (r"'a'", (String.Single,) * 3),
        (r"""'""'""", (String.Single,) * 3),
        (r"''''", (String.Single, String.Escape, String.Single)),
        (r"'\''", (String.Single, String.Escape, String.Single)),
        (r'"a"', (String.Double,) * 3),
        (r'''"''"''', (String.Double,) * 3),
        (r'""""', (String.Double, String.Escape, String.Double)),
        (r'"\""', (String.Double, String.Escape, String.Double)),
    ),
)
def test_string_literals(lexer, text, expected_types):
    tokens = list(lexer.get_tokens(text))[:len(expected_types)]
    assert all(t[0] == e for t, e in zip(tokens, expected_types))


@pytest.mark.parametrize(
    'text',
    (
        "@a", "@1", "@._.$",
        "@'?'", """@'abc''def"`ghi'""",
        '@"#"', '''@"abc""def'`ghi"''',
        '@`^`', """@`abc``def'"ghi`""",
        "@@timestamp",
        "@@session.auto_increment_offset",
        "@@global.auto_increment_offset",
        "@@persist.auto_increment_offset",
        "@@persist_only.auto_increment_offset",
        '?',
    ),
)
def test_variables(lexer, text):
    tokens = list(lexer.get_tokens(text))
    assert all(t[0] == Name.Variable for t in tokens[:-1])
    assert ''.join([t[1] for t in tokens]).strip() == text.strip()


@pytest.mark.parametrize('text', ('true', 'false', 'null', 'unknown'))
def test_constants(lexer, text):
    assert list(lexer.get_tokens(text))[0] == (Name.Constant, text)


@pytest.mark.parametrize('text', ('-- abc', '--\tabc', '#abc'))
def test_comments_single_line(lexer, text):
    # Test the standalone comment.
    tokens = list(lexer.get_tokens(text))
    assert tokens[0] == (Comment.Single, text)

    # Test the comment with mixed tokens.
    tokens = list(lexer.get_tokens('select' + text + '\nselect'))
    assert tokens[0] == (Keyword, 'select')
    assert tokens[1] == (Comment.Single, text)
    assert tokens[-2] == (Keyword, 'select')


@pytest.mark.parametrize(
    'text',
    (
        '/**/a', '/*a*b/c*/a', '/*\nabc\n*/a',
        '/* /* */a'
    )
)
def test_comments_multi_line(lexer, text):
    tokens = list(lexer.get_tokens(text))
    assert all(token[0] == Comment.Multiline for token in tokens[:-2])
    assert ''.join(token[1] for token in tokens).strip() == text.strip()

    # Validate nested comments are not supported.
    assert tokens[-2][0] != Comment.Multiline


@pytest.mark.parametrize(
    'text', ('BKA', 'SEMIJOIN'))
def test_optimizer_hints(lexer, text):
    good = '/*+ ' + text + '(), */'
    ignore = '/* ' + text + ' */'
    bad1 = '/*+ a' + text + '() */'
    bad2 = '/*+ ' + text + 'a */'
    assert (Comment.Preproc, text) in lexer.get_tokens(good)
    assert (Comment.Preproc, text) not in lexer.get_tokens(ignore)
    assert (Comment.Preproc, text) not in lexer.get_tokens(bad1)
    assert (Comment.Preproc, text) not in lexer.get_tokens(bad2)


@pytest.mark.parametrize(
    'text, expected_types',
    (
        # SET exceptions
        ('SET', (Keyword,)),
        ('SET abc = 1;', (Keyword,)),
        ('SET @abc = 1;', (Keyword,)),
        ('CHARACTER SET latin1', (Keyword, Text, Keyword)),
        ('SET("r", "g", "b")', (Keyword.Type, Punctuation)),
        ('SET ("r", "g", "b")', (Keyword.Type, Text, Punctuation)),
    ),
)
def test_exceptions(lexer, text, expected_types):
    tokens = list(lexer.get_tokens(text))[:len(expected_types)]
    assert all(t[0] == e for t, e in zip(tokens, expected_types))


@pytest.mark.parametrize(
    'text',
    (
        'SHOW', 'CREATE', 'ALTER', 'DROP',
        'SELECT', 'INSERT', 'UPDATE', 'DELETE',
        'WHERE', 'GROUP', 'ORDER', 'BY', 'AS',
        'DISTINCT', 'JOIN', 'WITH', 'RECURSIVE',
        'PARTITION', 'NTILE', 'MASTER_PASSWORD', 'XA',
        'REQUIRE_TABLE_PRIMARY_KEY_CHECK', 'STREAM',
    ),
)
def test_keywords(lexer, text):
    assert list(lexer.get_tokens(text))[0] == (Keyword, text)


@pytest.mark.parametrize(
    'text',
    (
        # Standard
        'INT(', 'VARCHAR(', 'ENUM(', 'DATETIME', 'GEOMETRY', 'POINT', 'JSON',
        # Aliases and compatibility
        'FIXED', 'MEDIUMINT', 'INT3', 'REAL', 'SERIAL',
        'LONG', 'NATIONAL', 'PRECISION', 'VARYING',
    ),
)
def test_data_types(lexer, text):
    assert list(lexer.get_tokens(text))[0] == (Keyword.Type, text.strip('('))


@pytest.mark.parametrize(
    'text',
    (
        # Common
        'CAST', 'CONCAT_WS', 'DAYNAME', 'IFNULL', 'NOW', 'SUBSTR',
        # Less common
        'CAN_ACCESS_COLUMN', 'JSON_CONTAINS_PATH', 'ST_GEOMFROMGEOJSON',
    ),
)
def test_functions(lexer, text):
    assert list(lexer.get_tokens(text + '('))[0] == (Name.Function, text)
    assert list(lexer.get_tokens(text + ' ('))[0] == (Name.Function, text)


@pytest.mark.parametrize(
    'text',
    (
        'abc_$123', '上市年限', 'ひらがな', '123_$abc', '123ひらがな',
    ),
)
def test_schema_object_names_unquoted(lexer, text):
    tokens = list(lexer.get_tokens(text))[:-1]
    assert all(token[0] == Name for token in tokens)
    assert ''.join(token[1] for token in tokens) == text


@pytest.mark.parametrize(
    'text',
    (
        '`a`', '`1`', '`上市年限`', '`ひらがな`', '`select`', '`concat(`',
        '`-- `', '`/*`', '`#`',
    ),
)
def test_schema_object_names_quoted(lexer, text):
    tokens = list(lexer.get_tokens(text))[:-1]
    assert tokens[0] == (Name.Quoted, '`')
    assert tokens[1] == (Name.Quoted, text[1:-1])
    assert tokens[2] == (Name.Quoted, '`')
    assert ''.join(token[1] for token in tokens) == text


@pytest.mark.parametrize('text', ('````', ))
def test_schema_object_names_quoted_escaped(lexer, text):
    """Test quoted schema object names with escape sequences."""
    tokens = list(lexer.get_tokens(text))[:-1]
    assert tokens[0] == (Name.Quoted, '`')
    assert tokens[1] == (Name.Quoted.Escape, text[1:-1])
    assert tokens[2] == (Name.Quoted, '`')
    assert ''.join(token[1] for token in tokens) == text


@pytest.mark.parametrize(
    'text',
    ('+', '*', '/', '%', '&&', ':=', '!', '<', '->>', '^', '|', '~'),
)
def test_operators(lexer, text):
    assert list(lexer.get_tokens(text))[0] == (Operator, text)


@pytest.mark.parametrize(
    'text, expected_types',
    (
        ('abc.efg', (Name, Punctuation, Name)),
        ('abc,efg', (Name, Punctuation, Name)),
        ('MAX(abc)', (Name.Function, Punctuation, Name, Punctuation)),
        ('efg;', (Name, Punctuation)),
    ),
)
def test_punctuation(lexer, text, expected_types):
    tokens = list(lexer.get_tokens(text))[:len(expected_types)]
    assert all(t[0] == e for t, e in zip(tokens, expected_types))
