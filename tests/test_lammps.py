"""
    Test the LAMMPS input-script lexer.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import get_lexer_by_name, get_lexer_for_filename
from pygments.token import Comment, Error, Keyword, Name, Number, Punctuation, String


def tokens(text):
    lexer = get_lexer_by_name('lammps', stripnl=False, ensurenl=False)
    result = list(lexer.get_tokens_unprocessed(text))
    assert ''.join(value for _, _, value in result) == text
    offset = 0
    for position, token, value in result:
        assert position == offset
        assert value
        assert token is not Error
        offset += len(value)
    return [(token, value) for _, token, value in result]


@pytest.mark.parametrize('filename', ['simulation.lmp', 'in.lammps'])
def test_filename(filename):
    assert get_lexer_for_filename(filename).name == 'LAMMPS'


def test_commands_and_arguments():
    result = tokens('  units lj\npair_style lj/cut 2.5\nfix 1 all nve\n')
    assert [value for token, value in result if token is Keyword] == [
        'units', 'pair_style', 'fix',
    ]
    assert (Name, 'lj/cut') in result
    assert (Number.Float, '2.5') in result


@pytest.mark.parametrize('value', ['0', '123', '-4', '+17'])
def test_integers(value):
    assert (Number.Integer, value) in tokens('run ' + value)


@pytest.mark.parametrize('value', ['.5', '1.', '-2.5', '+3.0e-4', '2E+3'])
def test_floats(value):
    assert (Number.Float, value) in tokens('timestep ' + value)


@pytest.mark.parametrize('value', ['2*4', '123.data', 'lj/cut', '5suffix'])
def test_non_numeric_words(value):
    assert (Name, value) in tokens('read_data ' + value)


def test_substitutions():
    result = tokens('read_data data.${sample}.$xx\nregion box block $((xlo+xhi)/2) 3\n')
    assert (Name, 'x') in result
    formula = ''.join(value for token, value in result if token is Name.Variable)
    assert formula == '${sample}$x$((xlo+xhi)/2)'
    assert (Number.Integer, '3') in result


def test_immediate_format_and_parentheses():
    result = tokens('variable x equal $(sqrt((v_x+1)*2):%10.3f)\nrun 4\n')
    assert ''.join(value for token, value in result if token is Name.Variable) == (
        '$(sqrt((v_x+1)*2):%10.3f)'
    )
    assert (Keyword, 'run') in result


def test_command_continuation():
    result = tokens('fix 1 all nvt temp 1 1 & \n  0.1\nrun 2\n')
    assert [value for token, value in result if token is Keyword] == ['fix', 'run']
    assert (Punctuation, '& \n') in result
    assert (Number.Float, '0.1') in result


def test_comment_continuation():
    result = tokens('# disabled command &\n  run 100 &\n  more arguments\nrun 2\n')
    assert [value for token, value in result if token is Keyword] == ['run']
    assert any('run 100' in value for token, value in result if token in Comment)


def test_comment_after_ampersand_ends_command():
    result = tokens('run 1 & # comment\nrun 2\n')
    assert [value for token, value in result if token is Keyword] == ['run', 'run']


@pytest.mark.parametrize('quote', ["'", '"', '"""'])
def test_quoted_markers(quote):
    result = tokens('print ' + quote + '# ${name} $(v_x+1)' + quote + '\nrun 2\n')
    assert not any(token in Comment or token is Name.Variable for token, _ in result)
    assert [value for token, value in result if token is Keyword] == ['print', 'run']


@pytest.mark.parametrize('quote', ["'", '"'])
def test_continued_string(quote):
    result = tokens('variable x string ' + quote + 'one &\n two' + quote + '\nrun 2\n')
    assert (String.Escape, '&\n') in result
    assert [value for token, value in result if token is Keyword] == ['variable', 'run']


def test_triple_quoted_string():
    result = tokens('print """one &\n# two\n${three}"""\nrun 2\n')
    assert not any(token in Comment or token is Name.Variable or token is String.Escape
                   for token, _ in result)
    assert [value for token, value in result if token is Keyword] == ['print', 'run']


@pytest.mark.parametrize('text', [
    '# last comment',
    'print "unfinished',
    "print 'unfinished",
    'print """unfinished\n# still a string',
    'variable x equal $(sqrt((v_x+1)',
    'run 10 &',
    'read_data ${unfinished',
])
def test_incomplete_input(text):
    tokens(text)


@pytest.mark.parametrize('quote', ["'", '"'])
def test_unclosed_string_recovers_at_newline(quote):
    result = tokens('print ' + quote + 'unfinished\nrun 2\n')
    assert (Keyword, 'run') in result


@pytest.mark.parametrize('ending', ['\n', '\r\n'])
def test_continuation_line_endings(ending):
    text = 'fix 1 all nvt temp 1 1 &' + ending + ' 0.1' + ending + 'run 2'
    result = tokens(text)
    assert [value for token, value in result if token is Keyword] == ['fix', 'run']
    assert (Punctuation, '&' + ending) in result


@pytest.mark.parametrize('prefix', ['$(1', '$((1', '${name'])
def test_comments_precede_variable_expansion(prefix):
    result = tokens('variable x equal ' + prefix + ' # comment)\nrun 2\n')
    assert ''.join(value for token, value in result if token in Comment) == '# comment)'
    assert (Keyword, 'run') in result


def test_command_word_continuation():
    result = tokens('pair_st&\nyle lj/cut 2.5\nrun 2\n')
    assert [value for token, value in result if token is Keyword] == [
        'pair_st', 'yle', 'run',
    ]
    assert (Name, 'lj/cut') in result


@pytest.mark.parametrize('variable, expected', [
    ('${sam&\nple}', '${sample}'),
    ('$&\nx', '$x'),
    ('$&\n{sample}', '${sample}'),
    ('$&\n(sqrt(4))', '$(sqrt(4))'),
])
def test_variable_continuation(variable, expected):
    result = tokens('print ' + variable + '\nrun 2\n')
    assert ''.join(value for token, value in result if token is Name.Variable) == expected
    assert (Keyword, 'run') in result


def test_quotes_protect_markers_inside_immediate_variable():
    result = tokens('print $(strcmp("a#b", "c"))\nrun 2\n')
    assert not any(token in Comment for token, _ in result)
    assert (String.Double, 'a#b') in result
    assert (Keyword, 'run') in result


@pytest.mark.parametrize('quote', ["'", '"'])
def test_broken_immediate_quote_recovers_at_newline(quote):
    result = tokens('print $((1+' + quote + 'unfinished\nrun 2\n')
    assert (Keyword, 'run') in result


def test_escaped_dollar():
    result = tokens(r"print cost:\$x")
    assert not any(token is Name.Variable for token, _ in result)
    assert (String.Escape, r"\$") in result
