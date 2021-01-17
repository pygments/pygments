# -*- coding: utf-8 -*-
"""
    Python Tests
    ~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexers import PythonLexer, Python3Lexer
from pygments.token import Token

import re


@pytest.fixture(scope='module')
def lexer2():
    yield PythonLexer()


@pytest.fixture(scope='module')
def lexer3():
    yield Python3Lexer()


def test_cls_builtin(lexer2):
    """
    Tests that a cls token gets interpreted as a Token.Name.Builtin.Pseudo
    """
    fragment = 'class TestClass():\n    @classmethod\n    def hello(cls):\n        pass\n'
    tokens = [
        (Token.Keyword, 'class'),
        (Token.Text, ' '),
        (Token.Name.Class, 'TestClass'),
        (Token.Punctuation, '('),
        (Token.Punctuation, ')'),
        (Token.Punctuation, ':'),
        (Token.Text, '\n'),
        (Token.Text, '    '),
        (Token.Name.Decorator, '@classmethod'),
        (Token.Text, '\n'),
        (Token.Text, '    '),
        (Token.Keyword, 'def'),
        (Token.Text, ' '),
        (Token.Name.Function, 'hello'),
        (Token.Punctuation, '('),
        (Token.Name.Builtin.Pseudo, 'cls'),
        (Token.Punctuation, ')'),
        (Token.Punctuation, ':'),
        (Token.Text, '\n'),
        (Token.Text, '        '),
        (Token.Keyword, 'pass'),
        (Token.Text, '\n'),
    ]
    assert list(lexer2.get_tokens(fragment)) == tokens


def test_needs_name(lexer3):
    """
    Tests that '@' is recognized as an Operator
    """
    fragment = 'S = (H @ beta - r).T @ inv(H @ V @ H.T) @ (H @ beta - r)\n'
    tokens = [
        (Token.Name, 'S'),
        (Token.Text, ' '),
        (Token.Operator, '='),
        (Token.Text, ' '),
        (Token.Punctuation, '('),
        (Token.Name, 'H'),
        (Token.Text, ' '),
        (Token.Operator, '@'),
        (Token.Text, ' '),
        (Token.Name, 'beta'),
        (Token.Text, ' '),
        (Token.Operator, '-'),
        (Token.Text, ' '),
        (Token.Name, 'r'),
        (Token.Punctuation, ')'),
        (Token.Operator, '.'),
        (Token.Name, 'T'),
        (Token.Text, ' '),
        (Token.Operator, '@'),
        (Token.Text, ' '),
        (Token.Name, 'inv'),
        (Token.Punctuation, '('),
        (Token.Name, 'H'),
        (Token.Text, ' '),
        (Token.Operator, '@'),
        (Token.Text, ' '),
        (Token.Name, 'V'),
        (Token.Text, ' '),
        (Token.Operator, '@'),
        (Token.Text, ' '),
        (Token.Name, 'H'),
        (Token.Operator, '.'),
        (Token.Name, 'T'),
        (Token.Punctuation, ')'),
        (Token.Text, ' '),
        (Token.Operator, '@'),
        (Token.Text, ' '),
        (Token.Punctuation, '('),
        (Token.Name, 'H'),
        (Token.Text, ' '),
        (Token.Operator, '@'),
        (Token.Text, ' '),
        (Token.Name, 'beta'),
        (Token.Text, ' '),
        (Token.Operator, '-'),
        (Token.Text, ' '),
        (Token.Name, 'r'),
        (Token.Punctuation, ')'),
        (Token.Text, '\n'),
    ]
    assert list(lexer3.get_tokens(fragment)) == tokens


def test_pep_515(lexer3):
    """
    Tests that the lexer can parse numeric literals with underscores
    """
    fragments = (
        (Token.Literal.Number.Integer, '1_000_000'),
        (Token.Literal.Number.Float, '1_000.000_001'),
        (Token.Literal.Number.Float, '1_000e1_000j'),
        (Token.Literal.Number.Hex, '0xCAFE_F00D'),
        (Token.Literal.Number.Bin, '0b_0011_1111_0100_1110'),
        (Token.Literal.Number.Oct, '0o_777_123'),
    )

    for token, fragment in fragments:
        tokens = [
            (token, fragment),
            (Token.Text, '\n'),
        ]
        assert list(lexer3.get_tokens(fragment)) == tokens


def test_walrus_operator(lexer3):
    """
    Tests that ':=' is recognized as an Operator
    """
    fragment = 'if (a := 2) > 4:'
    tokens = [
        (Token.Keyword, 'if'),
        (Token.Text, ' '),
        (Token.Punctuation, '('),
        (Token.Name, 'a'),
        (Token.Text, ' '),
        (Token.Operator, ':='),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '2'),
        (Token.Punctuation, ')'),
        (Token.Text, ' '),
        (Token.Operator, '>'),
        (Token.Text, ' '),
        (Token.Literal.Number.Integer, '4'),
        (Token.Punctuation, ':'),
        (Token.Text, '\n'),
    ]
    assert list(lexer3.get_tokens(fragment)) == tokens


def test_fstring(lexer3):
    """
    Tests that the lexer can parse f-strings
    """
    fragments_and_tokens = (
        # examples from PEP-0498
        (
            "f'My name is {name}, my age next year is {age+1}, my anniversary is {anniversary:%A, %B %d, %Y}.'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'My name is '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'name'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, ', my age next year is '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'age'),
                (Token.Operator, '+'),
                (Token.Literal.Number.Integer, '1'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, ', my anniversary is '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'anniversary'),
                (Token.Literal.String.Interpol, ':'),
                (Token.Literal.String.Single, '%A, %B %d, %Y'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, '.'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'He said his name is {name!r}.'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'He said his name is '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'name'),
                (Token.Literal.String.Interpol, '!r}'),
                (Token.Literal.String.Single, '.'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]

        ), (
            "f'input={value:#06x}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'input='),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'value'),
                (Token.Literal.String.Interpol, ':'),
                (Token.Literal.String.Single, '#06x'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            """f'{"quoted string"}'\n""",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Double, 'quoted string'),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            """f'{f"{inner}"}'\n""",  # not in the PEP
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'inner'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            # SyntaxError: f-string expression part cannot include a backslash
            "f'{\\'quoted string\\'}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Error, '\\'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'quoted string'),
                (Token.Literal.String.Escape, "\\'"),
                (Token.Literal.String.Single, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'{{ {4*10} }}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Escape, '{{'),
                (Token.Literal.String.Single, ' '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Literal.Number.Integer, '4'),
                (Token.Operator, '*'),
                (Token.Literal.Number.Integer, '10'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, ' '),
                (Token.Literal.String.Escape, '}}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'{{{4*10}}}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Escape, '{{'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Literal.Number.Integer, '4'),
                (Token.Operator, '*'),
                (Token.Literal.Number.Integer, '10'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Escape, '}}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "fr'x={4*10}'\n",
            [
                (Token.Literal.String.Affix, 'fr'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, "x="),
                (Token.Literal.String.Interpol, '{'),
                (Token.Literal.Number.Integer, '4'),
                (Token.Operator, '*'),
                (Token.Literal.Number.Integer, '10'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            """f'abc {a["x"]} def'\n""",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'abc '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'a'),
                (Token.Punctuation, '['),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Double, 'x'),
                (Token.Literal.String.Double, '"'),
                (Token.Punctuation, ']'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, ' def'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'''abc {a['x']} def'''\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'''"),
                (Token.Literal.String.Single, 'abc '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'a'),
                (Token.Punctuation, '['),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'x'),
                (Token.Literal.String.Single, "'"),
                (Token.Punctuation, ']'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, ' def'),
                (Token.Literal.String.Single, "'''"),
                (Token.Text, '\n')
            ]
        ), (
            """f'''{x
+1}'''\n""",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'''"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'x'),
                (Token.Text, '\n'),
                (Token.Operator, '+'),
                (Token.Literal.Number.Integer, '1'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'''"),
                (Token.Text, '\n')
            ]
        ), (
            """f'''{d[0
]}'''\n""",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'''"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'd'),
                (Token.Punctuation, '['),
                (Token.Literal.Number.Integer, '0'),
                (Token.Text, '\n'),
                (Token.Punctuation, ']'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'''"),
                (Token.Text, '\n')
            ]
        ), (
            "f'result: {value:{width}.{precision}}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'result: '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'value'),
                (Token.Literal.String.Interpol, ':'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'width'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, '.'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'precision'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "'a' 'b' f'{x}' '{c}' f'str<{y:^4}>' 'd' 'e'\n",
            [
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'a'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, ' '),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'b'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, ' '),
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'x'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, ' '),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{c}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, ' '),
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'str<'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'y'),
                (Token.Literal.String.Interpol, ':'),
                (Token.Literal.String.Single, '^4'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, '>'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, ' '),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'd'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, ' '),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'e'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'{i}:{d[i]}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'i'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, ':'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'd'),
                (Token.Punctuation, '['),
                (Token.Name, 'i'),
                (Token.Punctuation, ']'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'x = {x:+3}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, "x = "),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'x'),
                (Token.Literal.String.Interpol, ':'),
                (Token.Literal.String.Single, '+3'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'{fn(lst,2)} {fn(lst,3)}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'fn'),
                (Token.Punctuation, '('),
                (Token.Name, 'lst'),
                (Token.Punctuation, ','),
                (Token.Literal.Number.Integer, '2'),
                (Token.Punctuation, ')'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, ' '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'fn'),
                (Token.Punctuation, '('),
                (Token.Name, 'lst'),
                (Token.Punctuation, ','),
                (Token.Literal.Number.Integer, '3'),
                (Token.Punctuation, ')'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'mapping is { {a:b for (a, b) in ((1, 2), (3, 4))} }'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'mapping is '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Text, ' '),
                (Token.Punctuation, '{'),
                (Token.Name, 'a'),
                (Token.Punctuation, ':'),
                (Token.Name, 'b'),
                (Token.Text, ' '),
                (Token.Keyword, 'for'),
                (Token.Text, ' '),
                (Token.Punctuation, '('),
                (Token.Name, 'a'),
                (Token.Punctuation, ','),
                (Token.Text, ' '),
                (Token.Name, 'b'),
                (Token.Punctuation, ')'),
                (Token.Text, ' '),
                (Token.Operator.Word, 'in'),
                (Token.Text, ' '),
                (Token.Punctuation, '('),
                (Token.Punctuation, '('),
                (Token.Literal.Number.Integer, '1'),
                (Token.Punctuation, ','),
                (Token.Text, ' '),
                (Token.Literal.Number.Integer, '2'),
                (Token.Punctuation, ')'),
                (Token.Punctuation, ','),
                (Token.Text, ' '),
                (Token.Punctuation, '('),
                (Token.Literal.Number.Integer, '3'),
                (Token.Punctuation, ','),
                (Token.Text, ' '),
                (Token.Literal.Number.Integer, '4'),
                (Token.Punctuation, ')'),
                (Token.Punctuation, ')'),
                (Token.Punctuation, '}'),
                (Token.Text, ' '),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            """f'a={d["a"]}'\n""",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'a='),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'd'),
                (Token.Punctuation, '['),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Double, 'a'),
                (Token.Literal.String.Double, '"'),
                (Token.Punctuation, ']'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'a={d[a]}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, 'a='),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'd'),
                (Token.Punctuation, '['),
                (Token.Name, 'a'),
                (Token.Punctuation, ']'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "fr'{header}:\\s+'\n",
            [
                (Token.Literal.String.Affix, 'fr'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'header'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, ':'),
                (Token.Literal.String.Single, '\\'),
                (Token.Literal.String.Single, 's+'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'{a!r}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'a'),
                (Token.Literal.String.Interpol, '!r}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'{(lambda x: x*2)(3)}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Punctuation, '('),
                (Token.Keyword, 'lambda'),
                (Token.Text, ' '),
                (Token.Name, 'x'),
                (Token.Punctuation, ':'),
                (Token.Text, ' '),
                (Token.Name, 'x'),
                (Token.Operator, '*'),
                (Token.Literal.Number.Integer, '2'),
                (Token.Punctuation, ')'),
                (Token.Punctuation, '('),
                (Token.Literal.Number.Integer, '3'),
                (Token.Punctuation, ')'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "extra = f'{extra},waiters:{len(self._waiters)}'\n",
            [
                (Token.Name, 'extra'),
                (Token.Text, ' '),
                (Token.Operator, '='),
                (Token.Text, ' '),
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'extra'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, ',waiters:'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name.Builtin, 'len'),
                (Token.Punctuation, '('),
                (Token.Name.Builtin.Pseudo, 'self'),
                (Token.Operator, '.'),
                (Token.Name, '_waiters'),
                (Token.Punctuation, ')'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            'message.append(f" [line {lineno:2d}]")\n',
            [
                (Token.Name, 'message'),
                (Token.Operator, '.'),
                (Token.Name, 'append'),
                (Token.Punctuation, '('),
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Double, ' [line '),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'lineno'),
                (Token.Literal.String.Interpol, ':'),
                (Token.Literal.String.Double, '2d'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Double, ']'),
                (Token.Literal.String.Double, '"'),
                (Token.Punctuation, ')'),
                (Token.Text, '\n')
            ]
        ),
        # Examples from https://bugs.python.org/issue36817
        (
            'f"{foo=}"\n',
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'foo'),
                (Token.Literal.String.Interpol, '=}'),
                (Token.Literal.String.Double, '"'),
                (Token.Text, '\n')
            ]
        ), (
            "f'{foo=!s}'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'foo'),
                (Token.Literal.String.Interpol, '=!s}'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            'f"{math.pi=!f:.2f}"\n',
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'math'),
                (Token.Operator, '.'),
                (Token.Name, 'pi'),
                (Token.Literal.String.Interpol, '=!f:'),
                (Token.Literal.String.Double, '.2f'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Double, '"'),
                (Token.Text, '\n')
            ]
        ), (
            'f"{ chr(65) =}"\n',
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Text, ' '),
                (Token.Name.Builtin, 'chr'),
                (Token.Punctuation, '('),
                (Token.Literal.Number.Integer, '65'),
                (Token.Punctuation, ')'),
                (Token.Text, ' '),
                (Token.Literal.String.Interpol, '=}'),
                (Token.Literal.String.Double, '"'),
                (Token.Text, '\n')
            ]
        ), (
            'f"{chr(65) = }"\n',
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name.Builtin, 'chr'),
                (Token.Punctuation, '('),
                (Token.Literal.Number.Integer, '65'),
                (Token.Punctuation, ')'),
                (Token.Text, ' '),
                (Token.Literal.String.Interpol, '= }'),
                (Token.Literal.String.Double, '"'),
                (Token.Text, '\n')
            ]
        ), (
            "f'*{n=:30}*'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, '*'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'n'),
                (Token.Literal.String.Interpol, '=:'),
                (Token.Literal.String.Single, '30'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, '*'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            "f'*{n=!r:30}*'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, '*'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'n'),
                (Token.Literal.String.Interpol, '=!r:'),
                (Token.Literal.String.Single, '30'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, '*'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            """f"*{f'{n=}':30}*"\n""",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Double, '"'),
                (Token.Literal.String.Double, '*'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'n'),
                (Token.Literal.String.Interpol, '=}'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Interpol, ':'),
                (Token.Literal.String.Double, '30'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Double, '*'),
                (Token.Literal.String.Double, '"'),
                (Token.Text, '\n')
            ]
        ), (
            "f'*{n=:+<30}*'\n",
            [
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'"),
                (Token.Literal.String.Single, '*'),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'n'),
                (Token.Literal.String.Interpol, '=:'),
                (Token.Literal.String.Single, '+<30'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, '*'),
                (Token.Literal.String.Single, "'"),
                (Token.Text, '\n')
            ]
        ), (
            """
   f'''{foo
         = !s:20}'''\n""",
            [
                (Token.Text, '   '),
                (Token.Literal.String.Affix, 'f'),
                (Token.Literal.String.Single, "'''"),
                (Token.Literal.String.Interpol, '{'),
                (Token.Name, 'foo'),
                (Token.Text, '\n         '),
                (Token.Literal.String.Interpol, '= !s:'),
                (Token.Literal.String.Single, '20'),
                (Token.Literal.String.Interpol, '}'),
                (Token.Literal.String.Single, "'''"),
                (Token.Text, '\n')
            ]
        )

    )

    for fragment,tokens in fragments_and_tokens:
        assert list(lexer3.get_tokens(fragment)) == tokens

    # Now switch between single and double quotes, to cover both cases equally
    rep = {"'":'"', '"':"'"}
    pattern = re.compile("|".join(rep.keys()))
    for fragment,tokens in fragments_and_tokens:
        fragment = pattern.sub(lambda m: rep[m.group(0)], fragment)
        tokens = list(tokens)
        for i,(token,match) in enumerate(tokens):
            if token == Token.Literal.String.Single:
                token = Token.Literal.String.Double
            elif token == Token.Literal.String.Double:
                token = Token.Literal.String.Single
            match = pattern.sub(lambda m: rep[m.group(0)], match)
            tokens[i] = (token, match)
        assert list(lexer3.get_tokens(fragment)) == tokens
