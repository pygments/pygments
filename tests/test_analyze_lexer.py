# -*- coding: utf-8 -*-
"""
    Pygments basic API tests
    ~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from os import path

import pytest

from pygments import lexers

TESTDIR = path.dirname(path.abspath(__file__))


def _guess_lexer_for_file(filename):
    return lexers.guess_lexer(open(path.join(TESTDIR, 'examplefiles', filename),
                                   'r', encoding='utf-8').read())


@pytest.mark.skip(reason="This is identified as T-SQL")
def test_guess_lexer_fsharp():
    lx = _guess_lexer_for_file('Deflate.rs')
    assert lx.__class__.__name__ == 'FSharpLexer'


def test_guess_lexer_brainfuck():
    lx = lexers.guess_lexer(">>[-]<<[->>+<<]")
    assert lx.__class__.__name__ == 'BrainfuckLexer'


def test_guess_lexer_singularity():
    lx = _guess_lexer_for_file('Singularity')
    assert lx.__class__.__name__ == 'SingularityLexer'


@pytest.mark.skip(reason="This is identified as MIME")
def test_guess_lexer_matlab():
    lx = lexers.guess_lexer(r'A \ B')
    assert lx.__class__.__name__ == 'OctaveLexer'


@pytest.mark.skip(reason="This is identified as Python")
def test_guess_lexer_hybris():
    lx = _guess_lexer_for_file('hybris_File.hy')
    assert lx.__class__.__name__ == 'HybrisLexer'


def test_guess_lexer_forth():
    lx = _guess_lexer_for_file('demo.frt')
    assert lx.__class__.__name__ == 'ForthLexer'


def test_guess_lexer_modula2():
    lx = _guess_lexer_for_file('modula2_test_cases.def')
    assert lx.__class__.__name__ == 'Modula2Lexer'


def test_guess_lexer_unicon():
    lx = _guess_lexer_for_file('example.icn')
    assert lx.__class__.__name__ == 'UcodeLexer'


def test_guess_lexer_ezhil():
    lx = _guess_lexer_for_file('ezhil_primefactors.n')
    assert lx.__class__.__name__ == 'EzhilLexer'
