# -*- coding: utf-8 -*-
"""
    Pygments basic API tests
    ~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2020 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import random
from io import StringIO, BytesIO
from os import path

import pytest

from pygments import lexers, formatters, lex, format

TESTDIR = path.dirname(path.abspath(__file__))

def _guess_lexer_for_file(filename):
    return lexers.guess_lexer(open(path.join(TESTDIR, 'examplefiles', filename), 'r', encoding='utf-8').read())

@pytest.mark.skip(reason="This is identified as T-SQL")
def test_guess_lexer_fsharp():
    l = _guess_lexer_for_file('Deflate.rs')
    assert l.__class__.__name__ == 'FSharpLexer'

def test_guess_lexer_brainfuck():
    l = lexers.guess_lexer(">>[-]<<[->>+<<]")
    assert l.__class__.__name__ == 'BrainfuckLexer'

def test_guess_lexer_singularity():
    l = _guess_lexer_for_file('Singularity')
    assert l.__class__.__name__ == 'SingularityLexer'

@pytest.mark.skip(reason="This is identified as MIME")
def test_guess_lexer_matlab():
    l = lexers.guess_lexer(r'A \ B')
    assert l.__class__.__name__ == 'OctaveLexer'

@pytest.mark.skip(reason="This is identified as Python")
def test_guess_lexer_hybris():
    l = _guess_lexer_for_file('hybris_File.hy')
    assert l.__class__.__name__ == 'HybrisLexer'

def test_guess_lexer_forth():
    l = _guess_lexer_for_file('demo.frt')
    assert l.__class__.__name__ == 'ForthLexer'

def test_guess_lexer_modula2():
    l = _guess_lexer_for_file('modula2_test_cases.def')
    assert l.__class__.__name__ == 'Modula2Lexer'

def test_guess_lexer_unicon():
    l = _guess_lexer_for_file('example.icn')
    assert l.__class__.__name__ == 'UcodeLexer'

def test_guess_lexer_ezhil():
    l = _guess_lexer_for_file('ezhil_primefactors.n')
    assert l.__class__.__name__ == 'EzhilLexer'