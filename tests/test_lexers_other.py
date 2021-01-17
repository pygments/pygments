# -*- coding: utf-8 -*-
"""
    Tests for other lexers
    ~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import glob
import os

import pytest

from pygments.lexers import (
    EasytrieveLexer, GAPLexer, GDScriptLexer, JclLexer, RexxLexer, guess_lexer
)


def _example_file_path(filename):
    return os.path.join(os.path.dirname(__file__), 'examplefiles', filename)


@pytest.mark.parametrize('lexer', [
    EasytrieveLexer,
    JclLexer,
    RexxLexer,
])
def test_can_recognize_and_guess_example_files(lexer):
    for pattern in lexer.filenames:
        exampleFilesPattern = _example_file_path(pattern)
        for exampleFilePath in glob.glob(exampleFilesPattern):
            with open(exampleFilePath, 'rb') as fp:
                text = fp.read().decode('utf-8')
            probability = lexer.analyse_text(text)
            assert probability > 0, '%s must recognize %r' % (
                lexer.name, exampleFilePath)
            guessedLexer = guess_lexer(text)
            assert guessedLexer.name == lexer.name


def test_easytrieve_can_guess_from_text():
    assert EasytrieveLexer.analyse_text('MACRO')
    assert EasytrieveLexer.analyse_text('\nMACRO')
    assert EasytrieveLexer.analyse_text(' \nMACRO')
    assert EasytrieveLexer.analyse_text(' \n MACRO')
    assert EasytrieveLexer.analyse_text('*\nMACRO')
    assert EasytrieveLexer.analyse_text('*\n *\n\n \n*\n MACRO')


def test_rexx_can_guess_from_text():
    assert RexxLexer.analyse_text('/* */') == pytest.approx(0.01)
    assert RexxLexer.analyse_text('''/* Rexx */
            say "hello world"''') == pytest.approx(1.0)
    val = RexxLexer.analyse_text('/* */\n'
                                 'hello:pRoceduRe\n'
                                 '  say "hello world"')
    assert val > 0.5
    val = RexxLexer.analyse_text('''/* */
            if 1 > 0 then do
                say "ok"
            end
            else do
                say "huh?"
            end''')
    assert val > 0.2
    val = RexxLexer.analyse_text('''/* */
            greeting = "hello world!"
            parse value greeting "hello" name "!"
            say name''')
    assert val > 0.2


@pytest.mark.parametrize("file_path, lexer", [
    ("gdscript_example.gd", GDScriptLexer),
    ("example.gd", GAPLexer),
])
def test_chooses_correct_lexer_for_example_files(file_path, lexer):
    with open(_example_file_path(file_path), "rb") as fp:
        text = fp.read().decode("utf-8")
    guessed_lexer = guess_lexer(text)
    assert guessed_lexer.name == lexer.name
