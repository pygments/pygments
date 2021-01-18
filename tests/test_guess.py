"""
    Pygments basic API tests
    ~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from os import path

import pytest

from pygments.lexers import guess_lexer, get_lexer_by_name

TESTDIR = path.dirname(path.abspath(__file__))


def get_input(*fn):
    content = open(path.join(TESTDIR, 'lexers', *fn), encoding='utf-8').read()
    content, _, expected = content.partition('\n---tokens---\n')
    if content.startswith('---input---\n'):
        content = '\n' + content
    _, _, text = content.rpartition('\n---input---\n')
    if not text.endswith('\n'):
        text += '\n'
    return text


@pytest.mark.skip(reason="This is identified as T-SQL")
def test_guess_lexer_fsharp():
    lx = guess_lexer(get_input('fsharp', 'example.txt'))
    assert lx.__class__.__name__ == 'FSharpLexer'


def test_guess_lexer_brainfuck():
    lx = guess_lexer('>>[-]<<[->>+<<]')
    assert lx.__class__.__name__ == 'BrainfuckLexer'


def test_guess_lexer_singularity():
    lx = guess_lexer(get_input('singularity', 'example.txt'))
    assert lx.__class__.__name__ == 'SingularityLexer'


@pytest.mark.skip(reason="This is identified as MIME")
def test_guess_lexer_matlab():
    lx = lexers.guess_lexer(r'A \ B')
    assert lx.__class__.__name__ == 'OctaveLexer'


@pytest.mark.skip(reason="This is identified as Python")
def test_guess_lexer_hybris():
    lx = guess_lexer(get_input('hybris', 'example.txt'))
    assert lx.__class__.__name__ == 'HybrisLexer'


def test_guess_lexer_forth():
    lx = guess_lexer(get_input('forth', 'example.txt'))
    assert lx.__class__.__name__ == 'ForthLexer'


def test_guess_lexer_modula2():
    lx = guess_lexer(get_input('modula2', 'example2.txt'))
    assert lx.__class__.__name__ == 'Modula2Lexer'


def test_guess_lexer_unicon():
    lx = guess_lexer(get_input('unicon', 'example.txt'))
    assert lx.__class__.__name__ == 'UcodeLexer'


def test_guess_lexer_ezhil():
    lx = guess_lexer(get_input('ezhil', 'example.txt'))
    assert lx.__class__.__name__ == 'EzhilLexer'


def test_guess_lexer_gdscript():
    lx = guess_lexer(get_input('gdscript', 'example.txt'))
    assert lx.__class__.__name__ == 'GDScriptLexer'


def test_guess_lexer_gap():
    lx = guess_lexer(get_input('gap', 'example.txt'))
    assert lx.__class__.__name__ == 'GAPLexer'


def test_guess_lexer_easytrieve():
    lx = guess_lexer(get_input('easytrieve', 'example.txt'))
    assert lx.__class__.__name__ == 'EasytrieveLexer'
    lx = guess_lexer(get_input('easytrieve', 'example2.txt'))
    assert lx.__class__.__name__ == 'EasytrieveLexer'


def test_guess_lexer_jcl():
    lx = guess_lexer(get_input('jcl', 'example.txt'))
    assert lx.__class__.__name__ == 'JclLexer'


def test_guess_lexer_rexx():
    lx = guess_lexer(get_input('rexx', 'example.txt'))
    assert lx.__class__.__name__ == 'RexxLexer'


def test_easytrieve_can_guess_from_text():
    lx = get_lexer_by_name('easytrieve')
    assert lx.analyse_text('MACRO')
    assert lx.analyse_text('\nMACRO')
    assert lx.analyse_text(' \nMACRO')
    assert lx.analyse_text(' \n MACRO')
    assert lx.analyse_text('*\nMACRO')
    assert lx.analyse_text('*\n *\n\n \n*\n MACRO')


def test_rexx_can_guess_from_text():
    lx = get_lexer_by_name('rexx')
    assert lx.analyse_text('/* */') == pytest.approx(0.01)
    assert lx.analyse_text('''/* Rexx */
            say "hello world"''') == pytest.approx(1.0)
    val = lx.analyse_text('/* */\n'
                          'hello:pRoceduRe\n'
                          '  say "hello world"')
    assert val > 0.5
    val = lx.analyse_text('''/* */
            if 1 > 0 then do
                say "ok"
            end
            else do
                say "huh?"
            end''')
    assert val > 0.2
    val = lx.analyse_text('''/* */
            greeting = "hello world!"
            parse value greeting "hello" name "!"
            say name''')
    assert val > 0.2


def test_guess_cmake_lexer_from_header():
    headers = [
        "CMAKE_MINIMUM_REQUIRED(VERSION 2.6 FATAL_ERROR)",
        "cmake_minimum_required(version 3.13)  # CMake version check",
        " CMAKE_MINIMUM_REQUIRED\t( VERSION 2.6 FATAL_ERROR ) ",
    ]
    for header in headers:
        code = '\n'.join([
            header,
            'project(example)',
            'set(CMAKE_CXX_STANDARD 14)',
            'set(SOURCE_FILES main.cpp)',
            'add_executable(example ${SOURCE_FILES})',
        ])
        lexer = guess_lexer(code)
        assert lexer.__class__.__name__ == 'CMakeLexer', \
            "header must be detected as CMake: %r" % header


def test_guess_c_lexer():
    code = '''
    #include <stdio.h>
    #include <stdlib.h>

    int main(void);

    int main(void) {
        uint8_t x = 42;
        uint8_t y = x + 1;

        /* exit 1 for success! */
        return 1;
    }
    '''
    lexer = guess_lexer(code)
    assert lexer.__class__.__name__ == 'CLexer'
