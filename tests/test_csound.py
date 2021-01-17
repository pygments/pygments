# -*- coding: utf-8 -*-
"""
    Csound lexer tests
    ~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from textwrap import dedent

import pytest

from pygments.token import Comment, Error, Keyword, Name, Number, Operator, \
    Punctuation, String, Text
from pygments.lexers import CsoundOrchestraLexer


@pytest.fixture(scope='module')
def lexer():
    yield CsoundOrchestraLexer()


def test_comments(lexer):
    fragment = dedent('''\
        /*
         * comment
         */
        ; comment
        // comment
    ''')
    tokens = [
        (Comment.Multiline, '/*\n * comment\n */'),
        (Text, '\n'),
        (Comment.Single, '; comment'),
        (Text, '\n'),
        (Comment.Single, '// comment'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_instrument_blocks(lexer):
    fragment = dedent('''\
        instr/**/1,/**/N_a_M_e_,/**/+Name/**///
          iDuration = p3
          outc:a(aSignal)
        endin
    ''')
    tokens = [
        (Keyword.Declaration, 'instr'),
        (Comment.Multiline, '/**/'),
        (Name.Function, '1'),
        (Punctuation, ','),
        (Comment.Multiline, '/**/'),
        (Name.Function, 'N_a_M_e_'),
        (Punctuation, ','),
        (Comment.Multiline, '/**/'),
        (Punctuation, '+'),
        (Name.Function, 'Name'),
        (Comment.Multiline, '/**/'),
        (Comment.Single, '//'),
        (Text, '\n'),
        (Text, '  '),
        (Keyword.Type, 'i'),
        (Name, 'Duration'),
        (Text, ' '),
        (Operator, '='),
        (Text, ' '),
        (Name.Variable.Instance, 'p3'),
        (Text, '\n'),
        (Text, '  '),
        (Name.Builtin, 'outc'),
        (Punctuation, ':'),
        (Keyword.Type, 'a'),
        (Punctuation, '('),
        (Keyword.Type, 'a'),
        (Name, 'Signal'),
        (Punctuation, ')'),
        (Text, '\n'),
        (Keyword.Declaration, 'endin'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_user_defined_opcodes(lexer):
    fragment = dedent('''\
        opcode/**/aUDO,/**/i[],/**/aik//
          aUDO
        endop
    ''')
    tokens = [
        (Keyword.Declaration, 'opcode'),
        (Comment.Multiline, '/**/'),
        (Name.Function, 'aUDO'),
        (Punctuation, ','),
        (Comment.Multiline, '/**/'),
        (Keyword.Type, 'i[]'),
        (Punctuation, ','),
        (Comment.Multiline, '/**/'),
        (Keyword.Type, 'aik'),
        (Comment.Single, '//'),
        (Text, '\n'),
        (Text, '  '),
        (Name.Function, 'aUDO'),
        (Text, '\n'),
        (Keyword.Declaration, 'endop'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_numbers(lexer):
    fragment = '123 0123456789'
    tokens = [
        (Number.Integer, '123'),
        (Text, ' '),
        (Number.Integer, '0123456789'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
    fragment = '0xabcdef0123456789 0XABCDEF'
    tokens = [
        (Keyword.Type, '0x'),
        (Number.Hex, 'abcdef0123456789'),
        (Text, ' '),
        (Keyword.Type, '0X'),
        (Number.Hex, 'ABCDEF'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
    fragments = ['1e2', '3e+4', '5e-6', '7E8', '9E+0', '1E-2', '3.', '4.56', '.789']
    for fragment in fragments:
        tokens = [
            (Number.Float, fragment),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_quoted_strings(lexer):
    fragment = '"characters$MACRO."'
    tokens = [
        (String, '"'),
        (String, 'characters'),
        (Comment.Preproc, '$MACRO.'),
        (String, '"'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_braced_strings(lexer):
    fragment = dedent('''\
        {{
        characters$MACRO.
        }}
    ''')
    tokens = [
        (String, '{{'),
        (String, '\ncharacters$MACRO.\n'),
        (String, '}}'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_escape_sequences(lexer):
    for character in ['\\', 'a', 'b', 'n', 'r', 't', '"', '012', '345', '67']:
        escapedCharacter = '\\' + character
        fragment = '"' + escapedCharacter + '"'
        tokens = [
            (String, '"'),
            (String.Escape, escapedCharacter),
            (String, '"'),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens
        fragment = '{{' + escapedCharacter + '}}'
        tokens = [
            (String, '{{'),
            (String.Escape, escapedCharacter),
            (String, '}}'),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_operators(lexer):
    fragments = ['+', '-', '~', '¬', '!', '*', '/', '^', '%', '<<', '>>', '<', '>',
                 '<=', '>=', '==', '!=', '&', '#', '|', '&&', '||', '?', ':', '+=',
                 '-=', '*=', '/=']
    for fragment in fragments:
        tokens = [
            (Operator, fragment),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_global_value_identifiers(lexer):
    for fragment in ['0dbfs', 'A4', 'kr', 'ksmps', 'nchnls', 'nchnls_i', 'sr']:
        tokens = [
            (Name.Variable.Global, fragment),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_keywords(lexer):
    fragments = ['do', 'else', 'elseif', 'endif', 'enduntil', 'fi', 'if', 'ithen',
                 'kthen', 'od', 'then', 'until', 'while']
    for fragment in fragments:
        tokens = [
            (Keyword, fragment),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens
    for fragment in ['return', 'rireturn']:
        tokens = [
            (Keyword.Pseudo, fragment),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_labels(lexer):
    fragment = dedent('''\
        aLabel:
         label2:
    ''')
    tokens = [
        (Name.Label, 'aLabel'),
        (Punctuation, ':'),
        (Text, '\n'),
        (Text, ' '),
        (Name.Label, 'label2'),
        (Punctuation, ':'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_printks_and_prints_escape_sequences(lexer):
    escapedCharacters = ['%!', '%%', '%n', '%N', '%r', '%R', '%t', '%T', '\\\\a',
                         '\\\\A', '\\\\b', '\\\\B', '\\\\n', '\\\\N', '\\\\r',
                         '\\\\R', '\\\\t', '\\\\T']
    for opcode in ['printks', 'prints']:
        for escapedCharacter in escapedCharacters:
            fragment = opcode + ' "' + escapedCharacter + '"'
            tokens = [
                (Name.Builtin, opcode),
                (Text, ' '),
                (String, '"'),
                (String.Escape, escapedCharacter),
                (String, '"'),
                (Text, '\n')
            ]
            assert list(lexer.get_tokens(fragment)) == tokens


def test_goto_statements(lexer):
    for keyword in ['goto', 'igoto', 'kgoto']:
        fragment = keyword + ' aLabel'
        tokens = [
            (Keyword, keyword),
            (Text, ' '),
            (Name.Label, 'aLabel'),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens
    for opcode in ['reinit', 'rigoto', 'tigoto']:
        fragment = opcode + ' aLabel'
        tokens = [
            (Keyword.Pseudo, opcode),
            (Text, ' '),
            (Name.Label, 'aLabel'),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens
    for opcode in ['cggoto', 'cigoto', 'cingoto', 'ckgoto', 'cngoto', 'cnkgoto']:
        fragment = opcode + ' 1==0, aLabel'
        tokens = [
            (Keyword.Pseudo, opcode),
            (Text, ' '),
            (Number.Integer, '1'),
            (Operator, '=='),
            (Number.Integer, '0'),
            (Punctuation, ','),
            (Text, ' '),
            (Name.Label, 'aLabel'),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens
    fragment = 'timout 0, 0, aLabel'
    tokens = [
        (Keyword.Pseudo, 'timout'),
        (Text, ' '),
        (Number.Integer, '0'),
        (Punctuation, ','),
        (Text, ' '),
        (Number.Integer, '0'),
        (Punctuation, ','),
        (Text, ' '),
        (Name.Label, 'aLabel'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
    for opcode in ['loop_ge', 'loop_gt', 'loop_le', 'loop_lt']:
        fragment = opcode + ' 0, 0, 0, aLabel'
        tokens = [
            (Keyword.Pseudo, opcode),
            (Text, ' '),
            (Number.Integer, '0'),
            (Punctuation, ','),
            (Text, ' '),
            (Number.Integer, '0'),
            (Punctuation, ','),
            (Text, ' '),
            (Number.Integer, '0'),
            (Punctuation, ','),
            (Text, ' '),
            (Name.Label, 'aLabel'),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_include_directives(lexer):
    for character in ['"', '|']:
        fragment = '#include/**/' + character + 'file.udo' + character
        tokens = [
            (Comment.Preproc, '#include'),
            (Comment.Multiline, '/**/'),
            (String, character + 'file.udo' + character),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_includestr_directives(lexer):
    fragment = '#includestr/**/"$MACRO..udo"'
    tokens = [
        (Comment.Preproc, '#includestr'),
        (Comment.Multiline, '/**/'),
        (String, '"'),
        (Comment.Preproc, '$MACRO.'),
        (String, '.udo'),
        (String, '"'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_object_like_macro_definitions(lexer):
    fragment = dedent('''\
        # \tdefine MACRO#macro_body#
        #define/**/
        MACRO/**/
        #\\#macro
        body\\##
    ''')
    tokens = [
        (Comment.Preproc, '# \tdefine'),
        (Text, ' '),
        (Comment.Preproc, 'MACRO'),
        (Punctuation, '#'),
        (Comment.Preproc, 'macro_body'),
        (Punctuation, '#'),
        (Text, '\n'),
        (Comment.Preproc, '#define'),
        (Comment.Multiline, '/**/'),
        (Text, '\n'),
        (Comment.Preproc, 'MACRO'),
        (Comment.Multiline, '/**/'),
        (Text, '\n'),
        (Punctuation, '#'),
        (Comment.Preproc, '\\#'),
        (Comment.Preproc, 'macro\nbody'),
        (Comment.Preproc, '\\#'),
        (Punctuation, '#'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function_like_macro_definitions(lexer):
    fragment = dedent('''\
        #define MACRO(ARG1#ARG2) #macro_body#
        #define/**/
        MACRO(ARG1'ARG2' ARG3)/**/
        #\\#macro
        body\\##
    ''')
    tokens = [
        (Comment.Preproc, '#define'),
        (Text, ' '),
        (Comment.Preproc, 'MACRO'),
        (Punctuation, '('),
        (Comment.Preproc, 'ARG1'),
        (Punctuation, '#'),
        (Comment.Preproc, 'ARG2'),
        (Punctuation, ')'),
        (Text, ' '),
        (Punctuation, '#'),
        (Comment.Preproc, 'macro_body'),
        (Punctuation, '#'),
        (Text, '\n'),
        (Comment.Preproc, '#define'),
        (Comment.Multiline, '/**/'),
        (Text, '\n'),
        (Comment.Preproc, 'MACRO'),
        (Punctuation, '('),
        (Comment.Preproc, 'ARG1'),
        (Punctuation, "'"),
        (Comment.Preproc, 'ARG2'),
        (Punctuation, "'"),
        (Text, ' '),
        (Comment.Preproc, 'ARG3'),
        (Punctuation, ')'),
        (Comment.Multiline, '/**/'),
        (Text, '\n'),
        (Punctuation, '#'),
        (Comment.Preproc, '\\#'),
        (Comment.Preproc, 'macro\nbody'),
        (Comment.Preproc, '\\#'),
        (Punctuation, '#'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_macro_preprocessor_directives(lexer):
    for directive in ['#ifdef', '#ifndef', '#undef']:
        fragment = directive + ' MACRO'
        tokens = [
            (Comment.Preproc, directive),
            (Text, ' '),
            (Comment.Preproc, 'MACRO'),
            (Text, '\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_other_preprocessor_directives(lexer):
    fragment = dedent('''\
        #else
        #end
        #endif
        ###
        @ \t12345
        @@ \t67890
    ''')
    tokens = [
        (Comment.Preproc, '#else'),
        (Text, '\n'),
        (Comment.Preproc, '#end'),
        (Text, '\n'),
        (Comment.Preproc, '#endif'),
        (Text, '\n'),
        (Comment.Preproc, '###'),
        (Text, '\n'),
        (Comment.Preproc, '@ \t12345'),
        (Text, '\n'),
        (Comment.Preproc, '@@ \t67890'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function_like_macros(lexer):
    fragment = "$MACRO.(((x#y\\)))' \"(#'x)\\)x\\))\"# {{x\\))x)\\)(#'}});"
    tokens = [
        (Comment.Preproc, '$MACRO.'),
        (Punctuation, '('),
        (Comment.Preproc, '('),
        (Comment.Preproc, '('),
        (Comment.Preproc, 'x#y\\)'),
        (Comment.Preproc, ')'),
        (Comment.Preproc, ')'),
        (Punctuation, "'"),
        (Comment.Preproc, ' '),
        (String, '"'),
        (Error, '('),
        (Error, '#'),
        (Error, "'"),
        (String, 'x'),
        (Error, ')'),
        (Comment.Preproc, '\\)'),
        (String, 'x'),
        (Comment.Preproc, '\\)'),
        (Error, ')'),
        (String, '"'),
        (Punctuation, '#'),
        (Comment.Preproc, ' '),
        (String, '{{'),
        (String, 'x'),
        (Comment.Preproc, '\\)'),
        (Error, ')'),
        (String, 'x'),
        (Error, ')'),
        (Comment.Preproc, '\\)'),
        (Error, '('),
        (Error, '#'),
        (Error, "'"),
        (String, '}}'),
        (Punctuation, ')'),
        (Comment.Single, ';'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_name(lexer):
    fragment = 'kG:V'
    tokens = [
        (Keyword.Type, 'k'),
        (Name, 'G'),
        (Punctuation, ':'),
        (Name, 'V'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
