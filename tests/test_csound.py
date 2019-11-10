# -*- coding: utf-8 -*-
"""
    Csound lexer tests
    ~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
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
        (Comment.Multiline, u'/*\n * comment\n */'),
        (Text, u'\n'),
        (Comment.Single, u'; comment'),
        (Text, u'\n'),
        (Comment.Single, u'// comment'),
        (Text, u'\n')
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
        (Keyword.Declaration, u'instr'),
        (Comment.Multiline, u'/**/'),
        (Name.Function, u'1'),
        (Punctuation, u','),
        (Comment.Multiline, u'/**/'),
        (Name.Function, u'N_a_M_e_'),
        (Punctuation, u','),
        (Comment.Multiline, u'/**/'),
        (Punctuation, u'+'),
        (Name.Function, u'Name'),
        (Comment.Multiline, u'/**/'),
        (Comment.Single, u'//'),
        (Text, u'\n'),
        (Text, u'  '),
        (Keyword.Type, u'i'),
        (Name, u'Duration'),
        (Text, u' '),
        (Operator, u'='),
        (Text, u' '),
        (Name.Variable.Instance, u'p3'),
        (Text, u'\n'),
        (Text, u'  '),
        (Name.Builtin, u'outc'),
        (Punctuation, u':'),
        (Keyword.Type, u'a'),
        (Punctuation, u'('),
        (Keyword.Type, u'a'),
        (Name, u'Signal'),
        (Punctuation, u')'),
        (Text, u'\n'),
        (Keyword.Declaration, u'endin'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_user_defined_opcodes(lexer):
    fragment = dedent('''\
        opcode/**/aUDO,/**/i[],/**/aik//
          aUDO
        endop
    ''')
    tokens = [
        (Keyword.Declaration, u'opcode'),
        (Comment.Multiline, u'/**/'),
        (Name.Function, u'aUDO'),
        (Punctuation, u','),
        (Comment.Multiline, u'/**/'),
        (Keyword.Type, u'i[]'),
        (Punctuation, u','),
        (Comment.Multiline, u'/**/'),
        (Keyword.Type, u'aik'),
        (Comment.Single, u'//'),
        (Text, u'\n'),
        (Text, u'  '),
        (Name.Function, u'aUDO'),
        (Text, u'\n'),
        (Keyword.Declaration, u'endop'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_numbers(lexer):
    fragment = '123 0123456789'
    tokens = [
        (Number.Integer, u'123'),
        (Text, u' '),
        (Number.Integer, u'0123456789'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
    fragment = '0xabcdef0123456789 0XABCDEF'
    tokens = [
        (Keyword.Type, u'0x'),
        (Number.Hex, u'abcdef0123456789'),
        (Text, u' '),
        (Keyword.Type, u'0X'),
        (Number.Hex, u'ABCDEF'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
    fragments = ['1e2', '3e+4', '5e-6', '7E8', '9E+0', '1E-2', '3.', '4.56', '.789']
    for fragment in fragments:
        tokens = [
            (Number.Float, fragment),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_quoted_strings(lexer):
    fragment = '"characters$MACRO."'
    tokens = [
        (String, u'"'),
        (String, u'characters'),
        (Comment.Preproc, u'$MACRO.'),
        (String, u'"'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_braced_strings(lexer):
    fragment = dedent('''\
        {{
        characters$MACRO.
        }}
    ''')
    tokens = [
        (String, u'{{'),
        (String, u'\ncharacters$MACRO.\n'),
        (String, u'}}'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_escape_sequences(lexer):
    for character in ['\\', 'a', 'b', 'n', 'r', 't', '"', '012', '345', '67']:
        escapedCharacter = '\\' + character
        fragment = '"' + escapedCharacter + '"'
        tokens = [
            (String, u'"'),
            (String.Escape, escapedCharacter),
            (String, u'"'),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens
        fragment = '{{' + escapedCharacter + '}}'
        tokens = [
            (String, u'{{'),
            (String.Escape, escapedCharacter),
            (String, u'}}'),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_operators(lexer):
    fragments = ['+', '-', '~', u'¬', '!', '*', '/', '^', '%', '<<', '>>', '<', '>',
                 '<=', '>=', '==', '!=', '&', '#', '|', '&&', '||', '?', ':', '+=',
                 '-=', '*=', '/=']
    for fragment in fragments:
        tokens = [
            (Operator, fragment),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_global_value_identifiers(lexer):
    for fragment in ['0dbfs', 'A4', 'kr', 'ksmps', 'nchnls', 'nchnls_i', 'sr']:
        tokens = [
            (Name.Variable.Global, fragment),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_keywords(lexer):
    fragments = ['do', 'else', 'elseif', 'endif', 'enduntil', 'fi', 'if', 'ithen',
                 'kthen', 'od', 'then', 'until', 'while']
    for fragment in fragments:
        tokens = [
            (Keyword, fragment),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens
    for fragment in ['return', 'rireturn']:
        tokens = [
            (Keyword.Pseudo, fragment),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_labels(lexer):
    fragment = dedent('''\
        aLabel:
         label2:
    ''')
    tokens = [
        (Name.Label, u'aLabel'),
        (Punctuation, u':'),
        (Text, u'\n'),
        (Text, u' '),
        (Name.Label, u'label2'),
        (Punctuation, u':'),
        (Text, u'\n')
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
                (Text, u' '),
                (String, u'"'),
                (String.Escape, escapedCharacter),
                (String, u'"'),
                (Text, u'\n')
            ]
            assert list(lexer.get_tokens(fragment)) == tokens


def test_goto_statements(lexer):
    for keyword in ['goto', 'igoto', 'kgoto']:
        fragment = keyword + ' aLabel'
        tokens = [
            (Keyword, keyword),
            (Text, u' '),
            (Name.Label, u'aLabel'),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens
    for opcode in ['reinit', 'rigoto', 'tigoto']:
        fragment = opcode + ' aLabel'
        tokens = [
            (Keyword.Pseudo, opcode),
            (Text, u' '),
            (Name.Label, u'aLabel'),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens
    for opcode in ['cggoto', 'cigoto', 'cingoto', 'ckgoto', 'cngoto', 'cnkgoto']:
        fragment = opcode + ' 1==0, aLabel'
        tokens = [
            (Keyword.Pseudo, opcode),
            (Text, u' '),
            (Number.Integer, u'1'),
            (Operator, u'=='),
            (Number.Integer, u'0'),
            (Punctuation, u','),
            (Text, u' '),
            (Name.Label, u'aLabel'),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens
    fragment = 'timout 0, 0, aLabel'
    tokens = [
        (Keyword.Pseudo, 'timout'),
        (Text, u' '),
        (Number.Integer, u'0'),
        (Punctuation, u','),
        (Text, u' '),
        (Number.Integer, u'0'),
        (Punctuation, u','),
        (Text, u' '),
        (Name.Label, u'aLabel'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens
    for opcode in ['loop_ge', 'loop_gt', 'loop_le', 'loop_lt']:
        fragment = opcode + ' 0, 0, 0, aLabel'
        tokens = [
            (Keyword.Pseudo, opcode),
            (Text, u' '),
            (Number.Integer, u'0'),
            (Punctuation, u','),
            (Text, u' '),
            (Number.Integer, u'0'),
            (Punctuation, u','),
            (Text, u' '),
            (Number.Integer, u'0'),
            (Punctuation, u','),
            (Text, u' '),
            (Name.Label, u'aLabel'),
            (Text, u'\n')
        ]
        assert list(lexer.get_tokens(fragment)) == tokens


def test_include_directives(lexer):
    for character in ['"', '|']:
        fragment = '#include/**/' + character + 'file.udo' + character
        tokens = [
            (Comment.Preproc, u'#include'),
            (Comment.Multiline, u'/**/'),
            (String, character + u'file.udo' + character),
            (Text, u'\n')
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
        (Comment.Preproc, u'# \tdefine'),
        (Text, u' '),
        (Comment.Preproc, u'MACRO'),
        (Punctuation, u'#'),
        (Comment.Preproc, u'macro_body'),
        (Punctuation, u'#'),
        (Text, u'\n'),
        (Comment.Preproc, u'#define'),
        (Comment.Multiline, u'/**/'),
        (Text, u'\n'),
        (Comment.Preproc, u'MACRO'),
        (Comment.Multiline, u'/**/'),
        (Text, u'\n'),
        (Punctuation, u'#'),
        (Comment.Preproc, u'\\#'),
        (Comment.Preproc, u'macro\nbody'),
        (Comment.Preproc, u'\\#'),
        (Punctuation, u'#'),
        (Text, u'\n')
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
        (Comment.Preproc, u'#define'),
        (Text, u' '),
        (Comment.Preproc, u'MACRO'),
        (Punctuation, u'('),
        (Comment.Preproc, u'ARG1'),
        (Punctuation, u'#'),
        (Comment.Preproc, u'ARG2'),
        (Punctuation, u')'),
        (Text, u' '),
        (Punctuation, u'#'),
        (Comment.Preproc, u'macro_body'),
        (Punctuation, u'#'),
        (Text, u'\n'),
        (Comment.Preproc, u'#define'),
        (Comment.Multiline, u'/**/'),
        (Text, u'\n'),
        (Comment.Preproc, u'MACRO'),
        (Punctuation, u'('),
        (Comment.Preproc, u'ARG1'),
        (Punctuation, u"'"),
        (Comment.Preproc, u'ARG2'),
        (Punctuation, u"'"),
        (Text, u' '),
        (Comment.Preproc, u'ARG3'),
        (Punctuation, u')'),
        (Comment.Multiline, u'/**/'),
        (Text, u'\n'),
        (Punctuation, u'#'),
        (Comment.Preproc, u'\\#'),
        (Comment.Preproc, u'macro\nbody'),
        (Comment.Preproc, u'\\#'),
        (Punctuation, u'#'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_macro_preprocessor_directives(lexer):
    for directive in ['#ifdef', '#ifndef', '#undef']:
        fragment = directive + ' MACRO'
        tokens = [
            (Comment.Preproc, directive),
            (Text, u' '),
            (Comment.Preproc, u'MACRO'),
            (Text, u'\n')
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
        (Comment.Preproc, u'#else'),
        (Text, u'\n'),
        (Comment.Preproc, u'#end'),
        (Text, u'\n'),
        (Comment.Preproc, u'#endif'),
        (Text, u'\n'),
        (Comment.Preproc, u'###'),
        (Text, u'\n'),
        (Comment.Preproc, u'@ \t12345'),
        (Text, u'\n'),
        (Comment.Preproc, u'@@ \t67890'),
        (Text, u'\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens


def test_function_like_macros(lexer):
    fragment = "$MACRO.(((x#y\\)))' \"(#'x)\\)x\\))\"# {{x\\))x)\\)(#'}});"
    tokens = [
        (Comment.Preproc, u'$MACRO.'),
        (Punctuation, u'('),
        (Comment.Preproc, u'('),
        (Comment.Preproc, u'('),
        (Comment.Preproc, u'x#y\\)'),
        (Comment.Preproc, u')'),
        (Comment.Preproc, u')'),
        (Punctuation, u"'"),
        (Comment.Preproc, u' '),
        (String, u'"'),
        (Error, u'('),
        (Error, u'#'),
        (Error, u"'"),
        (String, u'x'),
        (Error, u')'),
        (Comment.Preproc, u'\\)'),
        (String, u'x'),
        (Comment.Preproc, u'\\)'),
        (Error, u')'),
        (String, u'"'),
        (Punctuation, u'#'),
        (Comment.Preproc, u' '),
        (String, u'{{'),
        (String, u'x'),
        (Comment.Preproc, u'\\)'),
        (Error, u')'),
        (String, u'x'),
        (Error, u')'),
        (Comment.Preproc, u'\\)'),
        (Error, u'('),
        (Error, u'#'),
        (Error, u"'"),
        (String, u'}}'),
        (Punctuation, u')'),
        (Comment.Single, u';'),
        (Text, u'\n')
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
