# -*- coding: utf-8 -*-
"""
    Basic MumpsLexer Test
    ~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2020 by the Pygments team, see AUTHORS
    :license: BSD, see LICENSE for details
"""

import pytest

from pygments.token import Whitespace, Keyword, Number, Operator, Text, Name, Punctuation, Comment, String
from pygments.lexers import MumpsLexer

@pytest.fixture(scope='module')
def lexer():
    yield MumpsLexer()

def test_quit_line(lexer):
    fragment = ' q'
    tokens = [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_quit_value_line(lexer):
    fragment = ' QUIT 1'
    tokens = [
        (Whitespace, ' '),
        (Keyword, 'QUIT'),
        (Whitespace, ' '),
        (Number, '1'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_quit_morph_line(lexer):
    fragment = ' q:$QUIT 1 Q'
    tokens = [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Operator, ':'),
        (Keyword, '$QUIT'),
        (Whitespace, ' '),
        (Number, '1'),
        (Whitespace, ' '),
        (Keyword, 'Q'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_stub_subroutine(lexer):
    fragment = 'stub q'
    tokens = [
        (Name.Label, 'stub'),
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Text, '\n')
        ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_simple_function(lexer):
    fragment = 'true() q 1'
    tokens = [
        (Name.Function, 'true'),
        (Punctuation, '('),
        (Punctuation, ')'),
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Whitespace, ' '),
        (Number, '1'),
        (Text, '\n')
        ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_arithmetic_unaryops(lexer):
    # 7.1.4.11 - unaryop (just + and -)
    fragment = 'abs(value) ; Returns the absolute value of a value\n'
    fragment+= ' q:value<0 -value\n'
    fragment+= ' q +value'
    tokens = [
        (Name.Function, 'abs'),
	(Punctuation, '('),
	(Name.Variable, 'value'),
	(Punctuation, ')'),
	(Whitespace, ' '),
	(Comment, '; Returns the absolute value of a value'),
	(Text, '\n'),
	(Whitespace, ' '),
	(Keyword, 'q'),
	(Operator, ':'),
	(Name.Variable, 'value'),
	(Operator, '<'),
	(Number, '0'),
	(Whitespace, ' '),
	(Operator, '-'),
	(Name.Variable, 'value'),
	(Text, '\n'),
	(Whitespace, ' '),
	(Keyword, 'q'),
	(Whitespace, ' '),
	(Operator, '+'),
	(Name.Variable, 'value'),
	(Text, '\n'),
	]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_compare(lexer):
    fragment = 'spaceship(left,right) ; Returns -1, 0, or 1 based on whether left is less than, equal to, or greater than the right value\n'
    fragment+= ' q:left>right 1\n'
    fragment+= ' q:left<right -1\n'
    fragment+= ' q 0'
    tokens = [
            (Name.Function, 'spaceship'),
            (Punctuation, '('),
            (Name.Variable, 'left'),
            (Punctuation, ','),
            (Name.Variable, 'right'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Comment, '; Returns -1, 0, or 1 based on whether left is less than, equal to, or greater than the right value'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'left'),
            (Operator, '>'),
            (Name.Variable, 'right'),
            (Whitespace, ' '),
            (Number, '1'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'left'),
            (Operator, '<'),
            (Name.Variable, 'right'),
            (Whitespace, ' '),
            # Per 7.1.4.2 - numlit - numbers do not start with a sign.
            # Therefore, negative numbers use the '-' operator...
            (Operator, '-'),
            # ...followed by a number
            (Number, '1'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Number, '0'),
            (Text, '\n'),
            ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_binaryops(lexer):
    # Operators defined in 7.2.1
    fragment = ' q 1_1+1/2-1*3#10**2\\3'
    tokens = [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Number, '1'),
            (Operator, '_'),
            (Number, '1'),
            (Operator, '+'),
            (Number, '1'),
            (Operator, '/'),
            (Number, '2'),
            (Operator, '-'),
            (Number, '1'),
            (Operator, '*'),
            (Number, '3'),
            (Operator, '#'),
            (Number, '10'),
            (Operator, '**'),
            (Number, '2'),
            (Operator, '\\'),
            (Number, '3'),
            (Text, '\n')
            ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_stringcomp(lexer):
    # Test string comparison operators in 7.2.2.3
    fragment = 'lowersub(a,b) ; Gets the $ORDER-wise lower subscript\n'
    fragment+= ' q:a="" b\n' # Direct equality test against empty string
    fragment+= ' q:b\']"" a\n' # "Does not follow empty string, i.e. is not the empty string
    fragment+= ' q:a]]b b\n'
    fragment+= ' q a'
    tokens = [
            (Name.Function, 'lowersub'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Comment, '; Gets the $ORDER-wise lower subscript'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'a'),
            (Operator, '='),
            (String, '""'),
            (Whitespace, ' '),
            (Name.Variable, 'b'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'b'),
            (Operator, '\''),
            (Operator, ']'),
            (String, '""'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'a'),
            (Operator, ']]'),
            (Name.Variable, 'b'),
            (Whitespace, ' '),
            (Name.Variable, 'b'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Text, '\n'),
            ]
    assert list(lexer.get_tokens(fragment)) == tokens
    
def test_string_contains(lexer):
    fragment = ' q "ABC123\\-*""[]\'"[b'
    tokens = [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (String, '"ABC123\\-*""[]\'"'),
            (Operator, '['),
            (Name.Variable, 'b'),
            (Text, '\n')
            ]
    assert list(lexer.get_tokens(fragment)) == tokens

def test_logicops(lexer):
    assert list(lexer.get_tokens('and(a,b) q a&b')) == [
            (Name.Function, 'and'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Operator, '&'),
            (Name.Variable, 'b'),
            (Text, '\n')
            ]
    assert list(lexer.get_tokens('nand(a,b) q a\'&b')) == [
            (Name.Function, 'nand'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Operator, '\''),
            (Operator, '&'),
            (Name.Variable, 'b'),
            (Text, '\n')
            ]
    assert list(lexer.get_tokens('or(a,b) q a!b')) == [
            (Name.Function, 'or'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Operator, '!'),
            (Name.Variable, 'b'),
            (Text, '\n')
            ]
    assert list(lexer.get_tokens('nor(a,b) q a\'!b')) == [
            (Name.Function, 'nor'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Operator, '\''),
            (Operator, '!'),
            (Name.Variable, 'b'),
            (Text, '\n')
            ]

def testSimplePattern(lexer):
    assert list(lexer.get_tokens(' Q var?1.AN')) == [
            (Whitespace, ' '),
            (Keyword, 'Q'),
            (Whitespace, ' '),
            (Name.Variable, 'var'),
            (Operator, '?'),
            (Number, '1'),
            (Punctuation, '.'),
            (Name.Entity, 'AN'),
            (Text, '\n')
            ]

def testSSNPattern(lexer):
    assert list(lexer.get_tokens(' Q var?3N1"-"2N1"-"4N')) == [
            (Whitespace, ' '),
            (Keyword, 'Q'),
            (Whitespace, ' '),
            (Name.Variable, 'var'),
            (Operator, '?'),
            (Number, '3'),
            (Name.Entity, 'N'),
            (Number, '1'),
            (String, '"-"'),
            (Number, '2'),
            (Name.Entity, 'N'),
            (Number, '1'),
            (String, '"-"'),
            (Number, '4'),
            (Name.Entity, 'N'),
            (Text, '\n')
            ]

def testAlternatorPattern(lexer):
    assert list(lexer.get_tokens(' quit var?1(1"%",1A).AN')) == [
            (Whitespace, ' '),
            (Keyword, 'quit'),
            (Whitespace, ' '),
            (Name.Variable, 'var'),
            (Operator, '?'),
            (Number, '1'),
            (Punctuation, '('),
            (Number, '1'),
            (String, '"%"'),
            (Punctuation, ','),
            (Number, '1'),
            (Name.Entity, 'A'),
            (Punctuation, ')'),
            (Punctuation, '.'),
            (Name.Entity, 'AN'),
            (Text, '\n'),
            ]

def testMProgrammerPatcode(lexer):
    assert list(lexer.get_tokens(' Q S?1YpizzaY')) == [
            (Whitespace, ' '),
            (Keyword, 'Q'),
            (Whitespace, ' '),
            (Name.Variable, 'S'),
            (Operator, '?'),
            (Number, '1'),
            (Name.Entity, 'YpizzaY'),
            (Text, '\n'),
            ]

def testImplementerPatcodes(lexer):
    assert list(lexer.get_tokens(' Q S?1ZbicycleZ')) == [
            (Whitespace, ' '),
            (Keyword, 'Q'),
            (Whitespace, ' '),
            (Name.Variable, 'S'),
            (Operator, '?'),
            (Number, '1'),
            (Name.Entity, 'ZbicycleZ'),
            (Text, '\n'),
            ]

def testIndirectPattern(lexer):
    assert list(lexer.get_tokens(' Q S?@patvar')) == [
            (Whitespace, ' '),
            (Keyword, 'Q'),
            (Whitespace, ' '),
            (Name.Variable, 'S'),
            (Operator, '?'),
            (Operator, '@'),
            (Name.Variable, 'patvar'),
            (Text, '\n'),
            ]

def testBreak(lexer):
    assert list(lexer.get_tokens(' BREAK')) == [
            (Whitespace, ' '),
            (Keyword, 'BREAK'),
            (Text, '\n'),
            ]

def testBreakQuit(lexer):
    assert list(lexer.get_tokens(' b:debug  q')) == [
            (Whitespace, ' '),
            (Keyword, 'b'),
            (Operator, ':'),
            (Name.Variable, 'debug'),
            (Whitespace, '  '),
            (Keyword, 'q'),
            (Text, '\n'),
            ]

def testSimpleClose(lexer):
    assert list(lexer.get_tokens(' close DEVICE')) == [
            (Whitespace, ' '),
            (Keyword, 'close'),
            (Whitespace, ' '),
            (Name.Variable, 'DEVICE'),
            (Text, '\n'),
            ]

def testIndirectClose(lexer):
    assert list(lexer.get_tokens(' close @closeargs')) == [
            (Whitespace, ' '),
            (Keyword, 'close'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'closeargs'),
            (Text, '\n'),
            ]

def testMultipleClose(lexer):
    assert list(lexer.get_tokens(' close DEVICE1,DEVICE2')) == [
            (Whitespace, ' '),
            (Keyword, 'close'),
            (Whitespace, ' '),
            (Name.Variable, 'DEVICE1'),
            (Punctuation, ','),
            (Name.Variable, 'DEVICE2'),
            (Text, '\n'),
            ]
    
def testComplexClose(lexer):
    assert list(lexer.get_tokens(' c device:(param1:key2=value2)')) == [
            (Whitespace, ' '),
            (Keyword, 'c'),
            (Whitespace, ' '),
            (Name.Variable, 'device'),
            (Punctuation, ':'),
            (Punctuation, '('),
            (Name.Variable, 'param1'),
            (Punctuation, ':'),
            (Name.Variable, 'key2'),
            (Operator, '='),
            (Name.Variable, 'value2'),
            (Punctuation, ')'),
            (Text, '\n'),
            ]

def testDoLabel(lexer):
    assert list(lexer.get_tokens(' DO label')) == [
            (Whitespace, ' '),
            (Keyword, 'DO'),
            (Whitespace, ' '),
            (Name.Label, 'label'),
            (Text, '\n'),
            ]

def testDoRoutine(lexer):
    assert list(lexer.get_tokens(' DO ^ROUTINE')) == [
            (Whitespace, ' '),
            (Keyword, 'DO'),
            (Whitespace, ' '),
            (Punctuation, '^'),
            (Name.Namespace, 'ROUTINE'),
            (Text, '\n'),
            ]

def testDoTagAndRoutine(lexer):
    assert list(lexer.get_tokens(' d tag^ROUTINE')) == [
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Whitespace, ' '),
            (Name.Label, 'tag'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROUTINE'),
            (Text, '\n'),
            ]

def testDoWithArgs(lexer):
    assert list(lexer.get_tokens(' d tag^ROUTINE(arg1,arg2)')) == [
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Whitespace, ' '),
            (Name.Label, 'tag'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROUTINE'),
            (Punctuation, '('),
            (Name.Variable, 'arg1'),
            (Punctuation, ','),
            (Name.Variable, 'arg2'),
            (Punctuation, ')'),
            (Text, '\n'),
            ]

def testDoArgsByReference(lexer):
    assert list(lexer.get_tokens(' d tag^ROUTINE(.arg1,.@arg2)')) == [
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Whitespace, ' '),
            (Name.Label, 'tag'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROUTINE'),
            (Punctuation, '('),
            (Punctuation, '.'),
            (Name.Variable, 'arg1'),
            (Punctuation, ','),
            # The period '.' denotes that this is pass-by-reference - not sure if this is "punctuation" or "operator"
            (Punctuation, '.'),
            (Operator, '@'),
            (Name.Variable, 'arg2'),
            (Punctuation, ')'),
            (Text, '\n'),
            ]

def testDoPostcondQuit(lexer):
    # Testing having multiple spaces between commands, which is allowed
    assert list(lexer.get_tokens(' d:tvexpr tag^ROUTINE   QUIT')) == [
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Operator, ':'),
            (Name.Variable, 'tvexpr'),
            (Whitespace, ' '),
            (Name.Label, 'tag'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROUTINE'),
            (Whitespace, '   '),
            (Keyword, 'QUIT'),
            (Text, '\n'),
            ]

def testDoBlock(lexer):
    assert list(lexer.get_tokens(' d  q:done')) == [
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Whitespace, '  '),
            (Keyword, 'q'),
            (Operator, ':'),
            (Name.Variable, 'done'),
            (Text, '\n'),
            ]

def testDoMultipleWPostcont(lexer):
    assert list(lexer.get_tokens(' do tag1:one,tag^ROU2:two')) == [
            (Whitespace, ' '),
            (Keyword, 'do'),
            (Whitespace, ' '),
            (Name.Label, 'tag1'),
            (Operator, ':'),
            (Name.Variable, 'one'),
            (Punctuation, ','),
            (Name.Label, 'tag'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROU2'),
            (Operator, ':'),
            (Name.Variable, 'two'),
            (Text, '\n'),
            ]

def testDoPostcondWParams(lexer):
    assert list(lexer.get_tokens(' do tag(1):one')) == [
            (Whitespace, ' '),
            (Keyword, 'do'),
            (Whitespace, ' '),
            (Name.Label, 'tag'),
            (Punctuation, '('),
            (Number, '1'),
            (Punctuation, ')'),
            (Operator, ':'),
            (Name.Variable, 'one'),
            (Text, '\n'),
            ]

def testDoIndirectArg(lexer):
    assert list(lexer.get_tokens(' d @doargs')) == [
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'doargs'),
            (Text, '\n'),
            ]

def testDoEntryref(lexer):
    assert list(lexer.get_tokens(' Do tag+7,other+offset^ROUTINE')) == [
            (Whitespace, ' '),
            (Keyword, 'Do'),
            (Whitespace, ' '),
            (Name.Label, 'tag'),
            (Operator, '+'),
            (Number, '7'),
            (Punctuation, ','),
            (Name.Label, 'other'),
            (Operator, '+'),
            (Name.Variable, 'offset'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROUTINE'),
            (Text, '\n'),
            ]

def testDoEntryrefWIndriects(lexer):
    assert list(lexer.get_tokens(' Do @tagname+x^@rounames(1):tv')) == [
            (Whitespace, ' '),
            (Keyword, 'Do'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'tagname'),
            (Operator, '+'),
            (Name.Variable, 'x'),
            (Punctuation, '^'),
            (Operator, '@'),
            (Name.Variable, 'rounames'),
            (Punctuation, '('),
            (Number, '1'),
            (Punctuation, ')'),
            (Operator, ':'),
            (Name.Variable, 'tv'),
            (Text, '\n'),
            ]

def testDoExternref(lexer):
    assert list(lexer.get_tokens(' do &PACKAGE.ROUTINE,&ROUTINE(arg),&PACKAGE.LABEL^ROUTINE(1,"abc")')) == [
            (Whitespace, ' '),
            (Keyword, 'do'),
            (Whitespace, ' '),
            (Name.Namespace, '&PACKAGE'),
            (Punctuation, '.'),
            (Name.Namespace, 'ROUTINE'),
            (Punctuation, ','),
            (Name.Namespace, '&ROUTINE'),
            (Punctuation, '('),
            (Name.Variable, 'arg'),
            (Punctuation, ')'),
            (Punctuation, ','),
            (Name.Namespace, '&PACKAGE'),
            (Punctuation, '.'),
            (Name.Label, 'LABEL'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROUTINE'),
            (Punctuation, '('),
            (Number, '1'),
            (Punctuation, ','),
            (String, '"abc"'),
            (Punctuation, ')'),
            (Text, '\n'),
            ]

def testElse(lexer):
    assert list(lexer.get_tokens(' else  do something')) == [
            (Whitespace, ' '),
            (Keyword, 'else'),
            (Whitespace, '  '),
            (Keyword, 'do'),
            (Whitespace, ' '),
            (Name.Label, 'something'),
            (Text, '\n'),
            ]

def testElseAbbr(lexer):
    assert list(lexer.get_tokens(' e  q')) == [
            (Whitespace, ' '),
            (Keyword, 'e'),
            (Whitespace, '  '),
            (Keyword, 'q'),
            (Text, '\n'),
            ]

def testForEver(lexer):
    assert list(lexer.get_tokens(' for  do')) == [
            (Whitespace, ' '),
            (Keyword, 'for'),
            (Whitespace, '  '),
            (Keyword, 'do'),
            (Text, '\n'),
            ]

def testForOnce(lexer):
    assert list(lexer.get_tokens(' f x=1 d')) == [
            (Whitespace, ' '),
            (Keyword, 'f'),
            (Whitespace, ' '),
            (Name.Variable, 'x'),
            (Operator, '='),
            (Number, '1'),
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Text, '\n'),
            ]

def testForBounds(lexer):
    assert list(lexer.get_tokens(' f x=1:1:5 d')) == [
            (Whitespace, ' '),
            (Keyword, 'f'),
            (Whitespace, ' '),
            (Name.Variable, 'x'),
            (Operator, '='),
            (Number, '1'),
            (Punctuation, ':'),
            (Number, '1'),
            (Punctuation, ':'),
            (Number, '5'),
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Text, '\n'),
            ]

def testForEverSteps(lexer):
    assert list(lexer.get_tokens(' f x=1:1 d')) == [
            (Whitespace, ' '),
            (Keyword, 'f'),
            (Whitespace, ' '),
            (Name.Variable, 'x'),
            (Operator, '='),
            (Number, '1'),
            (Punctuation, ':'),
            (Number, '1'),
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Text, '\n'),
            ]

def testGotoEntryrefs(lexer):
    assert list(lexer.get_tokens(' goto:done abort:fail,tag+3^ROUTINE')) == [
            (Whitespace, ' '),
            (Keyword, 'goto'),
            (Operator, ':'),
            (Name.Variable, 'done'),
            (Whitespace, ' '),
            (Name.Label, 'abort'),
            (Operator, ':'),
            (Name.Variable, 'fail'),
            (Punctuation, ','),
            (Name.Label, 'tag'),
            (Operator, '+'),
            (Number, '3'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROUTINE'),
            (Text, '\n'),
            ]

def testGotoIndirected(lexer):
    assert list(lexer.get_tokens(' g @gotoargs')) == [
            (Whitespace, ' '),
            (Keyword, 'g'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'gotoargs'),
            (Text, '\n'),
            ]

def testHalt(lexer):
    assert list(lexer.get_tokens(' halt')) == [
            (Whitespace, ' '),
            (Keyword, 'halt'),
            (Text, '\n'),
            ]

def testHaltAbbr(lexer):
    assert list(lexer.get_tokens(' H  ; Halt')) == [
            (Whitespace, ' '),
            (Keyword, 'H'),
            (Whitespace, '  '),
            (Comment, '; Halt'),
            (Text, '\n'),
            ]

def testHang(lexer):
    assert list(lexer.get_tokens(' hang:wait 2,extraTime')) == [
            (Whitespace, ' '),
            (Keyword, 'hang'),
            (Operator, ':'),
            (Name.Variable, 'wait'),
            (Whitespace, ' '),
            (Number, '2'),
            (Punctuation, ','),
            (Name.Variable, 'extraTime'),
            (Text, '\n'),
            ]

def testHangAbbr(lexer):
    assert list(lexer.get_tokens(' h 10 ; Hang ten!')) == [
            (Whitespace, ' '),
            (Keyword, 'h'),
            (Whitespace, ' '),
            (Number, '10'),
            (Whitespace, ' '),
            (Comment, '; Hang ten!'),
            (Text, '\n'),
            ]

def testIf(lexer):
    assert list(lexer.get_tokens(' if x=10,y>5 do')) == [
            (Whitespace, ' '),
            (Keyword, 'if'),
            (Whitespace, ' '),
            (Name.Variable, 'x'),
            (Operator, '='),
            (Number, '10'),
            (Punctuation, ','),
            (Name.Variable, 'y'),
            (Operator, '>'),
            (Number, '5'),
            (Whitespace, ' '),
            (Keyword, 'do'),
            (Text, '\n'),
            ]

def testIfAbbrEmpty(lexer):
    assert list(lexer.get_tokens(' i  d  q')) == [
            (Whitespace, ' '),
            (Keyword, 'i'),
            (Whitespace, '  '),
            (Keyword, 'd'),
            (Whitespace, '  '),
            (Keyword, 'q'),
            (Text, '\n'),
            ]

def testJob(lexer):
    assert list(lexer.get_tokens(' job tag+4^ROUTINE:foo:5')) == [
            (Whitespace, ' '),
            (Keyword, 'job'),
            (Whitespace, ' '),
            (Name.Label, 'tag'),
            (Operator, '+'),
            (Number, '4'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROUTINE'),
            (Punctuation, ':'),
            (Name.Variable, 'foo'),
            (Punctuation, ':'),
            (Number, '5'),
            (Text, '\n'),
            ]

def testJobList(lexer):
    assert list(lexer.get_tokens(
        ' j:bar tag^ROUTINE(1,"abc"):(startup="/opt/foo/script":err="job-err.txt"):3,@jobargs')) == [
            (Whitespace, ' '),
            (Keyword, 'j'),
            (Operator, ':'),
            (Name.Variable, 'bar'),
            (Whitespace, ' '),
            (Name.Label, 'tag'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROUTINE'),
            (Punctuation, '('),
            (Number, '1'),
            (Punctuation, ','),
            (String, '"abc"'),
            (Punctuation, ')'),
            (Punctuation, ':'),
            (Punctuation, '('),
            (Name.Variale, 'startup'),
            (Operator, '='),
            (String, '"/opt/foo/script"'),
            (Punctuation, ':'),
            (Name.Variable, 'err'),
            (Operator, '='),
            (String, '"job-err.txt"'),
            (Punctuation, ')'),
            (Punctuation, ':'),
            (Number, '3'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'jobargs'),
            (Text, '\n'),
            ]
