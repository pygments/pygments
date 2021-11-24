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

def testQuitLine(lexer):
    fragment = ' q'
    tokens = [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def testQuitValueLine(lexer):
    fragment = ' QUIT 1'
    tokens = [
        (Whitespace, ' '),
        (Keyword, 'QUIT'),
        (Whitespace, ' '),
        (Number, '1'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def testQuitMorphLine(lexer):
    fragment = ' q:$QUIT 1 Q'
    tokens = [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Punctuation, ':'),
        (Name.Variable.Magic, '$QUIT'),
        (Whitespace, ' '),
        (Number, '1'),
        (Whitespace, ' '),
        (Keyword, 'Q'),
        (Text, '\n')
    ]
    assert list(lexer.get_tokens(fragment)) == tokens

def testStubSubroutine(lexer):
    fragment = 'stub q'
    tokens = [
        (Name.Label, 'stub'),
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Text, '\n')
        ]
    assert list(lexer.get_tokens(fragment)) == tokens

def testSimpleFunction(lexer):
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

def testArithmeticUnaryops(lexer):
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
        (Punctuation, ':'),
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

def testCompare(lexer):
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
            (Punctuation, ':'),
            (Name.Variable, 'left'),
            (Operator, '>'),
            (Name.Variable, 'right'),
            (Whitespace, ' '),
            (Number, '1'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Punctuation, ':'),
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

def testQuitPi(lexer):
    assert list(lexer.get_tokens(' q 3.14159265')) == [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Whitespace, ' '),
        (Number, '3.14159265'),
        (Text, '\n')
        ]

def testQuitDecimal(lexer):
    assert list(lexer.get_tokens(' q .5')) == [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Whitespace, ' '),
        (Number, '.5'),
        (Text, '\n')
        ]

def testQuitExponent(lexer):
    assert list(lexer.get_tokens(' q 60.2214E22')) == [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Whitespace, ' '),
        (Number, '60.2214E22'),
        (Text, '\n')
        ]

def testQuitPosExponent(lexer):
    assert list(lexer.get_tokens(' q 3E+8')) == [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Whitespace, ' '),
        (Number, '3E+8'),
        (Text, '\n')
        ]

def testQuitNegExponent(lexer):
    assert list(lexer.get_tokens(' q 6.674E-11')) == [
        (Whitespace, ' '),
        (Keyword, 'q'),
        (Whitespace, ' '),
        (Number, '6.674E-11'),
        (Text, '\n')
        ]

def testBinaryops(lexer):
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

def testStringcomp(lexer):
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
            (Punctuation, ':'),
            (Name.Variable, 'a'),
            (Operator, '='),
            (String, '""'),
            (Whitespace, ' '),
            (Name.Variable, 'b'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Punctuation, ':'),
            (Name.Variable, 'b'),
            (Operator, '\''),
            (Operator, ']'),
            (String, '""'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Text, '\n'),
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Punctuation, ':'),
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
    
def testStringContains(lexer):
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

def testLogicops(lexer):
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

def testGlobal(lexer):
    assert list(lexer.get_tokens(' q ^A(1,"a")')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Global, '^A'),
            (Punctuation, '('),
            (Number, '1'),
            (Punctuation, ','),
            (String, '"a"'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testGlobalEnvironment(lexer):
    assert list(lexer.get_tokens(' q ^|"M.GLD"|A(1,"a")')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Global, '^'),
            (Punctuation, '|'),
            (String, '"M.GLD"'),
            (Punctuation, '|'),
            (Name.Variable.Global, 'A'),
            (Punctuation, '('),
            (Number, '1'),
            (Punctuation, ','),
            (String, '"a"'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testNakedReference(lexer):
    assert list(lexer.get_tokens(' q ^(1_"bar")')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Global, '^'),
            (Punctuation, '('),
            (Number, '1'),
            (Operator, '_'),
            (String, '"bar"'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNCharacter(lexer):
    assert list(lexer.get_tokens(' q ^$Character(expr)')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$Character'),
            (Punctuation, '('),
            (Name.Variable, 'expr'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNCharacterSub(lexer):
    assert list(lexer.get_tokens(' q ^$C(expr,"PATCODE",patcode)')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$C'),
            (Punctuation, '('),
            (Name.Variable, 'expr'),
            (Punctuation, ','),
            (String, '"PATCODE"'),
            (Punctuation, ','),
            (Name.Variable, 'patcode'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNDevice(lexer):
    assert list(lexer.get_tokens(' q ^$Device(dev)')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$Device'),
            (Punctuation, '('),
            (Name.Variable, 'dev'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNDeviceChar(lexer):
    assert list(lexer.get_tokens(' q ^$D(dev,"CHARACTER")')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$D'),
            (Punctuation, '('),
            (Name.Variable, 'dev'),
            (Punctuation, ','),
            (String, '"CHARACTER"'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNGlobal(lexer):
    assert list(lexer.get_tokens(' q ^$Global(gvn)')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$Global'),
            (Punctuation, '('),
            (Name.Variable, 'gvn'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNGlobalChar(lexer):
    assert list(lexer.get_tokens(' q ^$G(gvn,"CHARACTER")')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$G'),
            (Punctuation, '('),
            (Name.Variable, 'gvn'),
            (Punctuation, ','),
            (String, '"CHARACTER"'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNJob(lexer):
    assert list(lexer.get_tokens(' q ^$Job(pid)')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$Job'),
            (Punctuation, '('),
            (Name.Variable, 'pid'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNJobChar(lexer):
    assert list(lexer.get_tokens(' q ^$J(pid,"CHARACTER")')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$J'),
            (Punctuation, '('),
            (Name.Variable, 'pid'),
            (Punctuation, ','),
            (String, '"CHARACTER"'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNLock(lexer):
    assert list(lexer.get_tokens(' q ^$Lock(nref)')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$Lock'),
            (Punctuation, '('),
            (Name.Variable, 'nref'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNRoutine(lexer):
    assert list(lexer.get_tokens(' q ^$ROUTINE(routinexpr)')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$ROUTINE'),
            (Punctuation, '('),
            (Name.Variable, 'routinexpr'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNRoutineChar(lexer):
    assert list(lexer.get_tokens(' q ^$R(routinexpr,"CHARACTER")')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$R'),
            (Punctuation, '('),
            (Name.Variable, 'routinexpr'),
            (Punctuation, ','),
            (String, '"CHARACTER"'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNSystem(lexer):
    assert list(lexer.get_tokens(' q ^$SYSTEM(systemexpr)')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$SYSTEM'),
            (Punctuation, '('),
            (Name.Variable, 'systemexpr'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testSSVNSystemChar(lexer):
    assert list(lexer.get_tokens(' q ^$S(systemexpr,"CHARACTER")')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$S'),
            (Punctuation, '('),
            (Name.Variable, 'systemexpr'),
            (Punctuation, ','),
            (String, '"CHARACTER"'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

# 7.1.3.8 - Impelmentation-specific ssvns starting with ^$Z
def testSSVNZunspecified(lexer):
    assert list(lexer.get_tokens(' q ^$ZCheese(cheddar)')) == [
            (Whitespace, ' '),
            (Keyword, 'q'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '^$ZCheese'),
            (Punctuation, '('),
            (Name.Variable, 'cheddar'),
            (Punctuation, ')'),
            (Text, '\n')
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
            (Punctuation, ':'),
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

def testDoEnvironmentRoutine(lexer):
    assert list(lexer.get_tokens(' d ^|env|ROUTINE')) == [
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Whitespace, ' '),
            (Punctuation, '^'),
            (Punctuation, '|'),
            (Name.Variable, 'env'),
            (Punctuation, '|'),
            (Name.Namespace, 'ROUTINE'),
            (Text, '\n'),
            ]

def testDoEnvironmentLabelRoutine(lexer):
    assert list(lexer.get_tokens(' d label^|"M3.gld"|ROUTINE(1)')) == [
            (Whitespace, ' '),
            (Keyword, 'd'),
            (Whitespace, ' '),
            (Name.Label, 'label'),
            (Punctuation, '^'),
            (Punctuation, '|'),
            (String, '"M3.gld"'),
            (Punctuation, '|'),
            (Name.Namespace, 'ROUTINE'),
            (Punctuation, '('),
            (Number, '1'),
            (Punctuation, ')'),
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
            (Punctuation, ':'),
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
            (Punctuation, ':'),
            (Name.Variable, 'done'),
            (Text, '\n'),
            ]

def testDoMultipleWPostcont(lexer):
    assert list(lexer.get_tokens(' do tag1:one,tag^ROU2:two')) == [
            (Whitespace, ' '),
            (Keyword, 'do'),
            (Whitespace, ' '),
            (Name.Label, 'tag1'),
            (Punctuation, ':'),
            (Name.Variable, 'one'),
            (Punctuation, ','),
            (Name.Label, 'tag'),
            (Punctuation, '^'),
            (Name.Namespace, 'ROU2'),
            (Punctuation, ':'),
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
            (Punctuation, ':'),
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
            (Punctuation, ':'),
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
            (Punctuation, ':'),
            (Name.Variable, 'done'),
            (Whitespace, ' '),
            (Name.Label, 'abort'),
            (Punctuation, ':'),
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
            (Punctuation, ':'),
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
            (Punctuation, ':'),
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
            (Name.Variable, 'startup'),
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

def testKill(lexer):
    assert list(lexer.get_tokens(' kill:done a,b,c(d)')) == [
            (Whitespace, ' '),
            (Keyword, 'kill'),
            (Punctuation, ':'),
            (Name.Variable, 'done'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ','),
            (Name.Variable, 'c'),
            (Punctuation, '('),
            (Name.Variable, 'd'),
            (Punctuation, ')'),
            (Text, '\n')
            ]
def testKillAll(lexer):
    assert list(lexer.get_tokens(' k  q')) == [
            (Whitespace, ' '),
            (Keyword, 'k'),
            (Whitespace, '  '),
            (Keyword, 'q'),
            (Text, '\n')
            ]

def testKillExclusive(lexer):
    assert list(lexer.get_tokens(' k (save,these,@variables)')) == [
            (Whitespace, ' '),
            (Keyword, 'k'),
            (Whitespace, ' '),
            (Punctuation, '('),
            (Name.Variable, 'save'),
            (Punctuation, ','),
            (Name.Variable, 'these'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'variables'),
            (Punctuation, ')'),
            (Text, '\n'),
            ]
def testKillIndirect(lexer):
    assert list(lexer.get_tokens(' k @killargs')) == [
            (Whitespace, ' '),
            (Keyword, 'k'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'killargs'),
            (Text, '\n'),
            ]

def testLock(lexer):
    assert list(lexer.get_tokens(' lock only,these,things')) == [
            (Whitespace, ' '),
            (Keyword, 'lock'),
            (Whitespace, ' '),
            (Name, 'only'),
            (Punctuation, ','),
            (Name, 'these'),
            (Punctuation, ','),
            (Name, 'things'),
            (Text, '\n')
            ]
            

def testLockList(lexer):
    assert list(lexer.get_tokens(' lock +^GLOBALS("foo",1),-(FISH("red",blue),COWS):3,@lockargs,@glos:2')) == [
            (Whitespace, ' '),
            (Keyword, 'lock'),
            (Whitespace, ' '),
            (Operator, '+'),
            (Name, '^GLOBALS'),
            (Punctuation, '('),
            (String, '"foo"'),
            (Punctuation, ','),
            (Number, '1'),
            (Punctuation, ')'),
            (Punctuation, ','),
            (Operator, '-'),
            (Punctuation, '('),
            (Name, 'FISH'),
            (Punctuation, '('),
            (String, '"red"'),
            (Punctuation, ','),
            (Name.Variable, 'blue'),
            (Punctuation, ')'),
            (Punctuation, ','),
            (Name, 'COWS'),
            (Punctuation, ')'),
            (Punctuation, ':'),
            (Number, '3'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'lockargs'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'glos'),
            (Punctuation, ':'),
            (Number, '2'),
            (Text, '\n'),
            ]

def testUnlock(lexer):
    assert list(lexer.get_tokens(' l:done')) == [
            (Whitespace, ' '),
            (Keyword, 'l'),
            (Punctuation, ':'),
            (Name.Variable, 'done'),
            (Text, '\n'),
            ]

def testMerge(lexer):
    assert list(lexer.get_tokens(
        ' merge:ready target(a,b)=^source(c)')) == [
            (Whitespace, ' '),
            (Keyword, 'merge'),
            (Punctuation, ':'),
            (Name.Variable, 'ready'),
            (Whitespace, ' '),
            (Name.Variable, 'target'),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ')'),
            (Operator, '='),
            (Name.Variable.Global, '^source'),
            (Punctuation, '('),
            (Name.Variable, 'c'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testMergeList(lexer):
    assert list(lexer.get_tokens(
        ' m a=b,c=@d,@e=f,@g=@h,@mergarg,@i(j)=@k@(l)')) == [
            (Whitespace, ' '),
            (Keyword, 'm'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Operator, '='),
            (Name.Variable, 'b'),
            (Punctuation, ','),
            (Name.Variable, 'c'),
            (Operator, '='),
            (Operator, '@'),
            (Name.Variable, 'd'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'e'),
            (Operator, '='),
            (Name.Variable, 'f'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'g'),
            (Operator, '='),
            (Operator, '@'),
            (Name.Variable, 'h'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'mergarg'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'i'),
            (Punctuation, '('),
            (Name.Variable, 'j'),
            (Punctuation, ')'),
            (Operator, '='),
            (Operator, '@'),
            (Name.Variable, 'k'),
            (Operator, '@'),
            (Punctuation, '('),
            (Name.Variable, 'l'),
            (Punctuation, ')'),
            (Text, '\n'),
            ]

def testNewLocals(lexer):
    assert list(lexer.get_tokens(
        ' new a,b,c,d')) == [
            (Whitespace, ' '),
            (Keyword, 'new'),
            (Whitespace, ' '),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ','),
            (Name.Variable, 'c'),
            (Punctuation, ','),
            (Name.Variable, 'd'),
            (Text, '\n')
            ]

def testNewAll(lexer):
    assert list(lexer.get_tokens(' new:all  d')) == [
            (Whitespace, ' '),
            (Keyword, 'new'),
            (Punctuation, ':'),
            (Name.Variable, 'all'),
            (Whitespace, '  '),
            (Keyword, 'd'),
            (Text, '\n')
            ]

def testNewSVNandIndirect(lexer):
    assert list(lexer.get_tokens(' n $ETRAP,$ES,@newargs')) == [
            (Whitespace, ' '),
            (Keyword, 'n'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '$ETRAP'),
            (Punctuation, ','),
            (Name.Variable.Magic, '$ES'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'newargs'),
            (Text, '\n')
            ]

def testExclusiveNew(lexer):
    assert list(lexer.get_tokens(' n (dont,new,these)')) == [
            (Whitespace, ' '),
            (Keyword, 'n'),
            (Whitespace, ' '),
            (Punctuation, '('),
            (Name.Variable, 'dont'),
            (Punctuation, ','),
            (Name.Variable, 'new'),
            (Punctuation, ','),
            (Name.Variable, 'these'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testOpen(lexer):
    assert list(lexer.get_tokens(
        ' open devicefile:foo="bar":5:mnemspace')) == [
            (Whitespace, ' '),
            (Keyword, 'open'),
            (Whitespace, ' '),
            (Name.Variable, 'devicefile'),
            (Punctuation, ':'),
            (Name.Variable, 'foo'),
            (Operator, '='),
            (String, '"bar"'),
            (Punctuation, ':'),
            (Number, '5'),
            (Punctuation, ':'),
            (Name.Variable, 'mnemspace'),
            (Text, '\n'),
            ]

def testOpenList(lexer):
    assert list(lexer.get_tokens(
        ' o:ready @openargs,device,dev:foo:1,dev2:::spec,dev3::3' )) == [
            (Whitespace, ' '),
            (Keyword, 'o'),
            (Punctuation, ':'),
            (Name.Variable, 'ready'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'openargs'),
            (Punctuation, ','),
            (Name.Variable, 'device'),
            (Punctuation, ','),
            (Name.Variable, 'dev'),
            (Punctuation, ':'),
            (Name.Variable, 'foo'),
            (Punctuation, ':'),
            (Number, '1'),
            (Punctuation, ','),
            (Name.Variable, 'dev2'),
            (Punctuation, ':'),
            (Punctuation, ':'),
            (Punctuation, ':'),
            (Name.Variable, 'spec'),
            (Punctuation, ','),
            (Name.Variable, 'dev3'),
            (Punctuation, ':'),
            (Punctuation, ':'),
            (Number, '3'),
            (Text, '\n')
            ]

def testMnemonicspecGroup(lexer):
    assert list(lexer.get_tokens(' O dev:::(space1,space2,"steve")')) == [
            (Whitespace, ' '),
            (Keyword, 'O'),
            (Whitespace, ' '),
            (Name.Variable, 'dev'),
            (Punctuation, ':'),
            (Punctuation, ':'),
            (Punctuation, ':'),
            (Punctuation, '('),
            (Name.Variable, 'space1'),
            (Punctuation, ','),
            (Name.Variable, 'space2'),
            (Punctuation, ','),
            (String, '"steve"'),
            (Punctuation, ')'),
            (Text, '\n'),
            ]

def testRead(lexer):
    assert list(lexer.get_tokens(
        ' read #!!?5,"Hello!",!?5,"What is your name? ",name' )) == [
            (Whitespace, ' '),
            (Keyword, 'read'),
            (Whitespace, ' '),
            (Keyword.Pseudo, '#!!'),
            (Keyword.Pseudo, '?'),
            (Number, '5'),
            (Punctuation, ','),
            (String, '"Hello!"'),
            (Punctuation, ','),
            (Keyword.Pseudo, '!'),
            (Keyword.Pseudo, '?'),
            (Number, '5'),
            (Punctuation, ','),
            (String, '"What is your name? "'),
            (Punctuation, ','),
            (Name.Variable, 'name'),
            (Text, '\n')
            ]

def testReadCond(lexer):
    assert list(lexer.get_tokens( " r:'ready @readargs" )) == [
            (Whitespace, ' '),
            (Keyword, 'r'),
            (Punctuation, ':'),
            (Operator, '\''),
            (Name.Variable, 'ready'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'readargs'),
            (Text, '\n'),
            ]

def testReadTimeout(lexer):
    assert list(lexer.get_tokens( ' r var:60' )) == [
            (Whitespace, ' '),
            (Keyword, 'r'),
            (Whitespace, ' '),
            (Name.Variable, 'var'),
            (Punctuation, ':'),
            (Number, '60'),
            (Text, '\n')
            ]

def testReadLength(lexer):
    assert list(lexer.get_tokens( ' r var#20' )) == [
            (Whitespace, ' '),
            (Keyword, 'r'),
            (Whitespace, ' '),
            (Name.Variable, 'var'),
            (Punctuation, '#'),
            (Number, '20'),
            (Text, '\n')
            ]

def testReadLengthTimeout(lexer):
    assert list(lexer.get_tokens( ' r @varname#10:tmo' )) == [
            (Whitespace, ' '),
            (Keyword, 'r'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'varname'),
            (Punctuation, '#'),
            (Number, '10'),
            (Punctuation, ':'),
            (Name.Variable, 'tmo'),
            (Text, '\n')
            ]

def testReadChar(lexer):
    assert list(lexer.get_tokens( ' READ !,"Press a key",*key:15' )) == [
            (Whitespace, ' '),
            (Keyword, 'READ'),
            (Whitespace, ' '),
            (Keyword.Pseudo, '!'),
            (Punctuation, ','),
            (String, '"Press a key"'),
            (Punctuation, ','),
            (Keyword.Pseudo, '*'),
            (Name.Variable, 'key'),
            (Punctuation, ':'),
            (Number, '15'),
            (Text, '\n')
            ]

def testSet(lexer):
    assert list(lexer.get_tokens( ' SET A=1' )) == [
            (Whitespace, ' '),
            (Keyword, 'SET'),
            (Whitespace, ' '),
            (Name.Variable, 'A'),
            (Operator, '='),
            (Number, '1'),
            (Text, '\n')
            ]

def testSetList(lexer):
    assert list(lexer.get_tokens( ' s A=2,B(3)=4' )) == [
            (Whitespace, ' '),
            (Keyword, 's'),
            (Whitespace, ' '),
            (Name.Variable, 'A'),
            (Operator, '='),
            (Number, '2'),
            (Punctuation, ','),
            (Name.Variable, 'B'),
            (Punctuation, '('),
            (Number, '3'),
            (Punctuation, ')'),
            (Operator, '='),
            (Number, '4'),
            (Text, '\n')
            ]

def testSetGroup(lexer):
    assert list(lexer.get_tokens(' s (a,^b,c(2))=5+1')) == [
            (Whitespace, ' '),
            (Keyword, 's'),
            (Whitespace, ' '),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable.Global, '^b'),
            (Punctuation, ','),
            (Name.Variable, 'c'),
            (Punctuation, '('),
            (Number, '2'),
            (Punctuation, ')'),
            (Punctuation, ')'),
            (Operator, '='),
            (Number, '5'),
            (Operator, '+'),
            (Number, '1'),
            (Text, '\n')
            ]

def testSetIndirects(lexer):
    assert list(lexer.get_tokens(' set @("result="_expr),@glo=@glo+1,@glo@(@glo)=2')) == [
            (Whitespace, ' '),
            (Keyword, 'set'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Punctuation, '('),
            (String, '"result="'),
            (Operator, '_'),
            (Name.Variable, 'expr'),
            (Punctuation, ')'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'glo'),
            (Operator, '='),
            (Operator, '@'),
            (Name.Variable, 'glo'),
            (Operator, '+'),
            (Number, '1'),
            (Punctuation, ','),
            (Operator, '@'),
            (Name.Variable, 'glo'),
            (Operator, '@'),
            (Punctuation, '('),
            (Operator, '@'),
            (Name.Variable, 'glo'),
            (Punctuation, ')'),
            (Operator, '='),
            (Number, '2'),
            (Text, '\n')
            ]

def testSetPiece(lexer):
    assert list(lexer.get_tokens(' s $P(foo,",",2)="abc",$PIECE(bar,"^",3,4)="This^That"')) == [
            (Whitespace, ' '),
            (Keyword, 's'),
            (Whitespace, ' '),
            (Name.Function.Magic, '$P'),
            (Punctuation, '('),
            (Name.Variable, 'foo'),
            (Punctuation, ','),
            (String, '","'),
            (Punctuation, ','),
            (Number, '2'),
            (Punctuation, ')'),
            (Operator, '='),
            (String, '"abc"'),
            (Punctuation, ','),
            (Name.Function.Magic, '$PIECE'),
            (Punctuation, '('),
            (Name.Variable, 'bar'),
            (Punctuation, ','),
            (String, '"^"'),
            (Punctuation, ','),
            (Number, '3'),
            (Punctuation, ','),
            (Number, '4'),
            (Punctuation, ')'),
            (Operator, '='),
            (String, '"This^That"'),
            (Text, '\n')
            ]

def testSetExtract(lexer):
    assert list(lexer.get_tokens(' s $E(foo,pos)="4",$Extract(bar,3,5)="abc"')) == [
            (Whitespace, ' '),
            (Keyword, 's'),
            (Whitespace, ' '),
            (Name.Function.Magic, '$E'),
            (Punctuation, '('),
            (Name.Variable, 'foo'),
            (Punctuation, ','),
            (Name.Variable, 'pos'),
            (Punctuation, ')'),
            (Operator, '='),
            (String, '"4"'),
            (Punctuation, ','),
            (Name.Function.Magic, '$Extract'),
            (Punctuation, '('),
            (Name.Variable, 'bar'),
            (Punctuation, ','),
            (Number, '3'),
            (Punctuation, ','),
            (Number, '5'),
            (Punctuation, ')'),
            (Operator, '='),
            (String, '"abc"'),
            (Text, '\n')
            ]

def testSetEVLong(lexer):
    assert list(lexer.get_tokens(' set $ECODE=",U01,",$ETRAP="do handle^ERRORS"')) == [
            (Whitespace, ' '),
            (Keyword, 'set'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '$ECODE'),
            (Operator, '='),
            (String, '",U01,"'),
            (Punctuation, ','),
            (Name.Variable.Magic, '$ETRAP'),
            (Operator, '='),
            (String, '"do handle^ERRORS"'),
            (Text, '\n')
            ]

def testSetEVShort(lexer):
    assert list(lexer.get_tokens(' s $EC=",U02,",$ET="do handle^ERRORS"')) == [
            (Whitespace, ' '),
            (Keyword, 's'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '$EC'),
            (Operator, '='),
            (String, '",U02,"'),
            (Punctuation, ','),
            (Name.Variable.Magic, '$ET'),
            (Operator, '='),
            (String, '"do handle^ERRORS"'),
            (Text, '\n')
            ]

def testSetDevice(lexer):
    assert list(lexer.get_tokens(' s $DEVICE="1,Connection reset",$D=0')) == [
            (Whitespace, ' '),
            (Keyword, 's'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '$DEVICE'),
            (Operator, '='),
            (String, '"1,Connection reset"'),
            (Punctuation, ','),
            (Name.Variable.Magic, '$D'),
            (Operator, '='),
            (Number, '0'),
            (Text, '\n')
            ]

def testSetKey(lexer):
    assert list(lexer.get_tokens(' s $KEY="",$K=$C(10)')) == [
            (Whitespace, ' '),
            (Keyword, 's'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '$KEY'),
            (Operator, '='),
            (String, '""'),
            (Punctuation, ','),
            (Name.Variable.Magic, '$K'),
            (Operator, '='),
            (Name.Function.Magic, '$C'),
            (Punctuation, '('),
            (Number, '10'),
            (Punctuation, ')'),
            (Text, '\n'),
            ]

def testSetXY(lexer):
    assert list(lexer.get_tokens(' set $X=20,$Y=4')) == [
            (Whitespace, ' '),
            (Keyword, 'set'),
            (Whitespace, ' '),
            (Name.Variable.Magic, '$X'),
            (Operator, '='),
            (Number, '20'),
            (Punctuation, ','),
            (Name.Variable.Magic, '$Y'),
            (Operator, '='),
            (Number, '4'),
            (Text, '\n'),
            ]

def testTCommit(lexer):
    assert list(lexer.get_tokens(' tcommit:ready  quit')) == [
            (Whitespace, ' '),
            (Keyword, 'tcommit'),
            (Punctuation, ':'),
            (Name.Variable, 'ready'),
            (Whitespace, '  '),
            (Keyword, 'quit'),
            (Text, '\n')
            ]

def testTREstart(lexer):
    assert list(lexer.get_tokens(' trestart:foo')) == [
            (Whitespace, ' '),
            (Keyword, 'trestart'),
            (Punctuation, ':'),
            (Name.Variable, 'foo'),
            (Text, '\n')
            ]

def testTROllback(lexer):
    assert list(lexer.get_tokens(' tro:abort')) == [
            (Whitespace, ' '),
            (Keyword, 'tro'),
            (Punctuation, ':'),
            (Name.Variable, 'abort'),
            (Text, '\n')
            ]

def testTStart(lexer):
    assert list(lexer.get_tokens(' TStart:\'dryrun')) == [
            (Whitespace, ' '),
            (Keyword, 'TStart'),
            (Punctuation, ':'),
            (Operator, '\''),
            (Name.Variable, 'dryrun'),
            (Text, '\n')
            ]

def testTStartArg(lexer):
    assert list(lexer.get_tokens(' ts foo:serial')) == [
            (Whitespace, ' '),
            (Keyword, 'ts'),
            (Whitespace, ' '),
            (Name.Variable, 'foo'),
            (Punctuation, ':'),
            (Keyword, 'serial'),
            (Text, '\n')
            ]

def testTStartArgInd(lexer):
    assert list(lexer.get_tokens(' ts @foo:t=uniqID')) == [
            (Whitespace, ' '),
            (Keyword, 'ts'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'foo'),
            (Punctuation, ':'),
            (Keyword, 't'),
            (Operator, '='),
            (Name.Variable, 'uniqID'),
            (Text, '\n')
            ]

def testTStartInd(lexer):
    assert list(lexer.get_tokens(' ts @tsarg')) == [
            (Whitespace, ' '),
            (Keyword, 'ts'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'tsarg'),
            (Text, '\n')
            ]

def testTStartGroups(lexer):
    assert list(lexer.get_tokens(' TStart (a,b,c):(serial:t=5:zebra="yes")')) == [
            (Whitespace, ' '),
            (Keyword, 'TStart'),
            (Whitespace, ' '),
            (Punctuation, '('),
            (Name.Variable, 'a'),
            (Punctuation, ','),
            (Name.Variable, 'b'),
            (Punctuation, ','),
            (Name.Variable, 'c'),
            (Punctuation, ')'),
            (Punctuation, ':'),
            (Punctuation, '('),
            (Keyword, 'serial'),
            (Punctuation, ':'),
            (Keyword, 't'),
            (Operator, '='),
            (Number, '5'),
            (Punctuation, ':'),
            (Keyword, 'zebra'),
            (Operator, '='),
            (String, '"yes"'),
            (Punctuation, ')'),
            (Text, '\n')
            ]

def testTStartExclusive(lexer):
    assert list(lexer.get_tokens(' TSTART *')) == [
            (Whitespace, ' '),
            (Keyword, 'TSTART'),
            (Whitespace, ' '),
            (Keyword.Pseudo, '*'),
            (Text, '\n')
            ]

def testTStartEmpty(lexer):
    assert list(lexer.get_tokens( ' TS ():t="label"' )) == [
            (Whitespace, ' '),
            (Keyword, 'TS'),
            (Whitespace, ' '),
            (Punctuation, '('),
            (Punctuation, ')'),
            (Punctuation, ':'),
            (Keyword, 't'),
            (Operator, '='),
            (String, '"label"'),
            (Text, '\n')
            ]

def testUseVar(lexer):
    assert list(lexer.get_tokens( ' use newio')) == [
            (Whitespace, ' '),
            (Keyword, 'use'),
            (Whitespace, ' '),
            (Name.Variable, 'newio'),
            (Text, '\n')
            ]

def testUseIndirected(lexer):
    assert list(lexer.get_tokens(' u:open @useargs')) == [
            (Whitespace, ' '),
            (Keyword, 'u'),
            (Punctuation, ':'),
            (Name.Variable, 'open'),
            (Whitespace, ' '),
            (Operator, '@'),
            (Name.Variable, 'useargs'),
            (Text, '\n'),
            ]

def testUseDeviceParameters(lexer):
    assert list(lexer.get_tokens(' USE dev:foo="bar":mnemspec')) == [
            (Whitespace, ' '),
            (Keyword, 'USE'),
            (Whitespace, ' '),
            (Name.Variable, 'dev'),
            (Punctuation, ':'),
            (Name.Variable, 'foo'),
            (Operator, '='),
            (String, '"bar"'),
            (Punctuation, ':'),
            (Name.Variable, 'mnemspec'),
            (Text, '\n')
            ]

def testUseDeviceMnemOnly(lexer):
    assert list(lexer.get_tokens(' USE dev::mnems')) == [
            (Whitespace, ' '),
            (Keyword, 'USE'),
            (Whitespace, ' '),
            (Name.Variable, 'dev'),
            (Punctuation, ':'),
            (Punctuation, ':'),
            (Name.Variable, 'mnems'),
            (Text, '\n')
            ]

def testView(lexer):
    assert list(lexer.get_tokens(' VIEW')) == [
            (Whitespace, ' '),
            (Keyword, 'VIEW'),
            (Text, '\n')
            ]

def testViewPostcond(lexer):
    assert list(lexer.get_tokens(' v:debug  q')) == [
            (Whitespace, ' '),
            (Keyword, 'v'),
            (Punctuation, ':'),
            (Name.Variable, 'debug'),
            (Whitespace, '  '),
            (Keyword, 'q'),
            (Text, '\n')
            ]