# -*- coding: utf-8 -*-
"""
    Basic CrystalLexer Test
    ~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from __future__ import unicode_literals
import unittest

from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Error
from pygments.lexers import CrystalLexer


class CrystalTest(unittest.TestCase):

    def setUp(self):
        self.lexer = CrystalLexer()
        self.maxDiff = None

    def testRangeSyntax1(self):
        fragment = '1...3\n'
        tokens = [
            (Number.Integer, '1'),
            (Operator, '...'),
            (Number.Integer, '3'),
            (Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testRangeSyntax2(self):
        fragment = '1 .. 3\n'
        tokens = [
            (Number.Integer, '1'),
            (Text, ' '),
            (Operator, '..'),
            (Text, ' '),
            (Number.Integer, '3'),
            (Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testInterpolationNestedCurly(self):
        fragment = (
            '"A#{ (3..5).group_by { |x| x/2}.map '
            'do |k,v| "#{k}" end.join }" + "Z"\n')
        tokens = [
            (String.Double, '"'),
            (String.Double, 'A'),
            (String.Interpol, '#{'),
            (Text, ' '),
            (Punctuation, '('),
            (Number.Integer, '3'),
            (Operator, '..'),
            (Number.Integer, '5'),
            (Punctuation, ')'),
            (Operator, '.'),
            (Name, 'group_by'),
            (Text, ' '),
            (String.Interpol, '{'),
            (Text, ' '),
            (Operator, '|'),
            (Name, 'x'),
            (Operator, '|'),
            (Text, ' '),
            (Name, 'x'),
            (Operator, '/'),
            (Number.Integer, '2'),
            (String.Interpol, '}'),
            (Operator, '.'),
            (Name, 'map'),
            (Text, ' '),
            (Keyword, 'do'),
            (Text, ' '),
            (Operator, '|'),
            (Name, 'k'),
            (Punctuation, ','),
            (Name, 'v'),
            (Operator, '|'),
            (Text, ' '),
            (String.Double, '"'),
            (String.Interpol, '#{'),
            (Name, 'k'),
            (String.Interpol, '}'),
            (String.Double, '"'),
            (Text, ' '),
            (Keyword, 'end'),
            (Operator, '.'),
            (Name, 'join'),
            (Text, ' '),
            (String.Interpol, '}'),
            (String.Double, '"'),
            (Text, ' '),
            (Operator, '+'),
            (Text, ' '),
            (String.Double, '"'),
            (String.Double, 'Z'),
            (String.Double, '"'),
            (Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testOperatorMethods(self):
        fragment = '([] of Int32).[]?(5)\n'
        tokens = [
            (Punctuation, '('),
            (Operator, '['),
            (Operator, ']'),
            (Text, ' '),
            (Keyword, 'of'),
            (Text, ' '),
            (Name.Builtin, 'Int32'),
            (Punctuation, ')'),
            (Operator, '.'),
            (Name.Operator, '[]?'),
            (Punctuation, '('),
            (Number.Integer, '5'),
            (Punctuation, ')'),
            (Text, '\n')
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
    
    def testArrayAccess(self):
        fragment = '[5][5]?\n'
        tokens = [
            (Operator, '['),
            (Number.Integer, '5'),
            (Operator, ']'),
            (Operator, '['),
            (Number.Integer, '5'),
            (Operator, ']?'),
            (Text, '\n')
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testNumbers(self):
        for kind, testset in [
            (Number.Integer, '0  1  1_000_000  1u8  11231231231121312i64'),
            (Number.Float, '0.0  1.0_f32  1_f32  0f64  1e+4  1e111  1_234.567_890'),
            (Number.Bin, '0b1001_0110  0b0u8'),
            (Number.Oct, '0o17  0o7_i32'),
            (Number.Hex, '0xdeadBEEF'),
        ]:
            for fragment in testset.split():
                self.assertEqual([(kind, fragment), (Text, '\n')],
                                 list(self.lexer.get_tokens(fragment + '\n')))

        for fragment in '01  0b2  0x129g2  0o12358'.split():
            self.assertEqual(next(self.lexer.get_tokens(fragment + '\n'))[0],
                             Error)

    def testChars(self):
        for fragment in ["'a'", "'я'", "'\\u{1234}'", "'\n'"]:
            self.assertEqual([(String.Char, fragment), (Text, '\n')],
                             list(self.lexer.get_tokens(fragment + '\n')))
        self.assertEqual(next(self.lexer.get_tokens("'abc'"))[0], Error)

    def testMacro(self):
        fragment = (
            'def<=>(other : self) : Int\n'
            '{%for field in %w(first_name middle_name last_name)%}\n'
            'cmp={{field.id}}<=>other.{{field.id}}\n'
            'return cmp if cmp!=0\n'
            '{%end%}\n'
            '0\n'
            'end\n')
        tokens = [
            (Keyword, 'def'),
            (Name.Function, '<=>'),
            (Punctuation, '('),
            (Name, 'other'),
            (Text, ' '),
            (Punctuation, ':'),
            (Text, ' '),
            (Keyword.Pseudo, 'self'),
            (Punctuation, ')'),
            (Text, ' '),
            (Punctuation, ':'),
            (Text, ' '),
            (Name.Builtin, 'Int'),
            (Text, '\n'),
            (String.Interpol, '{%'),
            (Keyword, 'for'),
            (Text, ' '),
            (Name, 'field'),
            (Text, ' '),
            (Keyword, 'in'),
            (Text, ' '),
            (String.Other, '%w('),
            (String.Other, 'first_name middle_name last_name'),
            (String.Other, ')'),
            (String.Interpol, '%}'),
            (Text, '\n'),
            (Name, 'cmp'),
            (Operator, '='),
            (String.Interpol, '{{'),
            (Name, 'field'),
            (Operator, '.'),
            (Name, 'id'),
            (String.Interpol, '}}'),
            (Operator, '<=>'),
            (Name, 'other'),
            (Operator, '.'),
            (String.Interpol, '{{'),
            (Name, 'field'),
            (Operator, '.'),
            (Name, 'id'),
            (String.Interpol, '}}'),
            (Text, '\n'),
            (Keyword, 'return'),
            (Text, ' '),
            (Name, 'cmp'),
            (Text, ' '),
            (Keyword, 'if'),
            (Text, ' '),
            (Name, 'cmp'),
            (Operator, '!='),
            (Number.Integer, '0'),
            (Text, '\n'),
            (String.Interpol, '{%'),
            (Keyword, 'end'),
            (String.Interpol, '%}'),
            (Text, '\n'),
            (Number.Integer, '0'),
            (Text, '\n'),
            (Keyword, 'end'),
            (Text, '\n')
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testLib(self):
        fragment = (
            '@[Link("some")]\nlib LibSome\n'
            '@[CallConvention("X86_StdCall")]\nfun foo="some.foo"(thing : Void*) : LibC::Int\n'
            'end\n')
        tokens = [
            (Operator, '@['),
            (Name.Decorator, 'Link'),
            (Punctuation, '('),
            (String.Double, '"'),
            (String.Double, 'some'),
            (String.Double, '"'),
            (Punctuation, ')'),
            (Operator, ']'),
            (Text, '\n'),
            (Keyword, 'lib'),
            (Text, ' '),
            (Name.Namespace, 'LibSome'),
            (Text, '\n'),
            (Operator, '@['),
            (Name.Decorator, 'CallConvention'),
            (Punctuation, '('),
            (String.Double, '"'),
            (String.Double, 'X86_StdCall'),
            (String.Double, '"'),
            (Punctuation, ')'),
            (Operator, ']'),
            (Text, '\n'),
            (Keyword, 'fun'),
            (Text, ' '),
            (Name.Function, 'foo'),
            (Operator, '='),
            (String.Double, '"'),
            (String.Double, 'some.foo'),
            (String.Double, '"'),
            (Punctuation, '('),
            (Name, 'thing'),
            (Text, ' '),
            (Punctuation, ':'),
            (Text, ' '),
            (Name.Builtin, 'Void'),
            (Operator, '*'),
            (Punctuation, ')'),
            (Text, ' '),
            (Punctuation, ':'),
            (Text, ' '),
            (Name, 'LibC'),
            (Operator, '::'),
            (Name.Builtin, 'Int'),
            (Text, '\n'),
            (Keyword, 'end'),
            (Text, '\n')
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))

    def testEscapedBracestring(self):
        fragment = 'str.gsub(%r{\\\\\\\\}, "/")\n'
        tokens = [
            (Name, 'str'),
            (Operator, '.'),
            (Name, 'gsub'),
            (Punctuation, '('),
            (String.Regex, '%r{'),
            (String.Regex, '\\\\'),
            (String.Regex, '\\\\'),
            (String.Regex, '}'),
            (Punctuation, ','),
            (Text, ' '),
            (String.Double, '"'),
            (String.Double, '/'),
            (String.Double, '"'),
            (Punctuation, ')'),
            (Text, '\n'),
        ]
        self.assertEqual(tokens, list(self.lexer.get_tokens(fragment)))
