#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Test that syntax highlighting for USD files works correctly."""

import textwrap
import unittest

from pygments import lexers
from pygments import token


class UsdTest(unittest.TestCase):
    def setUp(self):
        self.lexer = lexers.UsdLexer()

    def _get(self, code):
        return list(self.lexer.get_tokens(code))

    def test_asset_path(self):
        code = '@/some/path/to/a/file/foo.usda@'
        expected = [
            (token.String.Interpol, code),
            (token.Text, '\n'),
        ]

        self.assertEqual(expected, self._get(code))

    def test_asset_path_uri(self):
        code = r'@file://SPECI__Z-_ALIZED(syntax_here)?with_arbitrary@#)(%*&)\characters.tar.gz@'
        expected = [
            (token.String.Interpol, code),
            (token.Text, '\n'),
        ]

        self.assertEqual(expected, self._get(code))

    def test_target_absolute(self):
        for code in [
            '</some/path/here>',
            '</some/another_one/here>',
            '</some/path/here.property_name>',
        ]:
            self.assertEqual(
                [(token.Name.Namespace, code), (token.Text, '\n')],
                self._get(code),
            )

    def test_target_relative(self):
        for code in [
            '<../some/path/here>',
            '<../some/another_one/here>',
            '<../some/path/here.property_name>',
        ]:
            self.assertEqual(
                [(token.Name.Namespace, code), (token.Text, '\n')],
                self._get(code),
            )

    def test_attribute(self):
        normal = 'double foo = 8.0'

        self.assertEqual(
            [
                (token.Token.Keyword.Type, 'double'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Name.Attribute, 'foo'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Operator, '='),
                (token.Token.Text, ' '),
                (token.Token.Literal.Number, '8.0'),
                (token.Token.Text, '\n'),
            ],
            self._get(normal),
        )

        custom = 'custom double foo = 8.0'

        self.assertEqual(
            [
                (token.Token.Keyword.Token, 'custom'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Keyword.Type, 'double'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Name.Attribute, 'foo'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Operator, '='),
                (token.Token.Text, ' '),
                (token.Token.Literal.Number, '8.0'),
                (token.Token.Text, '\n'),
            ],
            self._get(custom),
        )

        uniform = 'uniform double foo = 8.0'

        self.assertEqual(
            [
                (token.Token.Keyword.Token, 'uniform'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Keyword.Type, 'double'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Name.Attribute, 'foo'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Operator, '='),
                (token.Token.Text, ' '),
                (token.Token.Literal.Number, '8.0'),
                (token.Token.Text, '\n'),
            ],
            self._get(uniform),
        )

        custom_uniform = 'custom uniform double foo = 8.0'

        self.assertEqual(
            [
                (token.Token.Keyword.Token, 'custom'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Keyword.Token, 'uniform'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Keyword.Type, 'double'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Name.Attribute, 'foo'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Operator, '='),
                (token.Token.Text, ' '),
                (token.Token.Literal.Number, '8.0'),
                (token.Token.Text, '\n'),
            ],
            self._get(custom_uniform),
        )

        underscore = 'custom double foo_underscore_name = 8.0'

        self.assertEqual(
            [
                (token.Token.Keyword.Token, 'custom'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Keyword.Type, 'double'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Name.Attribute, 'foo_underscore_name'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Operator, '='),
                (token.Token.Text, ' '),
                (token.Token.Literal.Number, '8.0'),
                (token.Token.Text, '\n'),
            ],
            self._get(underscore),
        )

        array = 'double[] foo_underscore_name = [10.1, 12.0, 13]'

        self.assertEqual(
            [
                (token.Token.Keyword.Type, 'double[]'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Name.Attribute, 'foo_underscore_name'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Operator, '='),
                (token.Token.Text, ' '),
                (token.Token.Punctuation, '['),
                (token.Token.Literal.Number, '10.1'),
                (token.Token.Generic, ','),
                (token.Token.Text, ' '),
                (token.Token.Literal.Number, '12.0'),
                (token.Token.Generic, ','),
                (token.Token.Text, ' '),
                (token.Token.Literal.Number, '13'),
                (token.Token.Punctuation, ']'),
                (token.Token.Text, '\n'),
            ],
            self._get(array),
        )

        namespaced = 'double[] primvar:foo_thing = [10.1, 12.0, 13]'

        self.assertEqual(
            [
                (token.Token.Keyword.Type, 'double[]'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Name.Attribute, 'primvar:foo_thing'),
                (token.Token.Text.Whitespace, ' '),
                (token.Token.Operator, '='),
                (token.Token.Text, ' '),
                (token.Token.Punctuation, '['),
                (token.Token.Literal.Number, '10.1'),
                (token.Token.Generic, ','),
                (token.Token.Text, ' '),
                (token.Token.Literal.Number, '12.0'),
                (token.Token.Generic, ','),
                (token.Token.Text, ' '),
                (token.Token.Literal.Number, '13'),
                (token.Token.Punctuation, ']'),
                (token.Token.Text, '\n'),
            ],
            self._get(namespaced),
        )

        timesamples = textwrap.dedent(
            '''\
            custom int[] foo = [8, 10, 14]
            custom int[] foo.timeSamples = {
                1: [8, 0, 14],
                2: [-8, 0, 14],
            }
            '''
        )

        self.assertEqual(
            [
                (token.Token.Keyword.Token, u'custom'),
                (token.Token.Text.Whitespace, u' '),
                (token.Token.Keyword.Type, u'int[]'),
                (token.Token.Text.Whitespace, u' '),
                (token.Token.Name.Attribute, u'foo'),
                (token.Token.Text.Whitespace, u' '),
                (token.Token.Operator, u'='),
                (token.Token.Text, u' '),
                (token.Token.Punctuation, u'['),
                (token.Token.Literal.Number, u'8'),
                (token.Token.Generic, u','),
                (token.Token.Text, u' '),
                (token.Token.Literal.Number, u'10'),
                (token.Token.Generic, u','),
                (token.Token.Text, u' '),
                (token.Token.Literal.Number, u'14'),
                (token.Token.Punctuation, u']'),
                (token.Token.Text, u'\n'),
                (token.Token.Keyword.Token, u'custom'),
                (token.Token.Text.Whitespace, u' '),
                (token.Token.Keyword.Type, u'int[]'),
                (token.Token.Text.Whitespace, u' '),
                (token.Token.Name.Attribute, u'foo'),
                (token.Generic, u'.'),
                (token.Name.Keyword.Tokens, u'timeSamples'),
                (token.Token.Text.Whitespace, u' '),
                (token.Token.Operator, u'='),
                (token.Token.Text, u' '),
                (token.Token.Punctuation, u'{'),
                (token.Token.Text, u'\n    '),
                (token.Token.Literal.Number, u'1'),
                (token.Token.Generic, u':'),
                (token.Token.Text, u' '),
                (token.Token.Punctuation, u'['),
                (token.Token.Literal.Number, u'8'),
                (token.Token.Generic, u','),
                (token.Token.Text, u' '),
                (token.Token.Literal.Number, u'0'),
                (token.Token.Generic, u','),
                (token.Token.Text, u' '),
                (token.Token.Literal.Number, u'14'),
                (token.Token.Punctuation, u']'),
                (token.Token.Generic, u','),
                (token.Token.Text, u'\n    '),
                (token.Token.Literal.Number, u'2'),
                (token.Token.Generic, u':'),
                (token.Token.Text, u' '),
                (token.Token.Punctuation, u'['),
                (token.Token.Literal.Number, u'-8'),
                (token.Token.Generic, u','),
                (token.Token.Text, u' '),
                (token.Token.Literal.Number, u'0'),
                (token.Token.Generic, u','),
                (token.Token.Text, u' '),
                (token.Token.Literal.Number, u'14'),
                (token.Token.Punctuation, u']'),
                (token.Token.Generic, u','),
                (token.Token.Text, u'\n'),
                (token.Token.Punctuation, u'}'),
                (token.Token.Text, u'\n'),
            ],
            self._get(timesamples),
        )

    # def test_string_priority(self):
    #     raise NotImplementedError()

    def test_numbers(self):
        code = '8 8.0123312132, -4 -14.123'

        self.assertEqual(
            [
                (token.Token.Literal.Number, u'8'),
                (token.Token.Text, u' '),
                (token.Token.Literal.Number, u'8.0123312132'),
                (token.Token.Generic, u','),
                (token.Token.Text, u' '),
                (token.Token.Literal.Number, u'-4'),
                (token.Token.Text, u' '),
                (token.Token.Literal.Number, u'-14.123'),
                (token.Token.Text, u'\n'),
            ],
            self._get(code),
        )

    # def test_variant_set(self):
    #     code = textwrap.dedent(
    #         """
    #         def Xform "BottleMedical" (
    #             kind = "prop"
    #             payload = @./BottleMedical_payload.usd@</BottleMedical>
    #             variants = {
    #                 string modelingVariant = "LiquidBottleLg"
    #                 string shadingComplexity = "full"
    #             }
    #             add variantSets = ["modelingVariant", "shadingComplexity"]
    #         )
    #         {
    #             variantSet "modelingVariant" = {
    #                 "ALL_VARIANTS" {
    #                 }
    #             }
    #         }
    #         """
    #     )
    #     import pprint
    #
    #     pprint.pprint(self._get(code), indent=4)
    #     raise ValueError(self._get(code))
    #
    #     self.assertEqual(
    #         [   (Token.Keyword.Tokens, u'def'),
    #             (Token.Text, u' '),
    #             (Token.Generic, u'Xform'),
    #             (Token.Text, u' '),
    #             (Token.Literal.String, u'"BottleMedical"'),
    #             (Token.Text, u' '),
    #             (Token.Punctuation, u'('),
    #             (Token.Text, u'\n    '),
    #             (Token.Name.Attribute, u'kind'),
    #             (Token.Text, u' '),
    #             (Token.Operator, u'='),
    #             (Token.Text, u' '),
    #             (Token.Literal.String, u'"prop"'),
    #             (Token.Text, u'\n    '),
    #             (Token.Keyword.Tokens, u'payload'),
    #             (Token.Text, u' '),
    #             (Token.Operator, u'='),
    #             (Token.Text, u' '),
    #             (Token.Literal.String.Interpol, u'@./BottleMedical_payload.usd@'),
    #             (Token.Name.Namespace, u'</BottleMedical>'),
    #             (Token.Text, u'\n    '),
    #             (Token.Keyword.Tokens, u'variants'),
    #             (Token.Text, u' '),
    #             (Token.Operator, u'='),
    #             (Token.Text, u' '),
    #             (Token.Punctuation, u'{'),
    #             (Token.Text, u'\n        '),
    #             (Token.Keyword.Type, u'string'),
    #             (Token.Text.Whitespace, u' '),
    #             (Token.Name.Attribute, u'modelingVariant'),
    #             (Token.Text.Whitespace, u' '),
    #             (Token.Operator, u'='),
    #             (Token.Text, u' '),
    #             (Token.Literal.String, u'"LiquidBottleLg"'),
    #             (Token.Text, u'\n        '),
    #             (Token.Keyword.Type, u'string'),
    #             (Token.Text.Whitespace, u' '),
    #             (Token.Name.Attribute, u'shadingComplexity'),
    #             (Token.Text.Whitespace, u' '),
    #             (Token.Operator, u'='),
    #             (Token.Text, u' '),
    #             (Token.Literal.String, u'"full"'),
    #             (Token.Text, u'\n    '),
    #             (Token.Punctuation, u'}'),
    #             (Token.Text, u'\n    '),
    #             (Token.Keyword.Type, u'add'),
    #             (Token.Text.Whitespace, u' '),
    #             (Token.Name.Attribute, u'variantSets'),
    #             (Token.Text.Whitespace, u' '),
    #             (Token.Operator, u'='),
    #             (Token.Text, u' '),
    #             (Token.Punctuation, u'['),
    #             (Token.Literal.String, u'"modelingVariant", "shadingComplexity"'),
    #             (Token.Punctuation, u']'),
    #             (Token.Text, u'\n'),
    #             (Token.Punctuation, u')'),
    #             (Token.Text, u'\n'),
    #             (Token.Punctuation, u'{'),
    #             (Token.Text, u'\n    '),
    #             (Token.Keyword.Tokens, u'variantSet'),
    #             (Token.Text, u' '),
    #             (Token.Literal.String, u'"modelingVariant"'),
    #             (Token.Text, u' '),
    #             (Token.Operator, u'='),
    #             (Token.Text, u' '),
    #             (Token.Punctuation, u'{'),
    #             (Token.Text, u'\n        '),
    #             (Token.Literal.String, u'"ALL_VARIANTS"'),
    #             (Token.Text, u' '),
    #             (Token.Punctuation, u'{'),
    #             (Token.Text, u'\n        '),
    #             (Token.Punctuation, u'}'),
    #             (Token.Text, u'\n    '),
    #             (Token.Punctuation, u'}'),
    #             (Token.Text, u'\n'),
    #             (Token.Punctuation, u'}'),
    #             (Token.Text, u'\n')],
    #         self._get(code),
    #     )
    #
    # def test_metadata(self):
    #     raise NotImplementedError()
    #
    # def test_string_single_line(self):
    #     raise NotImplementedError()
    #
    # def test_string_multiple_line(self):
    #     raise NotImplementedError()
