#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Test that syntax highlighting for USD files works correctly."""

import textwrap
import unittest

from pygments import lexers, token


class UsdTest(unittest.TestCase):
    def setUp(self):
        self.lexer = lexers.UsdLexer()

    def _get(self, code):
        return list(self.lexer.get_tokens(code))

    def test_asset_path(self):
        code = "@/some/path/to/a/file/foo.usda@"
        expected = [
            (token.String.Interpol, code),
            (token.Text, "\n"),
        ]

        self.assertEqual(expected, self._get(code))

    def test_asset_path_uri(self):
        code = r"@file://SPECI__Z-_ALIZED(syntax_here)?with_arbitrary@#)(%*&)\characters.tar.gz@"
        expected = [
            (token.String.Interpol, code),
            (token.Text, "\n"),
        ]

        self.assertEqual(expected, self._get(code))

    def test_target_absolute(self):
        for code in [
            "</some/path/here>",
            "</some/another_one/here>",
            "</some/path/here.property_name>",
        ]:
            self.assertEqual(
                [(token.Name.Namespace, code), (token.Text, "\n")], self._get(code),
            )

    def test_target_relative(self):
        for code in [
            "<../some/path/here>",
            "<../some/another_one/here>",
            "<../some/path/here.property_name>",
        ]:
            self.assertEqual(
                [(token.Name.Namespace, code), (token.Text, "\n")], self._get(code),
            )

    def test_attribute(self):
        normal = "double foo = 8.0"

        self.assertEqual(
            [
                (token.Token.Keyword.Type, "double"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Name.Attribute, "foo"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Operator, "="),
                (token.Token.Text, " "),
                (token.Token.Literal.Number, "8.0"),
                (token.Token.Text, "\n"),
            ],
            self._get(normal),
        )

        custom = "custom double foo = 8.0"

        self.assertEqual(
            [
                (token.Token.Keyword.Token, "custom"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Keyword.Type, "double"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Name.Attribute, "foo"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Operator, "="),
                (token.Token.Text, " "),
                (token.Token.Literal.Number, "8.0"),
                (token.Token.Text, "\n"),
            ],
            self._get(custom),
        )

        uniform = "uniform double foo = 8.0"

        self.assertEqual(
            [
                (token.Token.Keyword.Token, "uniform"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Keyword.Type, "double"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Name.Attribute, "foo"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Operator, "="),
                (token.Token.Text, " "),
                (token.Token.Literal.Number, "8.0"),
                (token.Token.Text, "\n"),
            ],
            self._get(uniform),
        )

        custom_uniform = "custom uniform double foo = 8.0"

        self.assertEqual(
            [
                (token.Token.Keyword.Token, "custom"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Keyword.Token, "uniform"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Keyword.Type, "double"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Name.Attribute, "foo"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Operator, "="),
                (token.Token.Text, " "),
                (token.Token.Literal.Number, "8.0"),
                (token.Token.Text, "\n"),
            ],
            self._get(custom_uniform),
        )

        underscore = "custom double foo_underscore_name = 8.0"

        self.assertEqual(
            [
                (token.Token.Keyword.Token, "custom"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Keyword.Type, "double"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Name.Attribute, "foo_underscore_name"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Operator, "="),
                (token.Token.Text, " "),
                (token.Token.Literal.Number, "8.0"),
                (token.Token.Text, "\n"),
            ],
            self._get(underscore),
        )

        array = "double[] foo_underscore_name = [10.1, 12.0, 13]"

        self.assertEqual(
            [
                (token.Token.Keyword.Type, "double[]"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Name.Attribute, "foo_underscore_name"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Operator, "="),
                (token.Token.Text, " "),
                (token.Token.Punctuation, "["),
                (token.Token.Literal.Number, "10.1"),
                (token.Token.Generic, ","),
                (token.Token.Text, " "),
                (token.Token.Literal.Number, "12.0"),
                (token.Token.Generic, ","),
                (token.Token.Text, " "),
                (token.Token.Literal.Number, "13"),
                (token.Token.Punctuation, "]"),
                (token.Token.Text, "\n"),
            ],
            self._get(array),
        )

        namespaced = "double[] primvar:foo_thing = [10.1, 12.0, 13]"

        self.assertEqual(
            [
                (token.Token.Keyword.Type, "double[]"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Name.Attribute, "primvar:foo_thing"),
                (token.Token.Text.Whitespace, " "),
                (token.Token.Operator, "="),
                (token.Token.Text, " "),
                (token.Token.Punctuation, "["),
                (token.Token.Literal.Number, "10.1"),
                (token.Token.Generic, ","),
                (token.Token.Text, " "),
                (token.Token.Literal.Number, "12.0"),
                (token.Token.Generic, ","),
                (token.Token.Text, " "),
                (token.Token.Literal.Number, "13"),
                (token.Token.Punctuation, "]"),
                (token.Token.Text, "\n"),
            ],
            self._get(namespaced),
        )

        timesamples = textwrap.dedent(
            """\
            custom int[] foo = [8, 10, 14]
            custom int[] foo.timeSamples = {
                1: [8, 0, 14],
                2: [-8, 0, 14],
            }
            """
        )

        self.assertEqual(
            [
                (token.Token.Keyword.Token, u"custom"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Keyword.Type, u"int[]"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Name.Attribute, u"foo"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Operator, u"="),
                (token.Token.Text, u" "),
                (token.Token.Punctuation, u"["),
                (token.Token.Literal.Number, u"8"),
                (token.Token.Generic, u","),
                (token.Token.Text, u" "),
                (token.Token.Literal.Number, u"10"),
                (token.Token.Generic, u","),
                (token.Token.Text, u" "),
                (token.Token.Literal.Number, u"14"),
                (token.Token.Punctuation, u"]"),
                (token.Token.Text, u"\n"),
                (token.Token.Keyword.Token, u"custom"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Keyword.Type, u"int[]"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Name.Attribute, u"foo"),
                (token.Generic, u"."),
                (token.Name.Keyword.Tokens, u"timeSamples"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Operator, u"="),
                (token.Token.Text, u" "),
                (token.Token.Punctuation, u"{"),
                (token.Token.Text, u"\n    "),
                (token.Token.Literal.Number, u"1"),
                (token.Token.Generic, u":"),
                (token.Token.Text, u" "),
                (token.Token.Punctuation, u"["),
                (token.Token.Literal.Number, u"8"),
                (token.Token.Generic, u","),
                (token.Token.Text, u" "),
                (token.Token.Literal.Number, u"0"),
                (token.Token.Generic, u","),
                (token.Token.Text, u" "),
                (token.Token.Literal.Number, u"14"),
                (token.Token.Punctuation, u"]"),
                (token.Token.Generic, u","),
                (token.Token.Text, u"\n    "),
                (token.Token.Literal.Number, u"2"),
                (token.Token.Generic, u":"),
                (token.Token.Text, u" "),
                (token.Token.Punctuation, u"["),
                (token.Token.Literal.Number, u"-8"),
                (token.Token.Generic, u","),
                (token.Token.Text, u" "),
                (token.Token.Literal.Number, u"0"),
                (token.Token.Generic, u","),
                (token.Token.Text, u" "),
                (token.Token.Literal.Number, u"14"),
                (token.Token.Punctuation, u"]"),
                (token.Token.Generic, u","),
                (token.Token.Text, u"\n"),
                (token.Token.Punctuation, u"}"),
                (token.Token.Text, u"\n"),
            ],
            self._get(timesamples),
        )

    def test_string_priority(self):
        code = textwrap.dedent(
            '''\
            """
            custom int[] foo = [8, 10, 14]
            """'''
        )

        self.assertEqual(
            [
                (token.Token.Literal.String, u'"""\ncustom int[] foo = [8, 10, 14]\n"""'),
                (token.Token.Text, u'\n'),
            ],
            self._get(code),
        )

    def test_numbers(self):
        code = "8 8.0123312132, -4 -14.123"

        self.assertEqual(
            [
                (token.Token.Literal.Number, u"8"),
                (token.Token.Text, u" "),
                (token.Token.Literal.Number, u"8.0123312132"),
                (token.Token.Generic, u","),
                (token.Token.Text, u" "),
                (token.Token.Literal.Number, u"-4"),
                (token.Token.Text, u" "),
                (token.Token.Literal.Number, u"-14.123"),
                (token.Token.Text, u"\n"),
            ],
            self._get(code),
        )

    def test_composition_arcs(self):
        code = textwrap.dedent(
            """
            def Xform "BottleMedical" (
                kind = "prop"
                payload = @./BottleMedical_payload.usd@</BottleMedical>
                variants = {
                    string modelingVariant = "LiquidBottleLg"
                    string shadingComplexity = "full"
                }
                add variantSets = ["modelingVariant", "shadingComplexity"]
            )
            {
                variantSet "modelingVariant" = {
                    "ALL_VARIANTS" {
                    }
                }
            }
            """
        )

        self.assertEqual(
            [
                (token.Token.Keyword.Tokens, u"def"),
                (token.Token.Text, u" "),
                (token.Token.Generic, u"Xform"),
                (token.Token.Text, u" "),
                (token.Token.Literal.String, u'"BottleMedical"'),
                (token.Token.Text, u" "),
                (token.Token.Punctuation, u"("),
                (token.Token.Text, u"\n    "),
                (token.Token.Name.Attribute, u"kind"),
                (token.Token.Text, u" "),
                (token.Token.Operator, u"="),
                (token.Token.Text, u" "),
                (token.Token.Literal.String, u'"prop"'),
                (token.Token.Text, u"\n    "),
                (token.Token.Keyword.Tokens, u"payload"),
                (token.Token.Text, u" "),
                (token.Token.Operator, u"="),
                (token.Token.Text, u" "),
                (token.Token.Literal.String.Interpol, u"@./BottleMedical_payload.usd@"),
                (token.Token.Name.Namespace, u"</BottleMedical>"),
                (token.Token.Text, u"\n    "),
                (token.Token.Keyword.Tokens, u"variants"),
                (token.Token.Text, u" "),
                (token.Token.Operator, u"="),
                (token.Token.Text, u" "),
                (token.Token.Punctuation, u"{"),
                (token.Token.Text, u"\n        "),
                (token.Token.Keyword.Type, u"string"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Name.Attribute, u"modelingVariant"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Operator, u"="),
                (token.Token.Text, u" "),
                (token.Token.Literal.String, u'"LiquidBottleLg"'),
                (token.Token.Text, u"\n        "),
                (token.Token.Keyword.Type, u"string"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Name.Attribute, u"shadingComplexity"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Operator, u"="),
                (token.Token.Text, u" "),
                (token.Token.Literal.String, u'"full"'),
                (token.Token.Text, u"\n    "),
                (token.Token.Punctuation, u"}"),
                (token.Token.Text, u"\n    "),
                (token.Token.Keyword.Type, u"add"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Name.Attribute, u"variantSets"),
                (token.Token.Text.Whitespace, u" "),
                (token.Token.Operator, u"="),
                (token.Token.Text, u" "),
                (token.Token.Punctuation, u"["),
                (token.Token.Literal.String, u'"modelingVariant", "shadingComplexity"'),
                (token.Token.Punctuation, u"]"),
                (token.Token.Text, u"\n"),
                (token.Token.Punctuation, u")"),
                (token.Token.Text, u"\n"),
                (token.Token.Punctuation, u"{"),
                (token.Token.Text, u"\n    "),
                (token.Token.Keyword.Tokens, u"variantSet"),
                (token.Token.Text, u" "),
                (token.Token.Literal.String, u'"modelingVariant"'),
                (token.Token.Text, u" "),
                (token.Token.Operator, u"="),
                (token.Token.Text, u" "),
                (token.Token.Punctuation, u"{"),
                (token.Token.Text, u"\n        "),
                (token.Token.Literal.String, u'"ALL_VARIANTS"'),
                (token.Token.Text, u" "),
                (token.Token.Punctuation, u"{"),
                (token.Token.Text, u"\n        "),
                (token.Token.Punctuation, u"}"),
                (token.Token.Text, u"\n    "),
                (token.Token.Punctuation, u"}"),
                (token.Token.Text, u"\n"),
                (token.Token.Punctuation, u"}"),
                (token.Token.Text, u"\n"),
            ],
            self._get(code),
        )

    def test_string_single_line(self):
        code = '"Some \'text"'

        self.assertEqual(
            [
                (token.Token.Literal.String, code),
                (token.Token.Text, u'\n')
            ],
            self._get(code),
        )

    def test_string_multiple_line(self):
        code1 = textwrap.dedent(
            u'''\
            """
            Some text multiline
            """'''
        )

        self.assertEqual(
            [
                (token.Token.Literal.String, code1),
                (token.Text, '\n'),
            ],
            self._get(code1),
        )

        code2 = textwrap.dedent(
            u'''\
            """Some text multiline
            """'''
        )

        self.assertEqual(
            [
                (token.Token.Literal.String, code2),
                (token.Text, '\n'),
            ],
            self._get(code2),
        )

        code3 = textwrap.dedent(
            u'''\
            """
            Some text multiline"""'''
        )

        self.assertEqual(
            [
                (token.Token.Literal.String, code3),
                (token.Text, '\n'),
            ],
            self._get(code3),
        )
