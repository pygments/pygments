#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Test that syntax highlighting for USD files works correctly."""

import textwrap
import unittest

from pygments import UsdLexer
from pygments.token import Keyword, Literal, Name, Operator, Punctuation, \
    String, Text


class _Common(unittest.TestCase):
    """A basic class that makes it easier to write unittests."""

    def setUp(self):
        """Create a fresh USD lexer class before each test runs."""
        self.lexer = UsdLexer()

    def _get(self, code):
        """Tokenize the code into its unique parts.

        :param code: The USD source code to split up.
        :type code: str

        :returns: The tokenized pieces.
        :rtype: list[:class:`pygments.token._TokenType`]

        """
        return list(self.lexer.get_tokens(code))


class Features(_Common):
    """Test that different features of USD highlight as expected."""

    def test_asset_path(self):
        """Check that a regular file path highlights correctly."""
        for path in [
            "@./some/path/to/a/file/foo.usda@",
            "@/some/path/to/a/file/foo.usda@",
            "@some/path/to/a/file/foo.usda@",
            r"@file://SPECI__Z-_ALIZED(syntax_here)?with_arbitrary#)(%*&)\characters.tar.gz@",
        ]:
            expected = [
                (String.Interpol, path),
                (Text, "\n"),
            ]

            self.assertEqual(expected, self._get(path))

    def test_target_absolute(self):
        """Check that SdfPath syntax examples work correctly."""
        for code in [
            # Absolute paths
            "</some/another_one/here>",
            "</some/path/here.property_name>",
            "</some/path/here>",
            # Relative paths
            "<../some/another_one/here>",
            "<../some/path/here.property_name>",
            "<../some/path/here>",
        ]:
            self.assertEqual(
                [(Name.Namespace, code), (Text, "\n")], self._get(code),
            )

    def test_attribute(self):
        """Test different attribute syntax styles."""
        normal = "double foo = 8.0"

        self.assertEqual(
            [
                (Keyword.Type, "double"),
                (Text.Whitespace, " "),
                (Name.Attribute, "foo"),
                (Text.Whitespace, " "),
                (Operator, "="),
                (Text, " "),
                (Literal.Number, "8.0"),
                (Text, "\n"),
            ],
            self._get(normal),
        )

        custom = "custom double foo = 8.0"

        self.assertEqual(
            [
                (Keyword.Token, "custom"),
                (Text.Whitespace, " "),
                (Keyword.Type, "double"),
                (Text.Whitespace, " "),
                (Name.Attribute, "foo"),
                (Text.Whitespace, " "),
                (Operator, "="),
                (Text, " "),
                (Literal.Number, "8.0"),
                (Text, "\n"),
            ],
            self._get(custom),
        )

        uniform = "uniform double foo = 8.0"

        self.assertEqual(
            [
                (Keyword.Token, "uniform"),
                (Text.Whitespace, " "),
                (Keyword.Type, "double"),
                (Text.Whitespace, " "),
                (Name.Attribute, "foo"),
                (Text.Whitespace, " "),
                (Operator, "="),
                (Text, " "),
                (Literal.Number, "8.0"),
                (Text, "\n"),
            ],
            self._get(uniform),
        )

        custom_uniform = "custom uniform double foo = 8.0"

        self.assertEqual(
            [
                (Keyword.Token, "custom"),
                (Text.Whitespace, " "),
                (Keyword.Token, "uniform"),
                (Text.Whitespace, " "),
                (Keyword.Type, "double"),
                (Text.Whitespace, " "),
                (Name.Attribute, "foo"),
                (Text.Whitespace, " "),
                (Operator, "="),
                (Text, " "),
                (Literal.Number, "8.0"),
                (Text, "\n"),
            ],
            self._get(custom_uniform),
        )

        underscore = "custom double foo_underscore_name = 8.0"

        self.assertEqual(
            [
                (Keyword.Token, "custom"),
                (Text.Whitespace, " "),
                (Keyword.Type, "double"),
                (Text.Whitespace, " "),
                (Name.Attribute, "foo_underscore_name"),
                (Text.Whitespace, " "),
                (Operator, "="),
                (Text, " "),
                (Literal.Number, "8.0"),
                (Text, "\n"),
            ],
            self._get(underscore),
        )

        array = "double[] foo_underscore_name = [10.1, 12.0, 13]"

        self.assertEqual(
            [
                (Keyword.Type, "double[]"),
                (Text.Whitespace, " "),
                (Name.Attribute, "foo_underscore_name"),
                (Text.Whitespace, " "),
                (Operator, "="),
                (Text, " "),
                (Punctuation, "["),
                (Literal.Number, "10.1"),
                (Generic, ","),
                (Text, " "),
                (Literal.Number, "12.0"),
                (Generic, ","),
                (Text, " "),
                (Literal.Number, "13"),
                (Punctuation, "]"),
                (Text, "\n"),
            ],
            self._get(array),
        )

        namespaced = "double[] primvar:foo_thing = [10.1, 12.0, 13]"

        self.assertEqual(
            [
                (Keyword.Type, "double[]"),
                (Text.Whitespace, " "),
                (Name.Attribute, "primvar:foo_thing"),
                (Text.Whitespace, " "),
                (Operator, "="),
                (Text, " "),
                (Punctuation, "["),
                (Literal.Number, "10.1"),
                (Generic, ","),
                (Text, " "),
                (Literal.Number, "12.0"),
                (Generic, ","),
                (Text, " "),
                (Literal.Number, "13"),
                (Punctuation, "]"),
                (Text, "\n"),
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
                (Keyword.Token, u"custom"),
                (Text.Whitespace, u" "),
                (Keyword.Type, u"int[]"),
                (Text.Whitespace, u" "),
                (Name.Attribute, u"foo"),
                (Text.Whitespace, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Punctuation, u"["),
                (Literal.Number, u"8"),
                (Generic, u","),
                (Text, u" "),
                (Literal.Number, u"10"),
                (Generic, u","),
                (Text, u" "),
                (Literal.Number, u"14"),
                (Punctuation, u"]"),
                (Text, u"\n"),
                (Keyword.Token, u"custom"),
                (Text.Whitespace, u" "),
                (Keyword.Type, u"int[]"),
                (Text.Whitespace, u" "),
                (Name.Attribute, u"foo"),
                (Generic, u"."),
                (Name.Keyword.Tokens, u"timeSamples"),
                (Text.Whitespace, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Punctuation, u"{"),
                (Text, u"\n    "),
                (Literal.Number, u"1"),
                (Generic, u":"),
                (Text, u" "),
                (Punctuation, u"["),
                (Literal.Number, u"8"),
                (Generic, u","),
                (Text, u" "),
                (Literal.Number, u"0"),
                (Generic, u","),
                (Text, u" "),
                (Literal.Number, u"14"),
                (Punctuation, u"]"),
                (Generic, u","),
                (Text, u"\n    "),
                (Literal.Number, u"2"),
                (Generic, u":"),
                (Text, u" "),
                (Punctuation, u"["),
                (Literal.Number, u"-8"),
                (Generic, u","),
                (Text, u" "),
                (Literal.Number, u"0"),
                (Generic, u","),
                (Text, u" "),
                (Literal.Number, u"14"),
                (Punctuation, u"]"),
                (Generic, u","),
                (Text, u"\n"),
                (Punctuation, u"}"),
                (Text, u"\n"),
            ],
            self._get(timesamples),
        )

    def test_string_priority(self):
        """Make sure that no other rules override a string match."""
        code = textwrap.dedent(
            '''\
            """
            custom int[] foo = [8, 10, 14]
            """'''
        )

        self.assertEqual(
            [
                (Literal.String, u'"""\ncustom int[] foo = [8, 10, 14]\n"""'),
                (Text, u"\n"),
            ],
            self._get(code),
        )

    def test_numbers(self):
        """Check that different number representations work."""
        code = "8 8.0123312132, -4 -14.123"

        self.assertEqual(
            [
                (Literal.Number, u"8"),
                (Text, u" "),
                (Literal.Number, u"8.0123312132"),
                (Generic, u","),
                (Text, u" "),
                (Literal.Number, u"-4"),
                (Text, u" "),
                (Literal.Number, u"-14.123"),
                (Text, u"\n"),
            ],
            self._get(code),
        )

    def test_composition_arcs(self):
        """Test composition arc syntax highlighting."""
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
                (Keyword.Tokens, u"def"),
                (Text, u" "),
                (Generic, u"Xform"),
                (Text, u" "),
                (Literal.String, u'"BottleMedical"'),
                (Text, u" "),
                (Punctuation, u"("),
                (Text, u"\n    "),
                (Name.Builtins, u"kind"),
                (Text, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Literal.String, u'"prop"'),
                (Text, u"\n    "),
                (Keyword.Tokens, u"payload"),
                (Text, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Literal.String.Interpol, u"@./BottleMedical_payload.usd@"),
                (Name.Namespace, u"</BottleMedical>"),
                (Text, u"\n    "),
                (Keyword.Tokens, u"variants"),
                (Text, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Punctuation, u"{"),
                (Text, u"\n        "),
                (Keyword.Type, u"string"),
                (Text.Whitespace, u" "),
                (Name.Attribute, u"modelingVariant"),
                (Text.Whitespace, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Literal.String, u'"LiquidBottleLg"'),
                (Text, u"\n        "),
                (Keyword.Type, u"string"),
                (Text.Whitespace, u" "),
                (Name.Attribute, u"shadingComplexity"),
                (Text.Whitespace, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Literal.String, u'"full"'),
                (Text, u"\n    "),
                (Punctuation, u"}"),
                (Text, u"\n    "),
                (Keyword.Type, u"add"),
                (Text.Whitespace, u" "),
                (Name.Attribute, u"variantSets"),
                (Text.Whitespace, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Punctuation, u"["),
                (Literal.String, u'"modelingVariant"'),
                (Generic, u","),
                (Text, u" "),
                (Literal.String, u'"shadingComplexity"'),
                (Punctuation, u"]"),
                (Text, u"\n"),
                (Punctuation, u")"),
                (Text, u"\n"),
                (Punctuation, u"{"),
                (Text, u"\n    "),
                (Keyword.Tokens, u"variantSet"),
                (Text, u" "),
                (Literal.String, u'"modelingVariant"'),
                (Text, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Punctuation, u"{"),
                (Text, u"\n        "),
                (Literal.String, u'"ALL_VARIANTS"'),
                (Text, u" "),
                (Punctuation, u"{"),
                (Text, u"\n        "),
                (Punctuation, u"}"),
                (Text, u"\n    "),
                (Punctuation, u"}"),
                (Text, u"\n"),
                (Punctuation, u"}"),
                (Text, u"\n"),
            ],
            self._get(code),
        )

    def test_string_single_line(self):
        """Check a single string for the correct highlight."""
        code = '"Some \'text"'

        self.assertEqual(
            [(Literal.String, code), (Text, u"\n")], self._get(code),
        )

    def test_string_multiple_line(self):
        """Check that different multi-line strings work correctly."""
        code1 = textwrap.dedent(
            u'''\
            """
            Some text multiline
            """'''
        )

        self.assertEqual(
            [(Literal.String, code1), (Text, "\n"),], self._get(code1),
        )

        code2 = textwrap.dedent(
            u'''\
            """Some text multiline
            """'''
        )

        self.assertEqual(
            [(Literal.String, code2), (Text, "\n"),], self._get(code2),
        )

        code3 = textwrap.dedent(
            u'''\
            """
            Some text multiline"""'''
        )

        self.assertEqual(
            [(Literal.String, code3), (Text, "\n"),], self._get(code3),
        )


class EdgeCases(_Common):
    """Any bugs / weird cases that deserve special attention."""

    def test_metadata(self):
        """Make sure metadata [the stuff inside ()s] don't match as Attributes."""
        code = textwrap.dedent(
            """
            float[] primvars:skel:jointWeights = [1] (
                elementSize = 1
                interpolation = "constant"
            )
            """
        )

        self.assertEqual(
            [
                (Keyword.Type, u"float[]"),
                (Text.Whitespace, u" "),
                (Name.Attribute, u"primvars:skel:jointWeights"),
                (Text.Whitespace, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Punctuation, u"["),
                (Literal.Number, u"1"),
                (Punctuation, u"]"),
                (Text, u" "),
                (Punctuation, u"("),
                (Text, u"\n    "),
                (Name.Builtins, u"elementSize"),
                (Text, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Literal.Number, u"1"),
                (Text, u"\n    "),
                (Name.Builtins, u"interpolation"),
                (Text, u" "),
                (Operator, u"="),
                (Text, u" "),
                (Literal.String, u'"constant"'),
                (Text, u"\n"),
                (Punctuation, u")"),
                (Text, u"\n"),
            ],
            self._get(code),
        )

    def test_outer_match(self):
        """Make sure that text between located between quotes and @@s are not matched."""
        at_sign = "@firststring@ something else @secondstring@"

        self.assertEqual(
            [
                (Literal.String.Interpol, u"@firststring@"),
                (Text, u" "),
                (Generic, u"something"),
                (Text, u" "),
                (Generic, u"else"),
                (Text, u" "),
                (Literal.String.Interpol, u"@secondstring@"),
                (Text, u"\n"),
            ],
            self._get(at_sign),
        )

        single = "'firststring' something else 'secondstring'"

        self.assertEqual(
            [
                (Literal.String, u"'firststring'"),
                (Text, u" "),
                (Generic, u"something"),
                (Text, u" "),
                (Generic, u"else"),
                (Text, u" "),
                (Literal.String, u"'secondstring'"),
                (Text, u"\n"),
            ],
            self._get(single),
        )

        double = "'firststring' something else 'secondstring'"

        self.assertEqual(
            [
                (Literal.String, u"'firststring'"),
                (Text, u" "),
                (Generic, u"something"),
                (Text, u" "),
                (Generic, u"else"),
                (Text, u" "),
                (Literal.String, u"'secondstring'"),
                (Text, u"\n"),
            ],
            self._get(double),
        )
