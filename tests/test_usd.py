#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Test that syntax highlighting for USD files works correctly."""

import textwrap
import unittest

from pygments.lexers import UsdLexer
from pygments.token import Keyword, Literal, Name, Number, Operator, Punctuation, \
    String, Text, Whitespace


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
                (Whitespace, "\n"),
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
                [(Name.Namespace, code), (Whitespace, "\n")], self._get(code),
            )

    def test_attribute(self):
        """Test different attribute syntax styles."""
        normal = "double foo = 8.0"

        self.assertEqual(
            [
                (Keyword.Type, "double"),
                (Whitespace, " "),
                (Name.Attribute, "foo"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Number, "8.0"),
                (Whitespace, "\n"),
            ],
            self._get(normal),
        )

        custom = "custom double foo = 8.0"

        self.assertEqual(
            [
                (Keyword.Token, "custom"),
                (Whitespace, " "),
                (Keyword.Type, "double"),
                (Whitespace, " "),
                (Name.Attribute, "foo"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Number, "8.0"),
                (Whitespace, "\n"),
            ],
            self._get(custom),
        )

        uniform = "uniform double foo = 8.0"

        self.assertEqual(
            [
                (Keyword.Token, "uniform"),
                (Whitespace, " "),
                (Keyword.Type, "double"),
                (Whitespace, " "),
                (Name.Attribute, "foo"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Number, "8.0"),
                (Whitespace, "\n"),
            ],
            self._get(uniform),
        )

        custom_uniform = "custom uniform double foo = 8.0"

        self.assertEqual(
            [
                (Keyword.Token, "custom"),
                (Whitespace, " "),
                (Keyword.Token, "uniform"),
                (Whitespace, " "),
                (Keyword.Type, "double"),
                (Whitespace, " "),
                (Name.Attribute, "foo"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Number, "8.0"),
                (Whitespace, "\n"),
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
                (Whitespace, " "),
                (Number, "8.0"),
                (Whitespace, "\n"),
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
                (Whitespace, " "),
                (Punctuation, "["),
                (Number, "10.1"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "12.0"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "13"),
                (Punctuation, "]"),
                (Whitespace, "\n"),
            ],
            self._get(array),
        )

        namespaced = "double[] primvar:foo_thing = [10.1, 12.0, 13]"

        self.assertEqual(
            [
                (Keyword.Type, "double[]"),
                (Whitespace, " "),
                (Name.Attribute, "primvar:foo_thing"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Punctuation, "["),
                (Number, "10.1"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "12.0"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "13"),
                (Punctuation, "]"),
                (Whitespace, "\n"),
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
                (Whitespace, u" "),
                (Keyword.Type, u"int[]"),
                (Whitespace, u" "),
                (Name.Attribute, u"foo"),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (Punctuation, u"["),
                (Number, u"8"),
                (Punctuation, u","),
                (Whitespace, u" "),
                (Number, u"10"),
                (Punctuation, u","),
                (Whitespace, u" "),
                (Number, u"14"),
                (Punctuation, u"]"),
                (Whitespace, u"\n"),
                (Keyword.Token, u"custom"),
                (Whitespace, u" "),
                (Keyword.Type, u"int[]"),
                (Whitespace, u" "),
                (Name.Attribute, u"foo"),
                (Text, u"."),
                (Name.Keyword.Tokens, u"timeSamples"),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (Punctuation, u"{"),
                (Whitespace, u"\n    "),
                (Number, u"1"),
                (Punctuation, u":"),
                (Whitespace, u" "),
                (Punctuation, u"["),
                (Number, u"8"),
                (Punctuation, u","),
                (Whitespace, u" "),
                (Number, u"0"),
                (Punctuation, u","),
                (Whitespace, u" "),
                (Number, u"14"),
                (Punctuation, u"]"),
                (Punctuation, u","),
                (Whitespace, u"\n    "),
                (Number, u"2"),
                (Punctuation, u":"),
                (Whitespace, u" "),
                (Punctuation, u"["),
                (Number, u"-8"),
                (Punctuation, u","),
                (Whitespace, u" "),
                (Number, u"0"),
                (Punctuation, u","),
                (Whitespace, u" "),
                (Number, u"14"),
                (Punctuation, u"]"),
                (Punctuation, u","),
                (Whitespace, u"\n"),
                (Punctuation, u"}"),
                (Whitespace, u"\n"),
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
                (String, u'"""\ncustom int[] foo = [8, 10, 14]\n"""'),
                (Whitespace, u"\n"),
            ],
            self._get(code),
        )

    def test_numbers(self):
        """Check that different number representations work."""
        code = "8 8.0123312132, -4 -14.123 1e10 0.1e10 10.123e+10 0.123e-14"

        self.assertEqual(
            [
                (Number, u"8"),
                (Whitespace, u" "),
                (Number, u"8.0123312132"),
                (Punctuation, u","),
                (Whitespace, u" "),
                (Number, u"-4"),
                (Whitespace, u" "),
                (Number, u"-14.123"),
                (Whitespace, u" "),
                (Number, u"1e10"),
                (Whitespace, u" "),
                (Number, u"0.1e10"),
                (Whitespace, u" "),
                (Number, u"10.123e+10"),
                (Whitespace, u" "),
                (Number, u"0.123e-14"),
                (Whitespace, u"\n"),
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
                (Whitespace, u" "),
                (Text, u"Xform"),
                (Whitespace, u" "),
                (String, u'"BottleMedical"'),
                (Whitespace, u" "),
                (Punctuation, u"("),
                (Whitespace, u"\n    "),
                (Name.Builtins, u"kind"),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (String, u'"prop"'),
                (Whitespace, u"\n    "),
                (Keyword.Tokens, u"payload"),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (String.Interpol, u"@./BottleMedical_payload.usd@"),
                (Name.Namespace, u"</BottleMedical>"),
                (Whitespace, u"\n    "),
                (Keyword.Tokens, u"variants"),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (Punctuation, u"{"),
                (Whitespace, u"\n        "),
                (Keyword.Type, u"string"),
                (Whitespace, u" "),
                (Name.Attribute, u"modelingVariant"),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (String, u'"LiquidBottleLg"'),
                (Whitespace, u"\n        "),
                (Keyword.Type, u"string"),
                (Whitespace, u" "),
                (Name.Attribute, u"shadingComplexity"),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (String, u'"full"'),
                (Whitespace, u"\n    "),
                (Punctuation, u"}"),
                (Whitespace, u"\n    "),
                (Keyword.Type, u"add"),
                (Text.Whitespace, u" "),
                (Name.Attribute, u"variantSets"),
                (Text.Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (Punctuation, u"["),
                (String, u'"modelingVariant"'),
                (Punctuation, u","),
                (Whitespace, u" "),
                (String, u'"shadingComplexity"'),
                (Punctuation, u"]"),
                (Whitespace, u"\n"),
                (Punctuation, u")"),
                (Whitespace, u"\n"),
                (Punctuation, u"{"),
                (Whitespace, u"\n    "),
                (Keyword.Tokens, u"variantSet"),
                (Whitespace, u" "),
                (String, u'"modelingVariant"'),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (Punctuation, u"{"),
                (Whitespace, u"\n        "),
                (String, u'"ALL_VARIANTS"'),
                (Whitespace, u" "),
                (Punctuation, u"{"),
                (Whitespace, u"\n        "),
                (Punctuation, u"}"),
                (Whitespace, u"\n    "),
                (Punctuation, u"}"),
                (Whitespace, u"\n"),
                (Punctuation, u"}"),
                (Whitespace, u"\n"),
            ],
            self._get(code),
        )

    def test_string_single_line(self):
        """Check a single string for the correct highlight."""
        code = '"Some \'text"'

        self.assertEqual(
            [(String, code), (Whitespace, u"\n")], self._get(code),
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
            [(String, code1), (Whitespace, "\n"),], self._get(code1),
        )

        code2 = textwrap.dedent(
            u'''\
            """Some text multiline
            """'''
        )

        self.assertEqual(
            [(String, code2), (Whitespace, "\n"),], self._get(code2),
        )

        code3 = textwrap.dedent(
            u'''\
            """
            Some text multiline"""'''
        )

        self.assertEqual(
            [(String, code3), (Whitespace, "\n"),], self._get(code3),
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
                (Whitespace, u" "),
                (Name.Attribute, u"primvars:skel:jointWeights"),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (Punctuation, u"["),
                (Number, u"1"),
                (Punctuation, u"]"),
                (Whitespace, u" "),
                (Punctuation, u"("),
                (Whitespace, u"\n    "),
                (Name.Builtins, u"elementSize"),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (Number, u"1"),
                (Whitespace, u"\n    "),
                (Name.Builtins, u"interpolation"),
                (Whitespace, u" "),
                (Operator, u"="),
                (Whitespace, u" "),
                (String, u'"constant"'),
                (Whitespace, u"\n"),
                (Punctuation, u")"),
                (Whitespace, u"\n"),
            ],
            self._get(code),
        )

    def test_outer_match(self):
        """Make sure that text between located between quotes and @@s are not matched."""
        at_sign = "@firststring@ something else @secondstring@"

        self.assertEqual(
            [
                (String.Interpol, u"@firststring@"),
                (Whitespace, u" "),
                (Text, u"something"),
                (Whitespace, u" "),
                (Text, u"else"),
                (Whitespace, u" "),
                (String.Interpol, u"@secondstring@"),
                (Whitespace, u"\n"),
            ],
            self._get(at_sign),
        )

        single = "'firststring' something else 'secondstring'"

        self.assertEqual(
            [
                (String, u"'firststring'"),
                (Whitespace, u" "),
                (Text, u"something"),
                (Whitespace, u" "),
                (Text, u"else"),
                (Whitespace, u" "),
                (String, u"'secondstring'"),
                (Whitespace, u"\n"),
            ],
            self._get(single),
        )

        double = "'firststring' something else 'secondstring'"

        self.assertEqual(
            [
                (String, u"'firststring'"),
                (Whitespace, u" "),
                (Text, u"something"),
                (Whitespace, u" "),
                (Text, u"else"),
                (Whitespace, u" "),
                (String, u"'secondstring'"),
                (Whitespace, u"\n"),
            ],
            self._get(double),
        )
