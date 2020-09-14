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
                (Keyword.Token, "custom"),
                (Whitespace, " "),
                (Keyword.Type, "int[]"),
                (Whitespace, " "),
                (Name.Attribute, "foo"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Punctuation, "["),
                (Number, "8"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "10"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "14"),
                (Punctuation, "]"),
                (Whitespace, "\n"),
                (Keyword.Token, "custom"),
                (Whitespace, " "),
                (Keyword.Type, "int[]"),
                (Whitespace, " "),
                (Name.Attribute, "foo"),
                (Text, "."),
                (Name.Keyword.Tokens, "timeSamples"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Punctuation, "{"),
                (Whitespace, "\n    "),
                (Number, "1"),
                (Punctuation, ":"),
                (Whitespace, " "),
                (Punctuation, "["),
                (Number, "8"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "0"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "14"),
                (Punctuation, "]"),
                (Punctuation, ","),
                (Whitespace, "\n    "),
                (Number, "2"),
                (Punctuation, ":"),
                (Whitespace, " "),
                (Punctuation, "["),
                (Number, "-8"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "0"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "14"),
                (Punctuation, "]"),
                (Punctuation, ","),
                (Whitespace, "\n"),
                (Punctuation, "}"),
                (Whitespace, "\n"),
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
                (String, '"""\ncustom int[] foo = [8, 10, 14]\n"""'),
                (Whitespace, "\n"),
            ],
            self._get(code),
        )

    def test_numbers(self):
        """Check that different number representations work."""
        code = "8 8.0123312132, -4 -14.123 1e10 0.1e10 10.123e+10 0.123e-14"

        self.assertEqual(
            [
                (Number, "8"),
                (Whitespace, " "),
                (Number, "8.0123312132"),
                (Punctuation, ","),
                (Whitespace, " "),
                (Number, "-4"),
                (Whitespace, " "),
                (Number, "-14.123"),
                (Whitespace, " "),
                (Number, "1e10"),
                (Whitespace, " "),
                (Number, "0.1e10"),
                (Whitespace, " "),
                (Number, "10.123e+10"),
                (Whitespace, " "),
                (Number, "0.123e-14"),
                (Whitespace, "\n"),
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
                (Keyword.Tokens, "def"),
                (Whitespace, " "),
                (Text, "Xform"),
                (Whitespace, " "),
                (String, '"BottleMedical"'),
                (Whitespace, " "),
                (Punctuation, "("),
                (Whitespace, "\n    "),
                (Name.Builtins, "kind"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (String, '"prop"'),
                (Whitespace, "\n    "),
                (Keyword.Tokens, "payload"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (String.Interpol, "@./BottleMedical_payload.usd@"),
                (Name.Namespace, "</BottleMedical>"),
                (Whitespace, "\n    "),
                (Keyword.Tokens, "variants"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Punctuation, "{"),
                (Whitespace, "\n        "),
                (Keyword.Type, "string"),
                (Whitespace, " "),
                (Name.Attribute, "modelingVariant"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (String, '"LiquidBottleLg"'),
                (Whitespace, "\n        "),
                (Keyword.Type, "string"),
                (Whitespace, " "),
                (Name.Attribute, "shadingComplexity"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (String, '"full"'),
                (Whitespace, "\n    "),
                (Punctuation, "}"),
                (Whitespace, "\n    "),
                (Keyword.Type, "add"),
                (Text.Whitespace, " "),
                (Name.Attribute, "variantSets"),
                (Text.Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Punctuation, "["),
                (String, '"modelingVariant"'),
                (Punctuation, ","),
                (Whitespace, " "),
                (String, '"shadingComplexity"'),
                (Punctuation, "]"),
                (Whitespace, "\n"),
                (Punctuation, ")"),
                (Whitespace, "\n"),
                (Punctuation, "{"),
                (Whitespace, "\n    "),
                (Keyword.Tokens, "variantSet"),
                (Whitespace, " "),
                (String, '"modelingVariant"'),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Punctuation, "{"),
                (Whitespace, "\n        "),
                (String, '"ALL_VARIANTS"'),
                (Whitespace, " "),
                (Punctuation, "{"),
                (Whitespace, "\n        "),
                (Punctuation, "}"),
                (Whitespace, "\n    "),
                (Punctuation, "}"),
                (Whitespace, "\n"),
                (Punctuation, "}"),
                (Whitespace, "\n"),
            ],
            self._get(code),
        )

    def test_string_single_line(self):
        """Check a single string for the correct highlight."""
        code = '"Some \'text"'

        self.assertEqual(
            [(String, code), (Whitespace, "\n")], self._get(code),
        )

    def test_string_multiple_line(self):
        """Check that different multi-line strings work correctly."""
        code1 = textwrap.dedent(
            '''\
            """
            Some text multiline
            """'''
        )

        self.assertEqual(
            [(String, code1), (Whitespace, "\n"),], self._get(code1),
        )

        code2 = textwrap.dedent(
            '''\
            """Some text multiline
            """'''
        )

        self.assertEqual(
            [(String, code2), (Whitespace, "\n"),], self._get(code2),
        )

        code3 = textwrap.dedent(
            '''\
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
                (Keyword.Type, "float[]"),
                (Whitespace, " "),
                (Name.Attribute, "primvars:skel:jointWeights"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Punctuation, "["),
                (Number, "1"),
                (Punctuation, "]"),
                (Whitespace, " "),
                (Punctuation, "("),
                (Whitespace, "\n    "),
                (Name.Builtins, "elementSize"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (Number, "1"),
                (Whitespace, "\n    "),
                (Name.Builtins, "interpolation"),
                (Whitespace, " "),
                (Operator, "="),
                (Whitespace, " "),
                (String, '"constant"'),
                (Whitespace, "\n"),
                (Punctuation, ")"),
                (Whitespace, "\n"),
            ],
            self._get(code),
        )

    def test_outer_match(self):
        """Make sure that text between located between quotes and @@s are not matched."""
        at_sign = "@firststring@ something else @secondstring@"

        self.assertEqual(
            [
                (String.Interpol, "@firststring@"),
                (Whitespace, " "),
                (Text, "something"),
                (Whitespace, " "),
                (Text, "else"),
                (Whitespace, " "),
                (String.Interpol, "@secondstring@"),
                (Whitespace, "\n"),
            ],
            self._get(at_sign),
        )

        single = "'firststring' something else 'secondstring'"

        self.assertEqual(
            [
                (String, "'firststring'"),
                (Whitespace, " "),
                (Text, "something"),
                (Whitespace, " "),
                (Text, "else"),
                (Whitespace, " "),
                (String, "'secondstring'"),
                (Whitespace, "\n"),
            ],
            self._get(single),
        )

        double = "'firststring' something else 'secondstring'"

        self.assertEqual(
            [
                (String, "'firststring'"),
                (Whitespace, " "),
                (Text, "something"),
                (Whitespace, " "),
                (Text, "else"),
                (Whitespace, " "),
                (String, "'secondstring'"),
                (Whitespace, "\n"),
            ],
            self._get(double),
        )
