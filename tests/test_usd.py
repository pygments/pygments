#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Test that syntax highlighting for USD files works correctly."""

import textwrap
import unittest

from pygments import lexers, token


class _Common(unittest.TestCase):
    """A basic class that makes it easier to write unittests."""

    def setUp(self):
        """Create a fresh USD lexer class before each test runs."""
        self.lexer = lexers.UsdLexer()

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
                (token.String.Interpol, path),
                (token.Text, "\n"),
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
                [(token.Name.Namespace, code), (token.Text, "\n")], self._get(code),
            )

    def test_attribute(self):
        """Test different attribute syntax styles."""
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
        """Make sure that no other rules override a string match."""
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
        """Check that different number representations work."""
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
                (token.Token.Keyword.Tokens, u"def"),
                (token.Token.Text, u" "),
                (token.Token.Generic, u"Xform"),
                (token.Token.Text, u" "),
                (token.Token.Literal.String, u'"BottleMedical"'),
                (token.Token.Text, u" "),
                (token.Token.Punctuation, u"("),
                (token.Token.Text, u"\n    "),
                (token.Token.Name.Builtins, u"kind"),
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
                (token.Token.Literal.String, u'"modelingVariant"'),
                (token.Token.Generic, u','),
                (token.Token.Text, u' '),
                (token.Token.Literal.String, u'"shadingComplexity"'),
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
        """Check a single string for the correct highlight."""
        code = '"Some \'text"'

        self.assertEqual(
            [
                (token.Token.Literal.String, code),
                (token.Token.Text, u'\n')
            ],
            self._get(code),
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
                (token.Token.Keyword.Type, u'float[]'),
                (token.Token.Text.Whitespace, u' '),
                (token.Token.Name.Attribute, u'primvars:skel:jointWeights'),
                (token.Token.Text.Whitespace, u' '),
                (token.Token.Operator, u'='),
                (token.Token.Text, u' '),
                (token.Token.Punctuation, u'['),
                (token.Token.Literal.Number, u'1'),
                (token.Token.Punctuation, u']'),
                (token.Token.Text, u' '),
                (token.Token.Punctuation, u'('),
                (token.Token.Text, u'\n    '),
                (token.Token.Name.Builtins, u'elementSize'),
                (token.Token.Text, u' '),
                (token.Token.Operator, u'='),
                (token.Token.Text, u' '),
                (token.Token.Literal.Number, u'1'),
                (token.Token.Text, u'\n    '),
                (token.Token.Name.Builtins, u'interpolation'),
                (token.Token.Text, u' '),
                (token.Token.Operator, u'='),
                (token.Token.Text, u' '),
                (token.Token.Literal.String, u'"constant"'),
                (token.Token.Text, u'\n'),
                (token.Token.Punctuation, u')'),
                (token.Token.Text, u'\n'),
            ],
            self._get(code),
        )

    def test_outer_match(self):
        """Make sure that text between located between quotes and @@s are not matched."""
        at_sign = "@firststring@ something else @secondstring@"

        self.assertEqual(
            [
                (token.Token.Literal.String.Interpol, u'@firststring@'),
                (token.Token.Text, u' '),
                (token.Token.Generic, u'something'),
                (token.Token.Text, u' '),
                (token.Token.Generic, u'else'),
                (token.Token.Text, u' '),
                (token.Token.Literal.String.Interpol, u'@secondstring@'),
                (token.Token.Text, u'\n'),
            ],
            self._get(at_sign)
        )

        single = "'firststring' something else 'secondstring'"

        self.assertEqual(
            [
                (token.Token.Literal.String, u"'firststring'"),
                (token.Token.Text, u' '),
                (token.Token.Generic, u'something'),
                (token.Token.Text, u' '),
                (token.Token.Generic, u'else'),
                (token.Token.Text, u' '),
                (token.Token.Literal.String, u"'secondstring'"),
                (token.Token.Text, u'\n'),
            ],
            self._get(single)
        )

        double = "'firststring' something else 'secondstring'"

        self.assertEqual(
            [
                (token.Token.Literal.String, u"'firststring'"),
                (token.Token.Text, u' '),
                (token.Token.Generic, u'something'),
                (token.Token.Text, u' '),
                (token.Token.Generic, u'else'),
                (token.Token.Text, u' '),
                (token.Token.Literal.String, u"'secondstring'"),
                (token.Token.Text, u'\n'),
            ],
            self._get(double)
        )
