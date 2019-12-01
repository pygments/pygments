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
    #
    # def test_namespace_attribute(self):
    #     raise NotImplementedError()
    #
    # def test_numbers(self):
    #     raise NotImplementedError()
    #
    # def test_variant_set(self):
    #     raise NotImplementedError()
    #
    # def test_metadata(self):
    #     raise NotImplementedError()
    #
    # def test_string_single_line(self):
    #     raise NotImplementedError()
    #
    # def test_string_multiple_line(self):
    #     raise NotImplementedError()
