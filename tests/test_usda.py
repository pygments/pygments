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

    # def test_attribute(self):
    #     normal = 'double foo = 8.0'
    #
    #     custom = 'custom double foo = 8.0'
    #     underscore = 'custom double foo_asfd = 8.0'
    #     underscore_no_custom = 'double foo_asfd = 8.0'
    #     array = 'double[] foo_asfd = [10.1, 12.0, 13]'
    #     namespaced = 'double[] primvar:foo_thing = [10.1, 12.0, 13]'
    #
    #     timeSamples = textwrap.dedent(
    #         '''\
    #         custom int[] foo = [8, 10, 14]
    #         custom int[] foo.timeSamples = {
    #             1: [8, 0, 14],
    #             2: [-8, 0, 14],
    #         }
    #         '''
    #     )
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
