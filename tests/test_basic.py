# -*- coding: utf-8 -*-
"""
    Pygments Basic lexers tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2016 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import unittest

from pygments.lexers.basic import VBScriptLexer
from pygments.token import Error, Name, Number, Punctuation, String, Whitespace


class VBScriptLexerTest(unittest.TestCase):

    def setUp(self):
        self.lexer = VBScriptLexer()

    def _assert_are_tokens_of_type(self, examples, expected_token_type):
        for test_number, example in enumerate(examples.split(), 1):
            token_count = 0
            for token_type, token_value in self.lexer.get_tokens(example):
                if token_type != Whitespace:
                    token_count += 1
                    self.assertEqual(
                        token_type, expected_token_type,
                        'token_type #%d for %s is be %s but must be %s' %
                        (test_number, token_value, token_type, expected_token_type))
            self.assertEqual(
                token_count, 1,
                '%s must yield exactly 1 token instead of %d' %
                (example, token_count))

    def _assert_tokens_match(self, text, expected_tokens_without_trailing_newline):
        actual_tokens = tuple(self.lexer.get_tokens(text))
        if (len(actual_tokens) >= 1) and (actual_tokens[-1] == (Whitespace, '\n')):
            actual_tokens = tuple(actual_tokens[:-1])
        self.assertEqual(
            expected_tokens_without_trailing_newline, actual_tokens,
            'text must yield expected tokens: %s' % text)

    def test_can_lex_float(self):
        self._assert_are_tokens_of_type(
            '1. 1.e1 .1 1.2 1.2e3 1.2e+3 1.2e-3 1e2', Number.Float)
        self._assert_tokens_match(
            '1e2.1e2',
            ((Number.Float, '1e2'), (Number.Float, '.1e2'))
        )

    def test_can_reject_almost_float(self):
        self._assert_tokens_match(
            '.e1',
            ((Punctuation, '.'), (Name, 'e1')))

    def test_can_lex_integer(self):
        self._assert_are_tokens_of_type(
            '1 23 456', Number.Integer)

    def test_can_lex_names(self):
        self._assert_are_tokens_of_type(
            u'thingy thingy123 _thingy _123', Name)

    def test_can_recover_after_unterminated_string(self):
        self._assert_tokens_match(
            '"x\nx',
            ((String.Double, '"'), (String.Double, 'x'), (Error, '\n'), (Name, 'x'))
        )

    def test_can_recover_from_invalid_character(self):
        self._assert_tokens_match(
            'a;bc\nd',
            ((Name, 'a'), (Error, ';bc\n'), (Name, 'd'))
        )
