# -*- coding: utf-8 -*-
"""
    Pygments regex lexer tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import unittest

from pygments.lexers.markup import MarkdownLexer


class SameTextTests(unittest.TestCase):

    lexer = MarkdownLexer()

    def assert_same_text(self, text):
        """Show that lexed markdown does not remove any content. """
        tokens = list(self.lexer.get_tokens_unprocessed(text))
        output = ''.join(t[2] for t in tokens)
        self.assertEqual(text, output)

    def test_code_fence(self):
        self.assert_same_text(r'```\nfoo\n```\n')

    def test_code_fence_gsm(self):
        self.assert_same_text(r'```markdown\nfoo\n```\n')

    def test_code_fence_gsm_with_no_lexer(self):
        self.assert_same_text(r'```invalid-lexer\nfoo\n```\n')
