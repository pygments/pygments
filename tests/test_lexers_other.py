# -*- coding: utf-8 -*-
"""
    Tests for other lexers
    ~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2013 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""
import glob
import os
import unittest

from pygments.lexers import guess_lexer
from pygments.lexers.other import EasytrieveLexer, JclLexer, WebFocusLexer


def _exampleFilePath(filename):
    return os.path.join(os.path.dirname(__file__), 'examplefiles', filename)


class _AnalyseTextTest(unittest.TestCase):
    def setUp(self):
        raise NotImplementedError('self.lexer must be set')

    def testCanRecognizeAndGuessExampleFiles(self):
        for pattern in self.lexer.filenames:
            exampleFilesPattern = _exampleFilePath(pattern)
            for exampleFilePath in glob.glob(exampleFilesPattern):
                exampleFile = open(exampleFilePath, 'rb')
                try:
                    text = exampleFile.read()
                    probability = self.lexer.analyse_text(text)
                    self.assertTrue(probability > 0,
                            '%s must recognize %r' % (self.lexer.name, exampleFilePath))
                    guessedLexer = guess_lexer(text)
                    self.assertEqual(guessedLexer.name, self.lexer.name)
                finally:
                    exampleFile.close()


class EasytrieveLexerTest(_AnalyseTextTest):
    def setUp(self):
        self.lexer = EasytrieveLexer()


class JclLexerTest(_AnalyseTextTest):
    def setUp(self):
        self.lexer = JclLexer()


class WebFocusLexerTest(_AnalyseTextTest):
    def setUp(self):
        self.lexer = WebFocusLexer()
