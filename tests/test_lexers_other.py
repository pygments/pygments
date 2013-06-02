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
from pygments.lexers.other import EasytrieveLexer, JclLexer, RexxLexer, \
    WebFocusLexer


def _exampleFilePath(filename):
    return os.path.join(os.path.dirname(__file__), 'examplefiles', filename)


class AnalyseTextTest(unittest.TestCase):
    def _testCanRecognizeAndGuessExampleFiles(self, lexer):
        assert lexer is not None

        for pattern in lexer.filenames:
            exampleFilesPattern = _exampleFilePath(pattern)
            for exampleFilePath in glob.glob(exampleFilesPattern):
                exampleFile = open(exampleFilePath, 'rb')
                try:
                    text = exampleFile.read()
                    probability = lexer.analyse_text(text)
                    self.assertTrue(probability > 0,
                        '%s must recognize %r' % (
                        lexer.name, exampleFilePath))
                    guessedLexer = guess_lexer(text)
                    self.assertEqual(guessedLexer.name, lexer.name)
                finally:
                    exampleFile.close()

    def testCanRecognizeAndGuessExampleFiles(self):
        LEXERS_TO_TEST = [
            EasytrieveLexer,
            JclLexer,
            RexxLexer,
        ]
        for lexerToTest in LEXERS_TO_TEST:
            self._testCanRecognizeAndGuessExampleFiles(lexerToTest)


class EasyTrieveLexerTest(unittest.TestCase):
    def testCanGuessFromText(self):
        self.assertLess(0, EasytrieveLexer.analyse_text('MACRO'))
        self.assertLess(0, EasytrieveLexer.analyse_text('\nMACRO'))
        self.assertLess(0, EasytrieveLexer.analyse_text(' \nMACRO'))
        self.assertLess(0, EasytrieveLexer.analyse_text(' \n MACRO'))
        self.assertLess(0, EasytrieveLexer.analyse_text('*\nMACRO'))
        self.assertLess(0, EasytrieveLexer.analyse_text(
            '*\n *\n\n \n*\n MACRO'))


class RexxLexerTest(unittest.TestCase):
    def testCanGuessFromText(self):
        self.assertAlmostEqual(0.01,
            RexxLexer.analyse_text('/* */'))
        self.assertAlmostEqual(1.0,
            RexxLexer.analyse_text('''/* Rexx */
                say "hello world"'''))
        self.assertLess(0.5,
            RexxLexer.analyse_text('/* */\n'
                'hello:pRoceduRe\n'
                '  say "hello world"'))
        self.assertLess(0.2,
            RexxLexer.analyse_text('''/* */
                if 1 > 0 then do
                    say "ok"
                end
                else do
                    say "huh?"
                end'''))
        self.assertLess(0.2,
            RexxLexer.analyse_text('''/* */
                greeting = "hello world!"
                parse value greeting "hello" name "!"
                say name'''))
