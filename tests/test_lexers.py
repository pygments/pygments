import unittest

import pygments.lexers

class LexerTest(unittest.TestCase):

    def testImportAll(self):
        # instantiate every lexer, to see if the token type defs are correct
        for x in pygments.lexers.LEXERS.keys():
            c = getattr(pygments.lexers, x)()
