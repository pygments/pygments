# -*- coding: utf-8 -*-
"""
    BibTeX Test
    ~~~~~~~~~~~

    :copyright: Copyright 2006-2019 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import textwrap
import unittest

from pygments.lexers import BibTeXLexer, BSTLexer
from pygments.token import Token


class BibTeXTest(unittest.TestCase):
    def setUp(self):
        self.lexer = BibTeXLexer()

    def testPreamble(self):
        data = u'@PREAMBLE{"% some LaTeX code here"}'
        tokens = [
            (Token.Name.Class, u'@PREAMBLE'),
            (Token.Punctuation, u'{'),
            (Token.String, u'"'),
            (Token.String, u'% some LaTeX code here'),
            (Token.String, u'"'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(list(self.lexer.get_tokens(data)), tokens)

    def testString(self):
        data = u'@STRING(SCI = "Science")'
        tokens = [
            (Token.Name.Class, u'@STRING'),
            (Token.Punctuation, u'('),
            (Token.Name.Attribute, u'SCI'),
            (Token.Text, u' '),
            (Token.Punctuation, u'='),
            (Token.Text, u' '),
            (Token.String, u'"'),
            (Token.String, u'Science'),
            (Token.String, u'"'),
            (Token.Punctuation, u')'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(list(self.lexer.get_tokens(data)), tokens)

    def testEntry(self):
        data = u"""
            This is a comment.

            @ARTICLE{ruckenstein-diffusion,
                author = "Liu, Hongquin" # and # "Ruckenstein, Eli",
                year = 1997,
                month = JAN,
                pages = "888-895"
            }
        """

        tokens = [
            (Token.Comment, u'This is a comment.'),
            (Token.Text, u'\n\n'),
            (Token.Name.Class, u'@ARTICLE'),
            (Token.Punctuation, u'{'),
            (Token.Name.Label, u'ruckenstein-diffusion'),
            (Token.Punctuation, u','),
            (Token.Text, u'\n    '),
            (Token.Name.Attribute, u'author'),
            (Token.Text, u' '),
            (Token.Punctuation, u'='),
            (Token.Text, u' '),
            (Token.String, u'"'),
            (Token.String, u'Liu, Hongquin'),
            (Token.String, u'"'),
            (Token.Text, u' '),
            (Token.Punctuation, u'#'),
            (Token.Text, u' '),
            (Token.Name.Variable, u'and'),
            (Token.Text, u' '),
            (Token.Punctuation, u'#'),
            (Token.Text, u' '),
            (Token.String, u'"'),
            (Token.String, u'Ruckenstein, Eli'),
            (Token.String, u'"'),
            (Token.Punctuation, u','),
            (Token.Text, u'\n    '),
            (Token.Name.Attribute, u'year'),
            (Token.Text, u' '),
            (Token.Punctuation, u'='),
            (Token.Text, u' '),
            (Token.Number, u'1997'),
            (Token.Punctuation, u','),
            (Token.Text, u'\n    '),
            (Token.Name.Attribute, u'month'),
            (Token.Text, u' '),
            (Token.Punctuation, u'='),
            (Token.Text, u' '),
            (Token.Name.Variable, u'JAN'),
            (Token.Punctuation, u','),
            (Token.Text, u'\n    '),
            (Token.Name.Attribute, u'pages'),
            (Token.Text, u' '),
            (Token.Punctuation, u'='),
            (Token.Text, u' '),
            (Token.String, u'"'),
            (Token.String, u'888-895'),
            (Token.String, u'"'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(list(self.lexer.get_tokens(textwrap.dedent(data))), tokens)

    def testComment(self):
        data = '@COMMENT{test}'
        tokens = [
            (Token.Comment, u'@COMMENT'),
            (Token.Comment, u'{test}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(list(self.lexer.get_tokens(data)), tokens)

    def testMissingBody(self):
        data = '@ARTICLE xxx'
        tokens = [
            (Token.Name.Class, u'@ARTICLE'),
            (Token.Text, u' '),
            (Token.Error, u'x'),
            (Token.Error, u'x'),
            (Token.Error, u'x'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(list(self.lexer.get_tokens(data)), tokens)

    def testMismatchedBrace(self):
        data = '@PREAMBLE(""}'
        tokens = [
            (Token.Name.Class, u'@PREAMBLE'),
            (Token.Punctuation, u'('),
            (Token.String, u'"'),
            (Token.String, u'"'),
            (Token.Error, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(list(self.lexer.get_tokens(data)), tokens)


class BSTTest(unittest.TestCase):
    def setUp(self):
        self.lexer = BSTLexer()

    def testBasicBST(self):
        data = """
            % BibTeX standard bibliography style `plain'

            INTEGERS { output.state before.all }

            FUNCTION {sort.format.title}
            { 't :=
            "A " #2
                "An " #3
                "The " #4 t chop.word
                chop.word
            chop.word
            sortify
            #1 global.max$ substring$
            }

            ITERATE {call.type$}
        """
        tokens = [
            (Token.Comment.SingleLine, "% BibTeX standard bibliography style `plain'"),
            (Token.Text, u'\n\n'),
            (Token.Keyword, u'INTEGERS'),
            (Token.Text, u' '),
            (Token.Punctuation, u'{'),
            (Token.Text, u' '),
            (Token.Name.Variable, u'output.state'),
            (Token.Text, u' '),
            (Token.Name.Variable, u'before.all'),
            (Token.Text, u' '),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n\n'),
            (Token.Keyword, u'FUNCTION'),
            (Token.Text, u' '),
            (Token.Punctuation, u'{'),
            (Token.Name.Variable, u'sort.format.title'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'{'),
            (Token.Text, u' '),
            (Token.Name.Function, u"'t"),
            (Token.Text, u' '),
            (Token.Name.Variable, u':='),
            (Token.Text, u'\n'),
            (Token.Literal.String, u'"A "'),
            (Token.Text, u' '),
            (Token.Literal.Number, u'#2'),
            (Token.Text, u'\n    '),
            (Token.Literal.String, u'"An "'),
            (Token.Text, u' '),
            (Token.Literal.Number, u'#3'),
            (Token.Text, u'\n    '),
            (Token.Literal.String, u'"The "'),
            (Token.Text, u' '),
            (Token.Literal.Number, u'#4'),
            (Token.Text, u' '),
            (Token.Name.Variable, u't'),
            (Token.Text, u' '),
            (Token.Name.Variable, u'chop.word'),
            (Token.Text, u'\n    '),
            (Token.Name.Variable, u'chop.word'),
            (Token.Text, u'\n'),
            (Token.Name.Variable, u'chop.word'),
            (Token.Text, u'\n'),
            (Token.Name.Variable, u'sortify'),
            (Token.Text, u'\n'),
            (Token.Literal.Number, u'#1'),
            (Token.Text, u' '),
            (Token.Name.Builtin, u'global.max$'),
            (Token.Text, u' '),
            (Token.Name.Builtin, u'substring$'),
            (Token.Text, u'\n'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n\n'),
            (Token.Keyword, u'ITERATE'),
            (Token.Text, u' '),
            (Token.Punctuation, u'{'),
            (Token.Name.Builtin, u'call.type$'),
            (Token.Punctuation, u'}'),
            (Token.Text, u'\n'),
        ]
        self.assertEqual(list(self.lexer.get_tokens(textwrap.dedent(data))), tokens)
