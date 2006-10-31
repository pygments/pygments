# -*- coding: utf-8 -*-
"""
    Pygments basic API tests
    ~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: 2006 by Georg Brandl.
    :license: GNU LGPL, see LICENSE for more details.
"""

import unittest
import StringIO
import random

from pygments import lexers, formatters
from pygments.token import _TokenType

test_content = [chr(i) for i in xrange(33, 128)] * 5
random.shuffle(test_content)
test_content = ''.join(test_content) + '\n'

class LexersTest(unittest.TestCase):

    def test_import_all(self):
        # instantiate every lexer, to see if the token type defs are correct
        for x in lexers.LEXERS.keys():
            c = getattr(lexers, x)()

    def test_lexer_classes(self):
        a = self.assert_
        ae = self.assertEquals
        # test that every lexer class has the correct public API
        for lexer in lexers._iter_lexerclasses():
            for attr in 'name', 'aliases', 'filenames', 'alias_filenames', 'mimetypes':
                a(hasattr(lexer, attr))
            result = lexer.analyse_text("abc")
            a(isinstance(result, float) and 0.0 <= result <= 1.0)

            inst = lexer(opt1="val1", opt2="val2")
            tokens = list(inst.get_tokens(test_content))
            txt = ""
            for token in tokens:
                a(isinstance(token, tuple))
                a(isinstance(token[0], _TokenType))
                if isinstance(token[1], str):
                    print repr(token[1])
                a(isinstance(token[1], unicode))
                txt += token[1]
            ae(txt, test_content, "%s lexer roundtrip failed: %r != %r" %
                    (lexer.name, test_content, txt))

    def test_get_lexers(self):
        a = self.assert_
        ae = self.assertEquals
        # test that the lexers functions work

        for func, args in [(lexers.get_lexer_by_name, ("python",)),
                           (lexers.get_lexer_for_filename, ("test.py",)),
                           (lexers.get_lexer_for_mimetype, ("text/x-python",)),
                           (lexers.guess_lexer, ("#!/usr/bin/python -O\nprint",)),
                           (lexers.guess_lexer_for_filename, ("a.py", "<%= @foo %>"))
                           ]:
            x = func(opt="val", *args)
            a(isinstance(x, lexers.PythonLexer))
            ae(x.options["opt"], "val")


class FormattersTest(unittest.TestCase):

    def test_public_api(self):
        a = self.assert_
        ae = self.assertEquals
        ts = list(lexers.PythonLexer().get_tokens("def f(): pass"))
        out = StringIO.StringIO()
        # test that every formatter class has the correct public API
        for formatter, info in formatters.FORMATTERS.iteritems():
            a(len(info) == 4)
            a(info[0]) # name
            a(info[1]) # aliases
            a(info[3]) # doc

            inst = formatter(opt1="val1")
            inst.get_style_defs()
            inst.format(ts, out)

    def test_get_formatters(self):
        a = self.assert_
        ae = self.assertEquals
        # test that the formatters functions work
        x = formatters.get_formatter_by_name("html", opt="val")
        a(isinstance(x, formatters.HtmlFormatter))
        ae(x.options["opt"], "val")

        x = formatters.get_formatter_for_filename("a.html", opt="val")
        a(isinstance(x, formatters.HtmlFormatter))
        ae(x.options["opt"], "val")
