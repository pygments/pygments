# -*- coding: utf-8 -*-
"""
    Test suite for the token module
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest
import StringIO
import sys

from pygments import token


class TokenTest(unittest.TestCase):

    def test_tokentype(self):
        e = self.assertEquals
        r = self.assertRaises

        t = token.String

        e(t.split(), [token.Token, token.Literal, token.String])

        e(t.__class__, token._TokenType)

    def test_functions(self):
        self.assert_(token.is_token_subtype(token.String, token.String))
        self.assert_(token.is_token_subtype(token.String, token.Literal))
        self.failIf(token.is_token_subtype(token.Literal, token.String))

        self.assert_(token.string_to_tokentype(token.String) is token.String)
        self.assert_(token.string_to_tokentype('') is token.Token)
        self.assert_(token.string_to_tokentype('String') is token.String)

    def test_sanity_check(self):
        stp = token.STANDARD_TYPES.copy()
        stp[token.Token] = '---' # Token and Text do conflict, that is okay
        t = {}
        for k, v in stp.iteritems():
            t.setdefault(v, []).append(k)
        if len(t) == len(stp):
            return # Okay

        for k, v in t.iteritems():
            if len(v) > 1:
                self.fail("%r has more than one key: %r" % (k, v))
