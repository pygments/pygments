# -*- coding: utf-8 -*-
"""
    Test suite for the util module
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2012 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import unittest
import os

from pygments import util


class FakeLexer(object):
    def analyse(text):
        return float(text)
    analyse = util.make_analysator(analyse)


class UtilTest(unittest.TestCase):

    def test_getoptions(self):
        raises = self.assertRaises
        equals = self.assertEquals

        equals(util.get_bool_opt({}, 'a', True), True)
        equals(util.get_bool_opt({}, 'a', 1), True)
        equals(util.get_bool_opt({}, 'a', 'true'), True)
        equals(util.get_bool_opt({}, 'a', 'no'), False)
        raises(util.OptionError, util.get_bool_opt, {}, 'a', [])
        raises(util.OptionError, util.get_bool_opt, {}, 'a', 'foo')

        equals(util.get_int_opt({}, 'a', 1), 1)
        raises(util.OptionError, util.get_int_opt, {}, 'a', [])
        raises(util.OptionError, util.get_int_opt, {}, 'a', 'bar')

        equals(util.get_list_opt({}, 'a', [1]), [1])
        equals(util.get_list_opt({}, 'a', '1 2'), ['1', '2'])
        raises(util.OptionError, util.get_list_opt, {}, 'a', 1)


    def test_docstring_headline(self):
        def f1():
            """
            docstring headline

            other text
            """
        def f2():
            """
            docstring
            headline

            other text
            """

        self.assertEquals(util.docstring_headline(f1), "docstring headline")
        self.assertEquals(util.docstring_headline(f2), "docstring headline")

    def test_analysator_returns_float(self):
        # If an analysator wrapped by make_analysator returns a floating point
        # number, then that number will be returned by the wrapper.
        self.assertEquals(FakeLexer.analyse('0.5'), 0.5)

    def test_analysator_returns_boolean(self):
        # If an analysator wrapped by make_analysator returns a boolean value,
        # then the wrapper will return 1.0 if the boolean was True or 0.0 if
        # it was False.
        self.assertEquals(FakeLexer.analyse(True), 1.0)
        self.assertEquals(FakeLexer.analyse(False), 0.0)

    def test_analysator_raises_exception(self):
        # If an analysator wrapped by make_analysator raises an exception,
        # then the wrapper will return 0.0.
        class ErrorLexer(object):
            def analyse(text):
                raise RuntimeError('something bad happened')
            analyse = util.make_analysator(analyse)
        self.assertEquals(ErrorLexer.analyse(''), 0.0)

    def test_analysator_value_error(self):
        # When converting the analysator's return value to a float a
        # ValueError may occur.  If that happens 0.0 is returned instead.
        self.assertEquals(FakeLexer.analyse('bad input'), 0.0)

    def test_analysator_type_error(self):
        # When converting the analysator's return value to a float a
        # TypeError may occur.  If that happens 0.0 is returned instead.
        self.assertEquals(FakeLexer.analyse(None), 0.0)

    def test_shebang_matches(self):
        self.assert_(util.shebang_matches('#!/usr/bin/env python', r'python(2\.\d)?'))
        self.assert_(util.shebang_matches('#!/usr/bin/python2.4', r'python(2\.\d)?'))
        self.assert_(util.shebang_matches('#!/usr/bin/startsomethingwith python',
                                          r'python(2\.\d)?'))
        self.assert_(util.shebang_matches('#!C:\\Python2.4\\Python.exe',
                                          r'python(2\.\d)?'))

        self.failIf(util.shebang_matches('#!/usr/bin/python-ruby', r'python(2\.\d)?'))
        self.failIf(util.shebang_matches('#!/usr/bin/python/ruby', r'python(2\.\d)?'))
        self.failIf(util.shebang_matches('#!', r'python'))

    def test_doctype_matches(self):
        self.assert_(util.doctype_matches('<!DOCTYPE html PUBLIC "a"> <html>',
                                          'html.*'))
        self.failIf(util.doctype_matches('<?xml ?> <DOCTYPE html PUBLIC "a"> <html>',
                                         'html.*'))
        self.assert_(util.html_doctype_matches(
            '<?xml ?><!DOCTYPE html PUBLIC  "-//W3C//DTD XHTML 1.0 Strict//EN">'))

    def test_xml(self):
        self.assert_(util.looks_like_xml(
            '<?xml ?><!DOCTYPE html PUBLIC  "-//W3C//DTD XHTML 1.0 Strict//EN">'))
        self.assert_(util.looks_like_xml('<html xmlns>abc</html>'))
        self.failIf(util.looks_like_xml('<html>'))
