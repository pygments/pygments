# -*- coding: utf-8 -*-
"""
    Test suite for the util module
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pytest import raises

from pygments import util, console


class FakeLexer:
    def analyse(text):
        return text
    analyse = util.make_analysator(analyse)


def test_getoptions():
    assert util.get_bool_opt({}, 'a', True) is True
    assert util.get_bool_opt({}, 'a', 1) is True
    assert util.get_bool_opt({}, 'a', 'true') is True
    assert util.get_bool_opt({}, 'a', 'no') is False
    assert raises(util.OptionError, util.get_bool_opt, {}, 'a', [])
    assert raises(util.OptionError, util.get_bool_opt, {}, 'a', 'foo')

    assert util.get_int_opt({}, 'a', 1) == 1
    assert raises(util.OptionError, util.get_int_opt, {}, 'a', [])
    assert raises(util.OptionError, util.get_int_opt, {}, 'a', 'bar')

    assert util.get_list_opt({}, 'a', [1]) == [1]
    assert util.get_list_opt({}, 'a', '1 2') == ['1', '2']
    assert raises(util.OptionError, util.get_list_opt, {}, 'a', 1)

    assert util.get_choice_opt({}, 'a', ['foo', 'bar'], 'bar') == 'bar'
    assert util.get_choice_opt({}, 'a', ['foo', 'bar'], 'Bar', True) == 'bar'
    assert raises(util.OptionError, util.get_choice_opt, {}, 'a',
                  ['foo', 'bar'], 'baz')


def test_docstring_headline():
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
    def f3():
        pass

    assert util.docstring_headline(f1) == 'docstring headline'
    assert util.docstring_headline(f2) == 'docstring headline'
    assert util.docstring_headline(f3) == ''


def test_analysator_returns_float():
    # If an analysator wrapped by make_analysator returns a floating point
    # number, then that number will be returned by the wrapper.
    assert FakeLexer.analyse('0.5') == 0.5


def test_analysator_returns_boolean():
    # If an analysator wrapped by make_analysator returns a boolean value,
    # then the wrapper will return 1.0 if the boolean was True or 0.0 if
    # it was False.
    assert FakeLexer.analyse(True) == 1.0
    assert FakeLexer.analyse(False) == 0.0


def test_analysator_raises_exception():
    # If an analysator wrapped by make_analysator raises an exception,
    # then the wrapper will return 0.0.
    class ErrorLexer:
        def analyse(text):
            raise RuntimeError('something bad happened')
        analyse = util.make_analysator(analyse)
    assert ErrorLexer.analyse('') == 0.0


def test_analysator_value_error():
    # When converting the analysator's return value to a float a
    # ValueError may occur.  If that happens 0.0 is returned instead.
    assert FakeLexer.analyse('bad input') == 0.0


def test_analysator_type_error():
    # When converting the analysator's return value to a float a
    # TypeError may occur.  If that happens 0.0 is returned instead.
    assert FakeLexer.analyse('xxx') == 0.0


def test_shebang_matches():
    assert util.shebang_matches('#!/usr/bin/env python\n', r'python(2\.\d)?')
    assert util.shebang_matches('#!/usr/bin/python2.4', r'python(2\.\d)?')
    assert util.shebang_matches('#!/usr/bin/startsomethingwith python',
                                r'python(2\.\d)?')
    assert util.shebang_matches('#!C:\\Python2.4\\Python.exe', r'python(2\.\d)?')

    assert not util.shebang_matches('#!/usr/bin/python-ruby', r'python(2\.\d)?')
    assert not util.shebang_matches('#!/usr/bin/python/ruby', r'python(2\.\d)?')
    assert not util.shebang_matches('#!', r'python')


def test_doctype_matches():
    assert util.doctype_matches('<!DOCTYPE html> <html>', 'html.*')
    assert not util.doctype_matches(
        '<?xml ?> <DOCTYPE html PUBLIC "a"> <html>', 'html.*')
    assert util.html_doctype_matches(
        '<?xml ?><!DOCTYPE html PUBLIC  "-//W3C//DTD XHTML 1.0 Strict//EN">')


def test_xml():
    assert util.looks_like_xml(
        '<?xml ?><!DOCTYPE html PUBLIC  "-//W3C//DTD XHTML 1.0 Strict//EN">')
    assert util.looks_like_xml('<html xmlns>abc</html>')
    assert not util.looks_like_xml('<html>')


def test_format_lines():
    lst = ['cat', 'dog']
    output = util.format_lines('var', lst)
    d = {}
    exec(output, d)
    assert isinstance(d['var'], tuple)
    assert ('cat', 'dog') == d['var']


def test_duplicates_removed_seq_types():
    # tuple
    x = util.duplicates_removed(('a', 'a', 'b'))
    assert ['a', 'b'] == x
    # list
    x = util.duplicates_removed(['a', 'a', 'b'])
    assert ['a', 'b'] == x
    # iterator
    x = util.duplicates_removed(iter(('a', 'a', 'b')))
    assert ['a', 'b'] == x


def test_duplicates_removed_nonconsecutive():
    # keeps first
    x = util.duplicates_removed(('a', 'b', 'a'))
    assert ['a', 'b'] == x


def test_guess_decode():
    # UTF-8 should be decoded as UTF-8
    s = util.guess_decode('\xff'.encode('utf-8'))
    assert s == ('\xff', 'utf-8')

    # otherwise, it could be latin1 or the locale encoding...
    import locale
    s = util.guess_decode(b'\xff')
    assert s[1] in ('latin1', locale.getpreferredencoding())


def test_guess_decode_from_terminal():
    class Term:
        encoding = 'utf-7'

    s = util.guess_decode_from_terminal('\xff'.encode('utf-7'), Term)
    assert s == ('\xff', 'utf-7')

    s = util.guess_decode_from_terminal('\xff'.encode('utf-8'), Term)
    assert s == ('\xff', 'utf-8')


def test_console_ansiformat():
    f = console.ansiformat
    c = console.codes
    all_attrs = f('+*_blue_*+', 'text')
    assert c['blue'] in all_attrs and c['blink'] in all_attrs
    assert c['bold'] in all_attrs and c['underline'] in all_attrs
    assert c['reset'] in all_attrs
    assert raises(KeyError, f, '*mauve*', 'text')


def test_console_functions():
    assert console.reset_color() == console.codes['reset']
    assert console.colorize('blue', 'text') == \
        console.codes['blue'] + 'text' + console.codes['reset']
