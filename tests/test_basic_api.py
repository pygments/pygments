# -*- coding: utf-8 -*-
"""
    Pygments basic API tests
    ~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import random
from io import StringIO, BytesIO
from os import path

import pytest

from pygments import lexers, formatters, lex, format
from pygments.token import _TokenType, Text
from pygments.lexer import RegexLexer
from pygments.formatters.img import FontNotFound
from pygments.util import ClassNotFound

TESTDIR = path.dirname(path.abspath(__file__))
TESTFILE = path.join(TESTDIR, 'test_basic_api.py')

test_content = [chr(i) for i in range(33, 128)] * 5
random.shuffle(test_content)
test_content = ''.join(test_content) + '\n'


@pytest.mark.parametrize('name', lexers.LEXERS)
def test_lexer_instantiate_all(name):
    # instantiate every lexer, to see if the token type defs are correct
    getattr(lexers, name)


@pytest.mark.parametrize('cls', lexers._iter_lexerclasses(plugins=False))
def test_lexer_classes(cls):
    # test that every lexer class has the correct public API
    assert type(cls.name) is str
    for attr in 'aliases', 'filenames', 'alias_filenames', 'mimetypes':
        assert hasattr(cls, attr)
        assert type(getattr(cls, attr)) is list, \
            "%s: %s attribute wrong" % (cls, attr)
    result = cls.analyse_text("abc")
    assert isinstance(result, float) and 0.0 <= result <= 1.0
    result = cls.analyse_text(".abc")
    assert isinstance(result, float) and 0.0 <= result <= 1.0

    assert all(al.lower() == al for al in cls.aliases)

    if issubclass(cls, RegexLexer):
        inst = cls(opt1="val1", opt2="val2")
        if not hasattr(cls, '_tokens'):
            # if there's no "_tokens", the lexer has to be one with
            # multiple tokendef variants
            assert cls.token_variants
            for variant in cls.tokens:
                assert 'root' in cls.tokens[variant]
        else:
            assert 'root' in cls._tokens, \
                   '%s has no root state' % cls


@pytest.mark.parametrize('cls', lexers._iter_lexerclasses(plugins=False))
def test_random_input(cls):
    inst = cls()
    try:
        tokens = list(inst.get_tokens(test_content))
    except KeyboardInterrupt:
        raise KeyboardInterrupt(
            'interrupted %s.get_tokens(): test_content=%r' %
            (cls.__name__, test_content))
    txt = ""
    for token in tokens:
        assert isinstance(token, tuple)
        assert isinstance(token[0], _TokenType)
        assert isinstance(token[1], str)
        txt += token[1]
    assert txt == test_content, "%s lexer roundtrip failed: %r != %r" % \
        (cls.name, test_content, txt)


@pytest.mark.parametrize('cls', lexers._iter_lexerclasses(plugins=False))
def test_lexer_options(cls):
    if cls.__name__ == 'RawTokenLexer':
        # this one is special
        return

    # test that the basic options work
    def ensure(tokens, output):
        concatenated = ''.join(token[1] for token in tokens)
        assert concatenated == output, \
            '%s: %r != %r' % (cls, concatenated, output)

    inst = cls(stripnl=False)
    ensure(inst.get_tokens('a\nb'), 'a\nb\n')
    ensure(inst.get_tokens('\n\n\n'), '\n\n\n')
    inst = cls(stripall=True)
    ensure(inst.get_tokens('   \n  b\n\n\n'), 'b\n')
    # some lexers require full lines in input
    if ('ConsoleLexer' not in cls.__name__ and
        'SessionLexer' not in cls.__name__ and
        not cls.__name__.startswith('Literate') and
        cls.__name__ not in ('ErlangShellLexer', 'RobotFrameworkLexer')):
        inst = cls(ensurenl=False)
        ensure(inst.get_tokens('a\nb'), 'a\nb')
        inst = cls(ensurenl=False, stripall=True)
        ensure(inst.get_tokens('a\nb\n\n'), 'a\nb')


def test_get_lexers():
    # test that the lexers functions work
    for func, args in [(lexers.get_lexer_by_name, ("python",)),
                       (lexers.get_lexer_for_filename, ("test.py",)),
                       (lexers.get_lexer_for_mimetype, ("text/x-python",)),
                       (lexers.guess_lexer, ("#!/usr/bin/python3 -O\nprint",)),
                       (lexers.guess_lexer_for_filename, ("a.py", "<%= @foo %>"))
                       ]:
        x = func(opt='val', *args)
        assert isinstance(x, lexers.PythonLexer)
        assert x.options["opt"] == "val"

    for cls, (_, lname, aliases, _, mimetypes) in lexers.LEXERS.items():
        assert cls == lexers.find_lexer_class(lname).__name__

        for alias in aliases:
            assert cls == lexers.get_lexer_by_name(alias).__class__.__name__

        for mimetype in mimetypes:
            assert cls == lexers.get_lexer_for_mimetype(mimetype).__class__.__name__

    try:
        lexers.get_lexer_by_name(None)
    except ClassNotFound:
        pass
    else:
        raise Exception


@pytest.mark.parametrize('cls', [getattr(formatters, name)
                                 for name in formatters.FORMATTERS])
def test_formatter_public_api(cls):
    # test that every formatter class has the correct public API
    ts = list(lexers.PythonLexer().get_tokens("def f(): pass"))
    string_out = StringIO()
    bytes_out = BytesIO()

    info = formatters.FORMATTERS[cls.__name__]
    assert len(info) == 5
    assert info[1], "missing formatter name"
    assert info[2], "missing formatter aliases"
    assert info[4], "missing formatter docstring"

    try:
        inst = cls(opt1="val1")
    except (ImportError, FontNotFound) as e:
        pytest.skip(str(e))

    try:
        inst.get_style_defs()
    except NotImplementedError:
        # may be raised by formatters for which it doesn't make sense
        pass

    if cls.unicodeoutput:
        inst.format(ts, string_out)
    else:
        inst.format(ts, bytes_out)


def test_formatter_encodings():
    from pygments.formatters import HtmlFormatter

    # unicode output
    fmt = HtmlFormatter()
    tokens = [(Text, "ä")]
    out = format(tokens, fmt)
    assert type(out) is str
    assert "ä" in out

    # encoding option
    fmt = HtmlFormatter(encoding="latin1")
    tokens = [(Text, "ä")]
    assert "ä".encode("latin1") in format(tokens, fmt)

    # encoding and outencoding option
    fmt = HtmlFormatter(encoding="latin1", outencoding="utf8")
    tokens = [(Text, "ä")]
    assert "ä".encode("utf8") in format(tokens, fmt)


@pytest.mark.parametrize('cls', [getattr(formatters, name)
                                 for name in formatters.FORMATTERS])
def test_formatter_unicode_handling(cls):
    # test that the formatter supports encoding and Unicode
    tokens = list(lexers.PythonLexer(encoding='utf-8').
                  get_tokens("def f(): 'ä'"))

    try:
        inst = cls(encoding=None)
    except (ImportError, FontNotFound) as e:
        # some dependency or font not installed
        pytest.skip(str(e))

    if cls.name != 'Raw tokens':
        out = format(tokens, inst)
        if cls.unicodeoutput:
            assert type(out) is str, '%s: %r' % (cls, out)

        inst = cls(encoding='utf-8')
        out = format(tokens, inst)
        assert type(out) is bytes, '%s: %r' % (cls, out)
        # Cannot test for encoding, since formatters may have to escape
        # non-ASCII characters.
    else:
        inst = cls()
        out = format(tokens, inst)
        assert type(out) is bytes, '%s: %r' % (cls, out)


def test_get_formatters():
    # test that the formatters functions work
    x = formatters.get_formatter_by_name("html", opt="val")
    assert isinstance(x, formatters.HtmlFormatter)
    assert x.options["opt"] == "val"

    x = formatters.get_formatter_for_filename("a.html", opt="val")
    assert isinstance(x, formatters.HtmlFormatter)
    assert x.options["opt"] == "val"


def test_styles():
    # minimal style test
    from pygments.formatters import HtmlFormatter
    HtmlFormatter(style="pastie")


def test_bare_class_handler():
    from pygments.formatters import HtmlFormatter
    from pygments.lexers import PythonLexer
    try:
        lex('test\n', PythonLexer)
    except TypeError as e:
        assert 'lex() argument must be a lexer instance' in str(e)
    else:
        assert False, 'nothing raised'
    try:
        format([], HtmlFormatter)
    except TypeError as e:
        assert 'format() argument must be a formatter instance' in str(e)
    else:
        assert False, 'nothing raised'


class TestFilters:

    def test_basic(self):
        filters_args = [
            ('whitespace', {'spaces': True, 'tabs': True, 'newlines': True}),
            ('whitespace', {'wstokentype': False, 'spaces': True}),
            ('highlight', {'names': ['isinstance', 'lexers', 'x']}),
            ('codetagify', {'codetags': 'API'}),
            ('keywordcase', {'case': 'capitalize'}),
            ('raiseonerror', {}),
            ('gobble', {'n': 4}),
            ('tokenmerge', {}),
            ('symbols', {'lang': 'isabelle'}),
        ]
        for x, args in filters_args:
            lx = lexers.PythonLexer()
            lx.add_filter(x, **args)
            # We don't read as binary and decode, but instead read as text, as
            # we need consistent line endings. Otherwise we'll get \r\n on
            # Windows
            with open(TESTFILE, 'r', encoding='utf-8') as fp:
                text = fp.read()
            tokens = list(lx.get_tokens(text))
            assert all(isinstance(t[1], str) for t in tokens), \
                '%s filter did not return Unicode' % x
            roundtext = ''.join([t[1] for t in tokens])
            if x not in ('whitespace', 'keywordcase', 'gobble'):
                # these filters change the text
                assert roundtext == text, \
                    "lexer roundtrip with %s filter failed" % x

    def test_raiseonerror(self):
        lx = lexers.PythonLexer()
        lx.add_filter('raiseonerror', excclass=RuntimeError)
        assert pytest.raises(RuntimeError, list, lx.get_tokens('$'))

    def test_whitespace(self):
        lx = lexers.PythonLexer()
        lx.add_filter('whitespace', spaces='%')
        with open(TESTFILE, 'rb') as fp:
            text = fp.read().decode('utf-8')
        lxtext = ''.join([t[1] for t in list(lx.get_tokens(text))])
        assert ' ' not in lxtext

    def test_keywordcase(self):
        lx = lexers.PythonLexer()
        lx.add_filter('keywordcase', case='capitalize')
        with open(TESTFILE, 'rb') as fp:
            text = fp.read().decode('utf-8')
        lxtext = ''.join([t[1] for t in list(lx.get_tokens(text))])
        assert 'Def' in lxtext and 'Class' in lxtext

    def test_codetag(self):
        lx = lexers.PythonLexer()
        lx.add_filter('codetagify')
        text = '# BUG: text'
        tokens = list(lx.get_tokens(text))
        assert '# ' == tokens[0][1]
        assert 'BUG' == tokens[1][1]

    def test_codetag_boundary(self):
        # ticket #368
        lx = lexers.PythonLexer()
        lx.add_filter('codetagify')
        text = '# DEBUG: text'
        tokens = list(lx.get_tokens(text))
        assert '# DEBUG: text' == tokens[0][1]

    def test_symbols(self):
        lx = lexers.IsabelleLexer()
        lx.add_filter('symbols')
        text = 'lemma "A \\<Longrightarrow> B"'
        tokens = list(lx.get_tokens(text))
        assert 'lemma' == tokens[0][1]
        assert 'A ' == tokens[3][1]
        assert '\U000027f9' == tokens[4][1]
