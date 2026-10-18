"""
    Tests for scripts/optimize_lexers.py
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    The script merges runs of simple rules in the lexers shipped in the
    wheel; these tests check that it changes the rule tables but not the
    output of the lexers.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import importlib
import importlib.util
import types
from pathlib import Path

import pytest

import pygments
from pygments.lexer import merged
from pygments.token import Comment, Keyword, Name, Number, Whitespace

TESTDIR = Path(__file__).resolve().parent
SCRIPT = TESTDIR.parent / 'scripts' / 'optimize_lexers.py'

if Path(pygments.__file__).resolve().parent != TESTDIR.parent / 'pygments':
    # E.g. when testing the wheel, whose lexers are already optimized.
    pytest.skip('the script works on the lexer sources of this source tree',
                allow_module_level=True)


def _load_script():
    spec = importlib.util.spec_from_file_location('optimize_lexers', SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


optimize_lexers = _load_script()


def _load_lexer_module(source, name):
    module = types.ModuleType(name)
    module.__file__ = f'<{name}>'
    exec(compile(source, module.__file__, 'exec'), module.__dict__)
    return module


SAMPLE = r'''
from pygments.lexer import RegexLexer, ExtendedRegexLexer, bygroups, \
    include, words
from pygments.token import *


class SampleLexer(RegexLexer):
    name = 'Sample'
    aliases = ['sample']
    _ident = r'[a-z_]\w*'

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'#.*?$', Comment.Single),  # a comment
            (words(('if', 'else', 'while'), suffix=r'\b'), Keyword),
            (r'(\d+)(\.\d*)?', Number),  # inner groups
            (_ident, Name),
            (r'(def)(\s+)(' + _ident + ')',
             bygroups(Keyword, Whitespace, Name.Function)),
            (r'[+\-*/]', Operator),
            (r'"', String, 'string'),
            (r"(?P<q>')", String),      # named group: not foldable
            (r'(.)\1', Error),          # backreference: not foldable
            (r'(?i)true|false', Keyword.Constant),  # global flag: not foldable
            (r'[()]', Punctuation), (r'[,;]', Punctuation),
            include('misc'),
            (r'.', Error),
        ],
        'misc': [
            (r'@', Name.Decorator),
            (r'\$', Name.Variable),
        ],
        'string': [
            (r'[^"\\]+', String),
            (r'\\.', String.Escape),
            (r'"', String, '#pop'),
        ],
    }


class SampleExtendedLexer(ExtendedRegexLexer):
    name = 'Sample extended'
    aliases = ['sample-extended']

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'\d+', Number),
            (r'\w+', Name),
            (r'.', Error),
        ],
    }
'''

SAMPLE_TEXT = '''\
def foo(x):
    if x: y = 12.5 + x  # hi
    else: "a\\"b" @ $ TRUE aa '' ; ..
'''


@pytest.fixture(scope='module')
def sample():
    original = _load_lexer_module(SAMPLE, 'sample_original')
    optimized_source, merges = optimize_lexers.optimize_source(SAMPLE, original)
    optimized = _load_lexer_module(optimized_source, 'sample_optimized')
    return original, optimized, optimized_source, merges


def test_merges(sample):
    original, optimized, optimized_source, merges = sample
    assert sorted(merges) == [
        ('SampleExtendedLexer', 'root', 4),
        ('SampleLexer', 'misc', 2),
        ('SampleLexer', 'root', 2),
        ('SampleLexer', 'root', 5),
        ('SampleLexer', 'string', 2),
    ]
    assert optimize_lexers.IMPORT_LINE in optimized_source
    # Everything else is left alone.
    assert '# inner groups' not in optimized_source
    assert '# named group: not foldable' in optimized_source
    assert "bygroups(Keyword, Whitespace, Name.Function)" in optimized_source

    root = optimized.SampleLexer().tokens['root']
    assert root[0][0] == (r"(?P<g0>\s+)|(?P<g1>#.*?$)"
                          r"|(?P<g2>(else|if|while)\b)"
                          r"|(?P<g3>(\d+)(\.\d*)?)|(?P<g4>[a-z_]\w*)")
    assert isinstance(root[0][1], merged)
    assert root[0][1].tokens == (Whitespace, Comment.Single, Keyword,
                                 Number, Name)
    assert len(optimized.SampleLexer()._tokens['root']) == \
        len(original.SampleLexer()._tokens['root']) - 6


def test_output_unchanged(sample):
    original, optimized, _, _ = sample
    for cls in ('SampleLexer', 'SampleExtendedLexer'):
        expected = list(getattr(original, cls)().get_tokens(SAMPLE_TEXT))
        assert list(getattr(optimized, cls)().get_tokens(SAMPLE_TEXT)) == expected


def test_can_merge_pattern():
    can = optimize_lexers.can_merge_pattern
    assert can(r'\s+')
    assert can(r'(a)(b)|c')
    assert can(r'(?i:a)(?:b)(?=c)(?!d)(?<=e)')
    assert not can(r'(?P<name>a)')
    assert not can(r'(a)\1')
    assert not can(r'(a)?(?(1)b|c)')
    assert not can(r'(?i)abc')
    assert not can(r'abc(?s)')


def test_introspected_classes():
    skipped = optimize_lexers.introspected_classes()
    # PlPgsqlLexer looks for the keyword rule in PostgresLexer's rules, and
    # TeraTermLexer.analyse_text uses the regex of its first 'commands' rule.
    assert {'PostgresLexer', 'TeraTermLexer'} <= skipped
    # Whole states of these are reused by other lexers, which is fine.
    assert not {'PythonLexer', 'HaskellLexer'} & skipped
    _, optimized, merges = optimize_lexers.optimize_module(
        str(TESTDIR.parent / 'pygments' / 'lexers' / 'sql.py'))
    assert 'PostgresLexer' not in {name for name, _, _ in merges}
    assert merges


@pytest.mark.parametrize('pattern', [r'\s+', 'a"b', "a'b", 'a\'"b', 'a\\\\',
                                     'a\\', 'tab\t', 'nl\n', 'ünïcode'])
def test_regex_literal(pattern):
    literal = optimize_lexers.regex_literal(pattern)
    assert eval(literal) == pattern


@pytest.mark.parametrize('path', optimize_lexers.lexer_module_paths(),
                         ids=lambda path: Path(path).name)
def test_optimized_module_compiles(path):
    _, optimized, merges = optimize_lexers.optimize_module(path)
    compile(optimized, path, 'exec')
    assert all(n >= 2 for _, _, n in merges)


@pytest.mark.parametrize('module_name, lexer_name, alias', [
    ('python', 'PythonLexer', 'python'),
    ('c_cpp', 'CLexer', 'c'),
    ('javascript', 'JavascriptLexer', 'js'),
    ('ruby', 'RubyLexer', 'rb'),
    ('rust', 'RustLexer', 'rust'),
    ('php', 'PhpLexer', 'php'),
])
def test_optimized_lexer_output_unchanged(module_name, lexer_name, alias):
    path = str(TESTDIR.parent / 'pygments' / 'lexers' / f'{module_name}.py')
    _, optimized_source, merges = optimize_lexers.optimize_module(path)
    assert merges
    original = getattr(importlib.import_module(f'pygments.lexers.{module_name}'),
                       lexer_name)()
    optimized = getattr(_load_lexer_module(optimized_source,
                                           f'{module_name}_optimized'),
                        lexer_name)()
    assert sum(len(rules) for rules in optimized._tokens.values()) < \
        sum(len(rules) for rules in original._tokens.values())
    samples = [p for p in (TESTDIR / 'examplefiles' / alias).iterdir()
               if p.is_file() and p.suffix != '.output']
    assert samples
    for sample in samples:
        text = sample.read_text(encoding='utf-8')
        assert list(optimized.get_tokens(text)) == list(original.get_tokens(text))
