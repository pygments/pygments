"""
    Pygments merge-simple-rules optimisation tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Tests for the RegexLexerMeta optimisation that merges runs of consecutive
    plain-token rules in a state into a single combined regex.  The optimisation
    must be completely output-preserving.

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments.lexer import RegexLexer, bygroups
from pygments.lexers import get_lexer_by_name
from pygments.token import (Text, Whitespace, Number, Name, Operator,
                            Punctuation, Keyword, Other)


class SampleLexer(RegexLexer):
    """A small lexer with simple runs broken up by non-simple rules."""
    name = 'Sample'
    tokens = {
        'root': [
            # run of three simple rules ...
            (r'\s+', Whitespace),
            (r'\d+', Number.Integer),
            (r'(in|is|or)\b', Operator.Word),     # capturing group, plain token
            # ... a barrier: changes state ...
            (r'\(', Punctuation, 'paren'),
            # ... a barrier: callback action ...
            (r'(a)(=)', bygroups(Name, Operator)),
            # ... then another simple run.
            (r'[a-z]+', Name),
            (r'[+\-*/]', Operator),
        ],
        'paren': [
            (r'\)', Punctuation, '#pop'),
            (r'.', Other),
        ],
    }


class SampleLexerNoMerge(SampleLexer):
    merge_simple_rules = False


def _count(lexer):
    return {state: len(rules) for state, rules in lexer._tokens.items()}


def test_runs_are_merged():
    merged = SampleLexer()
    plain = SampleLexerNoMerge()
    # 'root' had 7 rules: a run of 3 simple rules, two adjacent barriers, then
    # a run of 2 simple rules -> combined + barrier + barrier + combined = 4.
    assert _count(plain)['root'] == 7
    assert _count(merged)['root'] == 4
    # the merged entries use a generated callback
    names = [getattr(a, '__name__', '') for _, a, _ in merged._tokens['root']]
    assert names.count('grouped') == 2


def test_output_preserved_small():
    merged = SampleLexer()
    plain = SampleLexerNoMerge()
    for text in ['1 + foo', 'in is or', '(a=b) 12', 'x*y/z\n', '  a=c  ',
                 'foo(bar) 3 in x']:
        assert list(merged.get_tokens(text)) == list(plain.get_tokens(text))


def test_disable_toggle():
    assert SampleLexer.merge_simple_rules is True
    # with merging off, no rule is collapsed
    plain = SampleLexerNoMerge()
    names = [getattr(a, '__name__', '') for _, a, _ in plain._tokens['root']]
    assert 'grouped' not in names


# A spread of bundled lexers that use RegexLexer / ExtendedRegexLexer.
BUNDLED = ['python', 'c', 'javascript', 'bash', 'html', 'css', 'yaml', 'rust',
           'go', 'ruby', 'typescript', 'sql', 'java', 'perl', 'php', 'lua',
           'ini', 'json']

SAMPLE_CODE = {
    'python': "def f(x):\n    s = f'{x!r:>{w}}'  # c\n    return [i for i in (1,2)]\n",
    'c': '#include <stdio.h>\nint main(void){ printf("%d\\n", 0xFF); return 0; }\n',
    'javascript': "const x = (a,b)=>a+b; let s=`t ${x}`; // c\nclass A {}\n",
    'html': '<!DOCTYPE html>\n<div class="x" id=y>hi &amp; bye</div>\n',
    'yaml': "key: value\nlist:\n  - a\n  - 12\n# comment\nnested: {a: 1}\n",
}
GENERIC = "the quick brown fox 123 == != {key: 'val'} /* x */ end: foo @bar\n"


@pytest.mark.parametrize('name', BUNDLED)
def test_bundled_output_preserved(name):
    lexer_cls = type(get_lexer_by_name(name))
    if not issubclass(lexer_cls, RegexLexer):
        pytest.skip('not a RegexLexer')
    merged = lexer_cls()
    if not hasattr(merged, '_tokens'):
        pytest.skip('lexer has no processed token table')
    plain = type(lexer_cls.__name__ + 'NoMerge', (lexer_cls,),
                 {'merge_simple_rules': False})()
    code = SAMPLE_CODE.get(name, GENERIC)
    assert list(merged.get_tokens(code)) == list(plain.get_tokens(code))
    # ensure the optimisation actually engaged somewhere
    merged_total = sum(len(v) for v in merged._tokens.values())
    plain_total = sum(len(v) for v in plain._tokens.values())
    assert merged_total <= plain_total


def test_inner_groups_and_classes_preserved():
    # Rules whose patterns contain capturing groups or '(' inside a character
    # class must still dispatch to the right token (regression: the punctuation
    # rule '[]{}:(),;[]' once got corrupted by group rewriting).
    py = get_lexer_by_name('python')
    code = "(a, b) = [1, 2]; x or y\n"
    plain = type('PyNoMerge', (type(py),), {'merge_simple_rules': False})()
    assert list(py.get_tokens(code)) == list(plain.get_tokens(code))
